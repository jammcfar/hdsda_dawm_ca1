### Title:  CA2 part 3
### Author: x19175329@student.ncirl.ie
### Desc:   R program to analyse the bank dataset

set.seed(11111987)

library(tidyverse)
library(tidymodels)
library(here)
library(naniar)
## library(naivebayes)
## library(recipeselectors)
# library(themis)
library(corrr)
library(C50)
library(vip)
library(ranger)

## add a user choice here, incase he doesnt want to DL off github or dont if it goes in appendix
"https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip"


## function to tidy up some output
tidyPreds <- function(data, target, model_fit, model_name, wrkflw_name) {
  temp_probs <- predict(model_fit, data, type = "prob") %>% as_tibble()
  temp_pred <- predict(model_fit, data)

  temp_tib <-
    data %>%
    select(target) %>%
    bind_cols(temp_pred) %>%
    bind_cols(temp_probs) %>%
    mutate(
      model = model_name,
      wrkflw = wrkflw_name
    )

  return(temp_tib)
}

df_bank_in <-
  read_delim(here("bank-additional-full.csv"),
    delim = ";"
  ) %>%
  sample_n(10000)

## set colname to snakecase, as per tidy style guide
colnames(df_bank_in) <- snakecase::to_snake_case(colnames(df_bank_in))

# some unusual na strings present in data
custom_na_strings <- c(common_na_strings, "nonexistent", "unknown")

## remove this colun, as per dataset documentation
## also remove pdays and poutcome, these are problematic
## and their correct encoding relies on SME knowledge
df_bank_proc <-
  df_bank_in %>%
  select(
    -duration,
    -pdays,
    -poutcome
  ) %>%
  replace_with_na_all(
    condition = ~ .x %in% custom_na_strings
  ) %>%
  mutate_if(is.character, as.factor) %>%
  distinct() # expensive, removes 21 rows

# Checking for intercorrelations (stick in appendix)
glimpse(df_bank_proc) %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  pivot_longer(age:nr_employed) %>%
  arrange(desc(value))

## lets try tidymodels. etc
bank_split <- initial_split(df_bank_proc, prop = 3 / 4)


# Create data frames for the two sets:
df_train <- training(bank_split)
df_test <- testing(bank_split)


df_facts <- df_train %>% select(
  where(is.factor),
  -y
)
fact_names <- paste(colnames(df_facts), sep = "|")

# Recipe to ready data for modelling
bank_rec <-
  recipe(y ~ ., data = df_train) %>%
  update_role(y, new_role = "outcome") %>%
  step_zv(all_predictors()) %>%
  step_knnimpute(all_predictors())

## tree based models can handle these bninaries as numerics
## also, since tree models are a series of if-else, dont have to dummy
## https://www.slideshare.net/Work-Bench/i-dont-want-to-be-a-dummy-encoding-predictors-for-trees

## Pre-process the data for simple train-test runs
bank_prep <- prep(bank_rec, training = df_train)
df_baked_train <- bake(bank_prep, df_train)
bank_test_prep <- prep(bank_rec, training = df_train)
df_baked_test <- bake(bank_prep, df_test)

# single C5.0===================================================================
single_c5 <-
  C5.0(y ~ .,
    data = df_baked_train
  )


c5_single_train_tib <-
  tidyPreds(
    df_baked_train,
    "y",
    single_c5,
    "C5.0",
    "Hold-back"
  )


c5_single_tib <-
  tidyPreds(
    df_baked_test,
    "y",
    single_c5,
    "C5.0",
    "Hold-back"
  )


plot(single_c5)

summary(single_c5)
## use https://www.tidymodels.org/start/tuning/ to tune a tree
## tidymodels only provides for tuning min_n
## tuned c5.0===================================================================

c5_tune <-
  decision_tree(
    min_n = tune()
  ) %>%
  set_engine("C5.0") %>%
  set_mode("classification")

c5_tune_param <-
  grid_random(min_n(), size = 5)

c5_folds <- mc_cv(df_train,
  props = 0.75,
  times = 5
)

c5_cv_wrkflw <- workflow() %>%
  add_model(c5_tune) %>%
  add_recipe(bank_rec)

c5_cv_res <-
  c5_cv_wrkflw %>%
  tune_grid(
    resamples = c5_folds,
    grid = c5_tune_param
  )

c5_best <-
  c5_cv_res %>%
  show_best("roc_auc")

## get within fold information
c5_best_info <-
  c5_best %>%
  select(
    auc = mean,
    auc_se = std_err
  ) %>%
  mutate(
    model = "C5.0",
    wrkflw = "Tuned-CV"
  ) %>%
  top_n(1, auc)

c5_wrkflw_best <-
  c5_cv_wrkflw %>%
  finalize_workflow(c5_best[1, ])

c5_best_fit <-
  c5_wrkflw_best %>%
  fit(df_train)

c5_best_fit %>%
  pull_workflow_fit() %>%
  vip()

c5_wrkflw_best %>%
  last_fit()

c5_last_fit <-
  c5_wrkflw_best %>%
  last_fit(bank_split)

tidyFolds <- function(fit, model_name, wrkflw_name) {
  temp_tib <-
    fit$.predictions[[1]] %>%
    select(
      Actual = y,
      no = .pred_no,
      yes = .pred_yes,
      Predicted = .pred_class
    ) %>%
    mutate(
      model = model_name,
      wrkflw = wrkflw_name
    )

  return(temp_tib)
}

c5_fold_tib <-
  tidyFolds(c5_last_fit, "C5.0", "Tuned-CV")

#
# c5_fold_tib <-
# c5_last_fit$.predictions[[1]] %>%
# select(
# Actual = y,
# no = .pred_no,
# yes = .pred_yes,
# Predicted = .pred_class
# ) %>%
# mutate(
# model = "C5.0",
# wrkflw = "Tuned-CV"
# )
#
## collect results for this
# c5_last_fit %>%
# collect_predictions() %>%
# roc_curve(y, .pred_no) %>%
# autoplot()
#
#
# c5_last_fit %>%
# collect_metrics()
#
#
# c5_res_probs <-
# predict(c5_last_fit, df_baked_test, type = "prob") %>%
# as_tibble()
#
# c5_single_pred <-
# df_baked_test %>%
# select(y) %>%
# bind_cols(c5_res_probs) %>%
# bind_cols(predict(single_c5, df_baked_test)) %>%
# rename(Actual = y, Predicted = "...4") %>%
# mutate(
# model = "C5.0",
# wrkflw = "Tuned"
# )




## Random forest
## Get n cores for ranger
cores_found <- parallel::detectCores()

# first do rf without tuning
single_rf <-
  ranger::ranger(y ~ .,
    data = df_baked_train,
    importance = "impurity",
    probability = T
  )

single_rf %>%
  vip()

summary(single_rf)


rf_single_probs <-
  predict(single_rf,
    df_baked_test,
    type = "response"
  )


rf_single_tib <-
  df_baked_test %>%
  select(y) %>%
  bind_cols(rf_single_probs$predictions %>% as_tibble()) %>%
  mutate(Predicted = as_factor(if_else(yes > 0.5, "yes", "no"))) %>%
  rename(Actual = y) %>%
  mutate(
    model = "Ranger",
    wrkflw = "Hold-back"
  )
#
# rf_auc <-
# rf_single_tib %>%
# roc_auc(
# truth = Actual,
# no
# )
#
# num_mets <- metric_set(bal_accuracy, kap, sens, spec)
#
# rf_mets <-
# bind_rows(
# rf_auc,
# rf_single_tib %>%
# num_mets(Actual, estimate = Predicted)
# )
#
#
#
#
# rf_res_probs$predictions
#
# summary(rf_res_probs)
#
# table(
# df_baked_test$y,
# rf_res_probs$predictions
# )
#
#
# c5_single_pred <-
# df_baked_test %>%
# select(y) %>%
# bind_cols(c5_res_probs) %>%
# bind_cols(predict(single_c5, df_baked_test)) %>%
# rename(Actual = y, Predicted = "...4")
#
# c5_auc <-
# c5_single_pred %>%
# roc_auc(
# truth = Actual,
# no
# )
#
# num_mets <- metric_set(bal_accuracy, kap, sens, spec)
#
## tuned rf model
# prep model
rf_mod <-
  rand_forest(
    mtry = tune(),
    min_n = tune()
  ) %>%
  set_engine("ranger",
    num.threads = cores_found,
    importance = "impurity"
  ) %>%
  set_mode("classification")

rf_tune_param <-
  grid_random(mtry(range = c(2, 8)), min_n(), size = 10)

rf_cv_wrkflw <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(bank_rec)

rf_cv_res <-
  rf_cv_wrkflw %>%
  tune_grid(
    resamples = c5_folds,
    grid = rf_tune_param
  )

rf_best <-
  rf_cv_res %>%
  show_best("roc_auc")

rf_best_info <-
  rf_best %>%
  select(
    auc = mean,
    auc_se = std_err
  ) %>%
  mutate(
    model = "Ranger",
    wrkflw = "Tuned-CV"
  ) %>%
  top_n(1, auc)

rf_wrkflw_best <-
  rf_cv_wrkflw %>%
  finalize_workflow(rf_best[1, ])

rf_best_fit <-
  rf_wrkflw_best %>%
  fit(df_train)

rf_best_fit %>%
  pull_workflow_fit() %>%
  vip()

rf_last_fit <-
  rf_wrkflw_best %>%
  last_fit(bank_split)

rf_fold_tib <-
  tidyFolds(rf_last_fit, "Ranger", "Tuned-CV")

## Eval and compare
preds_all <-
  bind_rows(
    c5_single_tib,
    c5_fold_tib,
    rf_single_tib,
    rf_fold_tib
  )

res_aucs <-
  preds_all %>%
  group_by(model, wrkflw) %>%
  roc_auc(
    truth = Actual,
    no
  )

num_mets <- metric_set(bal_accuracy, kap, sens, spec)

res_mets <-
  bind_rows(
    res_aucs,
    preds_all %>%
      group_by(model, wrkflw) %>%
      num_mets(Actual, estimate = Predicted)
  )

res_mets %>%
  select(-.estimator) %>%
  pivot_wider(
    names_from = c(model, wrkflw),
    values_from = .estimate
  )


## step_novel

bank_rf_fit <-
  bank_rf_workflow %>%
  fit(data = df_train)

rf_pred <-
  predict(bank_rf_fit,
    df_test,
    type = "prob"
  ) %>%
  bind_cols(df_test %>% select(y))

rf_pred %>%
  roc_curve(truth = y, .pred_no) %>%
  autoplot()

bank_rf_fit %>%
  pull_workflow_fit() %>%
  vip()

# bank_nb_workflow <-
# workflow() %>%
# add_model(nb_mod) %>%
# add_recipe(bank_rec)
#
### step_novel
#
# bank_fit <-
# bank_nb_workflow %>%
# fit(data = df_train)
#
# nb_pred <-
# predict(bank_fit,
# df_test,
# type = "prob") %>%
# bind_cols(df_test %>% select(y))
#
# nb_pred %>%
# roc_curve(truth = y, .pred_no) %>%
# autoplot()

# try randomforest


## decision tree now
bank_tr_workflow <-
  workflow() %>%
  add_model(rpart_mod) %>%
  add_recipe(bank_rec)

## step_novel

bank_tr_fit <-
  bank_tr_workflow %>%
  fit(data = df_train)

tr_pred <-
  predict(bank_tr_fit,
    df_test,
    type = "prob"
  ) %>%
  bind_cols(df_test %>% select(y))

tr_pred %>%
  roc_curve(truth = y, .pred_no) %>%
  autoplot()
