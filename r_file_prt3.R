### Title:  CA2 part 3
### Author: x19175329@student.ncirl.ie
### Desc:   R program to analyse the bank dataset

set.seed(11111987)

library(tidyverse)
library(tidymodels)
library(here)
library(naniar)
## library(naivebayes)
library(recipeselectors)
library(themis)
library(corrr)
library(C50)

## add a user choice here, incase he doesnt want to DL off github or dont if it goes in appendix
"https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip"

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
  mutate_if(is.character, as.factor)

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

# Recipe to ready data for modelling
bank_rec <-
  recipe(y ~ ., data = df_train) %>%
  update_role(y, new_role = "outcome") %>%
  # step_novel(all_predictors(), -all_numeric()) %>%
  step_unknown(all_predictors(), -all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

#  step_select_roc(all_predictors(),
#                  threshold = 0.6,
#                  outcome = "y") %>%
#  step_rose(y) #actually made no difference to rpart?

# tree based models can handle these bninaries as numerics

## Pre-process the data
bank_prep <- prep(bank_rec, training = df_train)
df_baked_train <- bake(bank_prep, df_train)

# probably need to factorise variables first
C5.0(
  x = df_baked_train[, -43],
  y = as.factor(df_baked_train[, 43])
)

## Apply the rpart model once

c5_mod <-
  decision_tree() %>%
  set_engine("C5.0") %>%
  set_mode("classification") %>%
  translate()

c5_workflow <-
  workflow() %>%
  add_model(c5_mod) %>%
  add_recipe(bank_rec)

## step_novel

c5_fit <-
  c5_workflow %>%
  fit(data = df_train)

summary(bank_tree_fit)


foo_train_recipe <- prep(pre_processor(df_train), training = df_train)
training_processed <- bake(foo_train_recipe, new_data = df_training)

## cross-validation
df_train_cvs <-
  mc_cv(df_baked_train,
    prop = 0.75,
    times = 10
  )



nb_mod <-
  discrim::naive_Bayes() %>%
  set_engine("naivebayes") %>%
  translate()

rf_mod <-
  rand_forest(
    mtry = 3,
    trees = 200,
    min_n = 20
  ) %>%
  set_engine("ranger") %>%
  set_mode("classification")


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

bank_rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(bank_rec)

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
