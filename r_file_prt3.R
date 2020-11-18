### Title:  CA2 part 3
### Author: x19175329@student.ncirl.ie
### Desc:   R program to analyse the bank dataset

set.seed(11111987)

library(tidyverse)
library(tidymodels)
library(here)
library(naniar)
library(naivebayes)
library(recipeselectors)

df_bank_in <-
    read_delim(here("bank-additional-full.csv"),
               delim = ";")

rand_samp = sample(1:nrow(df_bank_in),
                   size = 10000,
                   replace = F
                   )

df_bank_samp <- df_bank_in[rand_samp, ]

## set colname to snakecase, as per tidy style guide
colnames(df_bank_samp) <- snakecase::to_snake_case(colnames(df_bank_samp))

##remove this column, and set pdays to NA, as per dataset documentation
## justification for pdays as missing https://medium.com/@abbdar/first-steps-in-machine-learning-predicting-subscription-for-bank-deposits-866516b90e4

df_bank_proc <-
  df_bank_samp %>%
  mutate(pdays = if_else(pdays != 999,
                         pdays,
                         NA_real_)) %>%
  select(-duration)

#funModeling::status(df_bank_in)

custom_na_strings <- c(common_na_strings, "nonexistent",  "unknown")

df_bank_nas <-
  df_bank_proc %>% replace_with_na_all(
                condition = ~.x %in% custom_na_strings)

#funModeling::status(df_nas)

##convert all characters to factors
df_clean <-
  df_nas %>%
  mutate_if(is.character, as.factor) %>%
  mutate(y = as_factor(y))

## lets try tidymodels. etc
df_split <- initial_split(df_clean, prop = 3/4)

# Create data frames for the two sets:
df_train <- training(df_split)
df_test  <- testing(df_split)

bank_rec <-
  recipe(y ~ ., data = df_train) %>%
  update_role(y, new_role = "outcome") %>%
  step_novel(all_predictors(), -all_numeric()) %>%
  step_unknown(all_predictors(), -all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_select_roc(all_predictors(),
                  threshold = 0.7,
                  outcome = "y")

nb_mod <-
  discrim::naive_Bayes() %>%
  set_engine("naivebayes")

rf_mod <-
  rand_forest(mtry = 3,
              trees = 200,
              min_n = 20) %>%
  set_engine("ranger") %>%
  set_mode("classification")

bank_nb_workflow <-
  workflow() %>%
  add_model(nb_mod) %>%
  add_recipe(bank_rec)

##step_novel

bank_fit <-
  bank_nb_workflow %>%
  fit(data = df_train)

nb_pred <-
    predict(bank_fit,
            df_test,
            type = "prob") %>%
  bind_cols(df_test %>% select(y))

nb_pred %>%
  roc_curve(truth = y, .pred_no) %>%
  autoplot()

#try randomforest

bank_rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(bank_rec)

##step_novel

bank_rf_fit <-
  bank_rf_workflow %>%
  fit(data = df_train)

rf_pred <-
    predict(bank_rf_fit,
            df_test,
            type = "prob") %>%
  bind_cols(df_test %>% select(y))

rf_pred %>%
  roc_curve(truth = y, .pred_no) %>%
  autoplot()
