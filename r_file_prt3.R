### Title:  CA2 part 3
### Author: x19175329@student.ncirl.ie
### Desc:   R program to analyse the bank dataset
### Notes:  Some of the modelling code is duplicated for interprability


## Pre-data loading actions=====================================================

set.seed(11111987)

# convenience function to install the packages if they are not installed
package_installer <- function(x) {
  # find packages from vector which are not installed and save them
  missing_pkg <- which(!package_list %in% installed.packages()[, 1])
  # if there are any missing ones then install them, else print a message
  if (length(missing_pkg) > 0) {
    install.packages(package_list[missing_pkg])
  } else {
    print("All packages already installed!")
  }
}

# vector of required package names
package_list <- c(
  "tidyverse",
  "snakecase",
  "naniar",
  "corrr",
  "tidymodels",
  "ranger",
  "C50",
  "vip",
  "parallel",
  "ggpubr"
)

package_installer(package_list)

# load all packages
lapply(package_list, library, character.only = T)

## Get n cores for ranger
cores_found <- parallel::detectCores()

## A function to tidy predictions from test data
tidyFolds <- function(fit, model_name) {
  temp_tib <-
    fit$.predictions[[1]] %>%
    select(
      Actual = y,
      no = .pred_no,
      yes = .pred_yes,
      Predicted = .pred_class
    ) %>%
    mutate(
      model = model_name
    )

  return(temp_tib)
}


## Data import and pre-processing===============================================

# Download zipped dataset and store as a temporary file
data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip"

temp_file <- tempfile()

download.file(data_url, temp_file)

# Read in and downsample bank data.
df_bank_in <-
  read_delim(unz(temp_file, "bank-additional/bank-additional-full.csv"),
    delim = ";"
  ) %>%
  slice_sample(n = 10000)

# Unload from memory
unlink(temp_file)

# Set colname to snakecase, as per tidy style guide
colnames(df_bank_in) <- snakecase::to_snake_case(colnames(df_bank_in))

# Some unusual na strings present in data to be removed
custom_na_strings <- c(common_na_strings, "nonexistent", "unknown")

# Remove "duration", as per documentation
# Also remove pdays and poutcome; these are problematic
# and their correct encoding relies on SME knowledge
# Finally, sets chars to factors, creates NAs and removes dupes
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
  distinct()

# Checking for intercorrelations of numerics, then tidy and output
df_cors <-
  df_bank_proc %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  shave() %>%
  pivot_longer(age:nr_employed) %>%
  arrange(desc(value)) %>%
  top_n(5, value) %>%
  mutate(value = round(value, 3)) %>%
  rename(
    `Variable 1` = rowname,
    `Variable 2` = name,
    `Pearson's R` = value
  ) %>%
  ggtexttable(rows = NULL, theme = ttheme("light"))

ggsave(
  filename = "bank_cors.png",
  plot = df_cors,
  dpi = 300,
  units = "cm",
  height = 6,
  width = 10
)


## Initial Tidymodels preperations==============================================

# Define the data split
bank_split <- initial_split(df_bank_proc, prop = 7 / 10)

# Create tibbles for the two sets:
df_train <- training(bank_split)
df_test <- testing(bank_split)

# Recipe to ready data for modelling
# step_zv removes zero-variance predictors
# step_knnimpute performs necessary knn_imputation with defaults
bank_rec <-
  recipe(y ~ ., data = df_train) %>%
  update_role(y, new_role = "outcome") %>%
  step_zv(all_predictors()) %>%
  step_knnimpute(all_predictors())

# Pre-process the data for tree plot later outside
# the Tidymodels framework
bank_prep <- prep(bank_rec, training = df_train)
df_baked_train <- bake(bank_prep, df_train)

## Logic for monte-carlo cross-validation
cv_folds <- mc_cv(df_train,
  props = 0.70,
  times = 10
)


## C5.0 model===================================================================

# Prep model
c5_tune <-
  decision_tree(
    min_n = tune()
  ) %>%
  set_engine("C5.0") %>%
  set_mode("classification")

# Prep tuning parameters
c5_tune_param <-
  grid_random(min_n(),
    size = 10
  )

# Prep workflow
c5_cv_wrkflw <- workflow() %>%
  add_model(c5_tune) %>%
  add_recipe(bank_rec)

# Perform tuning
c5_cv_res <-
  c5_cv_wrkflw %>%
  tune_grid(
    resamples = cv_folds,
    grid = c5_tune_param
  )

## Get best tuning results
c5_best <-
  c5_cv_res %>%
  show_best("roc_auc")

# Plot tuning results
c5_tune_plot <-
  c5_best %>%
  mutate(min_n = as_factor(min_n)) %>%
  ggplot(aes(x = min_n, y = mean)) +
  geom_col(fill = "grey85", colour = "grey25") +
  geom_errorbar(aes(
    ymin = mean - (std_err * 1.96),
    ymax = mean + (std_err * 1.96)
  ),
  width = 0.3
  ) +
  coord_cartesian(ylim = c(0.6, 0.8)) +
  theme_classic() +
  labs(
    y = "Mean AUC",
    x = "Minimum samples required for split"
  )

ggsave(
  filename = "c5_tune_plot.png",
  plot = c5_tune_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)

# Extract best model
c5_wrkflw_best <-
  c5_cv_wrkflw %>%
  finalize_workflow(c5_best[1, ])

# Apply to whole training set
c5_best_fit <-
  c5_wrkflw_best %>%
  fit(df_train)

# Get model from this last step
c5_best_fit_wrkflw <-
  c5_best_fit %>%
  pull_workflow_fit()

# Generate summary (cant plot within this framework)
summary(c5_best_fit_wrkflw$fit)

# This model is functionally identical
# Use it for a plot of the tree model
c5_c <- C5.0Control(minCases = c(c5_best[1, 1]))

single_c5 <-
  C5.0(y ~ .,
    data = df_baked_train, control = c5_c
  )

png(
  file = "c5_tree_fit.png",
  width = 1800
)
plot(single_c5)
dev.off()

# Plot of variable importances
c5_varimp_plot <-
  c5_best_fit_wrkflw %>%
  vip(aesthetics = list(color = "grey25", fill = "grey75")) +
  theme_classic()

ggsave(
  filename = "c5_varimp_plot.png",
  plot = c5_varimp_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)

## Apply final model to test data
c5_last_fit <-
  c5_wrkflw_best %>%
  last_fit(bank_split)

## Tidy predictions
c5_pred_tib <-
  tidyFolds(c5_last_fit, "C5.0")


## Random forest================================================================

# Prep model
rf_mod <-
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
  set_engine("ranger",
    num.threads = cores_found,
    importance = "impurity"
  ) %>%
  set_mode("classification")

# Prep tuning parameters
rf_tune_param <-
  grid_random(mtry(range = c(2, 8)), min_n(), size = 10)

# Prep workflow
rf_cv_wrkflw <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(bank_rec)

# Tune model
rf_cv_res <-
  rf_cv_wrkflw %>%
  tune_grid(
    resamples = cv_folds,
    grid = rf_tune_param
  )

# Get best tuning results
rf_best <-
  rf_cv_res %>%
  show_best("roc_auc")

# Plot tuning results
rf_tune_plot <-
  rf_best %>%
  mutate(
    mtry = as_factor(mtry),
    min_n = as_factor(min_n)
  ) %>%
  ggplot(aes(x = min_n, y = mean, fill = mtry)) +
  geom_bar(
    position = "dodge",
    stat = "identity",
    colour = "grey25"
  ) +
  geom_errorbar(aes(
    ymin = mean - (std_err * 1.96),
    ymax = mean + (std_err * 1.96)
  ),
  width = 0.3
  ) +
  coord_cartesian(ylim = c(0.6, 0.8)) +
  theme_classic() +
  labs(
    y = "Mean AUC",
    x = "Minimum samples required for split"
  ) +
  scale_fill_brewer("mtry")

ggsave(
  filename = "rf_tune_plot.png",
  plot = rf_tune_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)

# Pull out best model
rf_wrkflw_best <-
  rf_cv_wrkflw %>%
  finalize_workflow(rf_best[1, ])

# Fit to all training data
rf_refit <-
  rf_wrkflw_best %>%
  fit(df_train)

# Get model from last step
rf_refit_fit <-
  rf_refit %>%
  pull_workflow_fit()

# Summary data of final model
summary(rf_refit_fit$fit)

treeInfo(rf_refit_fit$fit, tree = 1)

# Variable importance of final model
rf_varimp_plot <-
  rf_refit_fit %>%
  vip(aesthetics = list(colour = "grey25", fill = "grey75")) +
  theme_classic()

ggsave(
  filename = "rf_varimp_plot.png",
  plot = rf_varimp_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)


# Apply to test data
rf_last_fit <-
  rf_wrkflw_best %>%
  last_fit(bank_split)

# Tidy output
rf_pred_tib <-
  tidyFolds(rf_last_fit, "Ranger")

## Eval and compare=============================================================

# Bind rows and prepare a table of high-level metrics
preds_all <-
  bind_rows(
    c5_pred_tib,
    rf_pred_tib
  )


## Do ROC curves
roc_plot_dat <-
  preds_all %>%
  group_by(model) %>%
  roc_curve(
    truth = Actual,
    no
  )

# Get better thresholds based on geometric mean
roc_thresholds <-
  roc_plot_dat %>%
  mutate(g_mean = sqrt(specificity * sensitivity)) %>%
  arrange(desc(g_mean)) %>%
  top_n(1, g_mean)


# Plot the curves, highlighting optimal thresholds
roc_plot <-
  roc_plot_dat %>%
  autoplot() +
  geom_point(
    data = roc_thresholds,
    aes(
      x = 1 - specificity,
      y = sensitivity,
      colour = model
    )
  ) +
  geom_text(
    data = roc_thresholds[1, ],
    aes(
      x = 1 - specificity,
      y = sensitivity,
      colour = model,
      label = paste(
        "TH: ",
        round(.threshold, 3),
        "\nSN: ",
        round(sensitivity, 3),
        "\nSP: ",
        round(specificity, 3)
      )
    ),
    vjust = 0.5,
    hjust = 1.25,
    size = 3
  ) +
  geom_text(
    data = roc_thresholds[2, ],
    aes(
      x = 1 - specificity,
      y = sensitivity,
      colour = model,
      label = paste(
        "TH: ",
        round(.threshold, 3),
        "\nSN: ",
        round(sensitivity, 3),
        "\nSP: ",
        round(specificity, 3)
      )
    ),
    vjust = 1,
    hjust = -0.2,
    size = 3
  ) +
  scale_colour_manual("Model",
    values = c("#BD6B31", "#3182BD")
  ) +
  labs(x = "1 - Specificity", y = "Sensitivity")

ggsave(
  filename = "roc_plot.png",
  plot = roc_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)

# Calc AUC
res_aucs <-
  preds_all %>%
  group_by(model) %>%
  roc_auc(
    truth = Actual,
    no
  )

# For other metrics, recalculate predicted classifications
preds_adjusted <-
  preds_all %>%
  select(-Predicted) %>%
  left_join(roc_thresholds, by = c("model" = "model")) %>%
  select(-specificity, -sensitivity, -g_mean) %>%
  mutate(Predicted = case_when(
    no >= .threshold ~ "no",
    TRUE ~ "yes"
  )) %>%
  mutate(Predicted = as_factor(Predicted))


# Calc other metrics
kum_mets <- metric_set(bal_accuracy, kap, sens, spec, ppv, npv)

res_mets <-
  bind_rows(
    res_aucs,
    preds_adjusted %>%
      group_by(model) %>%
      num_mets(Actual, estimate = Predicted)
  )

# Compile to a tidy table
metrics_out <-
  res_mets %>%
  select(-.estimator,
    Metric = .metric
  ) %>%
  pivot_wider(
    names_from = c(model),
    values_from = .estimate
  ) %>%
  mutate_at(c("C5.0", "Ranger"), function(x) {
    round(x, 3)
  }) %>%
  ggtexttable(rows = NULL, theme = ttheme("light"))

ggsave(
  filename = "model_metrics.png",
  plot = metrics_out,
  dpi = 300,
  units = "cm",
  height = 6,
  width = 10
)

# Do confusion matrices (manually, framework doesnt support %'s)
conf_tidy <-
  preds_adjusted %>%
  group_by(model) %>%
  count(Actual, Predicted) %>%
  group_by(model, Predicted) %>%
  mutate(total = sum(n)) %>%
  mutate(prop = n / total) %>%
  mutate(Actual = factor(Actual, levels = c("yes", "no")))

conf_plot <-
  conf_tidy %>%
  ggplot(aes(x = Predicted, y = Actual, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = paste("n = ", n, "\n", round(prop, 3) * 100, "%"))) +
  facet_wrap(model ~ ., strip.position = "bottom") +
  theme_classic() +
  labs(fill = "% of\npredicted\nvalues") +
  scale_fill_gradient(low = "#DEEBF7", high = "#3182BD") +
  scale_x_discrete(position = "top")

ggsave(
  filename = "conf_plot.png",
  plot = conf_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 15
)
