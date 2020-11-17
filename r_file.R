### Title:  R analysis file
### Author: x19175329@student.ncirl.ie
### Desc:   All-in-one analysis file

### General
set.seed(11111987)
library(tidyverse)
library(tidymodels)
library(here)
library(naniar)
library(arules)
library(arulesViz)
library(shinythemes)
library(naivebayes)

### Part 1: Association rules

##Preview data
g_lines <- readLines("Groceries.csv")

##fix dates not being seperated
##date_rgx <- "\\d+/\\d+/\\d+"

## this way is good, but comment out for now
#g_dates <- str_extract(g_lines, date_rgx)
#g_nodates <- str_remove(g_lines, date_rgx)
##
#### convert to list
#### remove last two chars from every row in vector
#g_nodates_s <- substr(g_nodates, 1, nchar(g_nodates) - 2)
#g_nodates_l <- str_split(g_nodates_s, ", ")
#names(g_nodates_l) <- g_dates
##
#g_trans <- as(g_nodates_l, "transactions")
##
#inspect(head(g_trans))
#
#itemFrequencyPlot(g_trans)
#
#image(g_trans[1:10])
#
#arls <- apriori(g_trans)
#
#plot(arls)
#inspectDT(arls)
#ruleExplorer(arls)

##that was fine, but really not sure if thats what Enda wants...
#this bit here is not used
#g_in <- read_csv("Groceries.csv",
                 #col_names = F,
                 #trim_ws = T,
                 #)

##actually do this here
#all the row-lengths
g_row_l <- count.fields("Groceries.csv")
g_row_l_max <- max(g_row_l)

g_in <-
read.table("Groceries.csv",
           sep = ",",
           header = F,
           fill = T,
           col.names = paste("x", seq(1:g_row_l_max)))

date_rgx <- "\\d+/\\d+/\\d+"


g_tidy <-
  g_in %>%
  as_tibble() %>%
  mutate(dates = str_extract(x.1, date_rgx),
         x.1 = str_remove(x.1, date_rgx)) %>%
rowid_to_column("id_var") %>%
pivot_longer(cols = x.1:x.54, values_to = "items") %>%
mutate(items = trimws(items)) %>%
select(id_var, dates, items) %>%
filter(items != "",
       items != "") %>%
group_by(id_var, dates) %>%
distinct() %>%
ungroup()

g_wide_bin <-
  g_tidy %>%
##  rowid_to_column("id_var") %>%
mutate(dummy = 1) %>%
  pivot_wider(names_from = items,
              values_from = dummy,
              id_cols = c(id_var)) %>%
#mutate_at(vars(yogurt:sugar), ~ ifelse(is.na(.), NA, 1)) %>%
mutate_at(vars(yogurt:sugar), ~ as.factor(.)) %>%
select(-id_var)

g_trans <- as(g_wide_bin, "transactions")
plot(g_trans)

ap_params <- list(support = 0.10,
                  confidence = 0.50,
                  minlen = 2,
                  maxlen = 4)


fit <- apriori(g_trans,
               parameter = ap_params)

sort(fit, by="support")[1:10]
arulesViz::ruleExplorer(fit)
inspect(fit)

##


### Part 2

## Data in

df_in <- read_csv(here("adult.data"),
                  col_names = F)

rand_samp = sample(1:nrow(df_in),
                   size = 10000,
                   replace = F
                    )

df_samp <- df_in[rand_samp, ]

# column names come in a seperate information file
col_names_in <- read_lines(
  here("adult.names")
)

col_strings_raw <- str_match_all(col_names_in, "\\w+(?=.*:)")

col_strings_clean <- c()

for(i in 97:length(col_strings_raw)){

  if(length(col_strings_raw[[i]]) > 1){

     foo_string <- paste(col_strings_raw[[i]][1],
           col_strings_raw[[i]][2],
           sep = "_"
           )

    col_strings_clean[i-96] <- foo_string
    } else{

    col_strings_clean[i-96] <- col_strings_raw[[i]][1]
      }
}

col_strings_clean

glimpse(df_samp)

colnames(df_samp) <- c(col_strings_clean, "target")

df_samp

##inspect the data
common_na_strings
df_nas <-
  df_samp %>% replace_with_na_all(
              condition = ~.x %in% common_na_strings)


vis_miss(x = df_nas)

View(df_nas)

##try bank dataset==================================================
df_bank_in <-
    read_delim(here("bank-additional-full.csv"),
               delim = ";")

rand_samp = sample(1:nrow(df_bank_in),
                   size = 10000,
                   replace = F
                   )

df_bank_samp <- df_bank_in[rand_samp, ]

colnames(df_bank_samp) <- snakecase::to_snake_case(colnames(df_bank_samp))

##remove this column, as per dataset documentation
df_bank_in$duration <- NULL

funModeling::status(df_bank_in)

custom_na_strings <- c(common_na_strings, "nonexistent",  "unknown")

df_nas <-
  df_bank_samp %>% replace_with_na_all(
                condition = ~.x %in% custom_na_strings)

funModeling::status(df_nas)

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
  step_zv(all_predictors())

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
