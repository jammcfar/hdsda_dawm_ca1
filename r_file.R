### Title:  Association rules analysis file
### Author: x19175329@student.ncirl.ie
### Desc:   All-in-one analysis file for DAWM CA1

# Set seed
set.seed(11111987)

# Packages
library(tidyverse)
library(here)
library(arules)
library(arulesViz)
library(shinythemes)

# Get data length
g_lines <- readLines("Groceries.csv")

# Get the max row width so we can fill with NAs
g_row_l <- count.fields("Groceries.csv")
g_row_l_max <- max(g_row_l)

g_in <-
  read.table("Groceries.csv",
    sep = ",",
    header = F,
    fill = T,
    col.names = paste("x", seq(1:g_row_l_max))
  )

# A regex to grab the dates from malformed column
date_rgx <- "\\d+/\\d+/\\d+"

g_datefix <-
  g_in %>%
  as_tibble() %>%
  mutate(
    dates = str_extract(x.1, date_rgx),
    x.1 = str_remove(x.1, date_rgx)
  )

# number of unique days
n_dates <- length(unique(g_datefix$dates))

g_tidy <-
  g_datefix %>%
  rowid_to_column("id_var") %>%
  pivot_longer(cols = x.1:x.54, values_to = "items") %>%
  mutate(items = trimws(items)) %>%
  select(id_var, items) %>%
  filter(
    items != "",
    items != ""
  ) %>%
  group_by(id_var) %>%
  distinct() %>%
  mutate(item_no = 1:n()) %>%
  ungroup()

## quick exploration
g_for_sum <-
  g_tidy %>%
  group_by(id_var) %>%
  summarise(n_by_id = n()) %>%
  ungroup() %>%
  summarise(
    Min = min(n_by_id),
    Median = median(n_by_id),
    Max = max(n_by_id),
    Mode = modeest::mlv(n_by_id)
  )

## quick hist (redo)
hist(g_tidy %>%
  group_by(id_var) %>%
  summarise(n_by_id = n()) %>% select(n_by_id) %>% as_vector(),
breaks = 25, xlim = c(0, 30)
)

## make into list, then coerce to transactions object
g_list <- split(g_tidy$items, g_tidy$id_var)

g_trans <- as(
  object = g_list,
  Class = "transactions"
)

## Explore transactions
# Quick summary of transactions
g_summ <- summary(g_trans)
(g_summ)

# Check data structure for obvious issues
image(g_trans[1:50, ], )

# use a rule-of-thumb for support (i.e. bought twice a day)
supp_rot <- (n_dates * 2) / nrow(g_tidy)

# Do item frequencies
itemFrequencyPlot(g_trans, topN = 10, type = "absolute", main = "Item Frequency")
itemFrequencyPlot(g_trans, support = supp_rot, topN = 10)

## Run Apriori
# Set up parameters
ap_params <- list(
  support = supp_rot,
  confidence = 0.25,
  minlen = 2,
  maxlen = 10,
  maxtime = 20,
  target = "rules"
)

fit <- apriori(g_trans,
  parameter = ap_params
)

summary(fit)

inspect(sort(fit, by = "confidence")[1:20])
inspect(sort(fit, by = "lift")[1:20])

## most interesting rules lie on the support/confidence border (sc-optimal rules)Bayardo, Jr RJ, Agrawal R (1999)
plot(fit)
plot(fit, method = "two-key plot")

## Plot rules in a grid
plot(fit, method = "grouped")

top_rules <- head(fit, n = 20, by = "lift")
inspect(sort(top_rules, by = "lift"))
plot(top_rules, method = "graph")

# arulesViz::ruleExplorer(fit)
inspect(fit)

## get ones that aren't vegetables
`%notin%` <- Negate(`%in%`)
fit_noveg <- subset(fit, items %notin% "vegetables")

inspect(sort(fit_noveg, by = "lift")[1:20])
top_noveg_rules <- head(fit_noveg, n = 10, by = "lift")
plot(top_noveg_rules, method = "graph")


ap_params_2 <- list(
  support = supp_rot,
  confidence = 0.25,
  minlen = 2,
  maxlen = 2,
  maxtime = 20,
  target = "rules"
)

fit_2 <- apriori(g_trans,
  parameter = ap_params_2
)

summary(fit_2)

inspect(sort(fit_2, by = "confidence")[1:20])
inspect(sort(fit_2, by = "lift")[1:20])
