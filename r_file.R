### Title:  CA2 part 1
### Author: x19175329@student.ncirl.ie
### Desc:   All-in-one analysis file for DAWM CA1


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
  "arules",
  "arulesViz",
  "shinythemes",
  "ggpubr"
)

# load all packages
lapply(package_list, library, character.only = T)

# Set ggplot2 theme globally
theme_set(theme_classic())


## Data import and pre-processing===============================================

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

# A regex to extract the dates from malformed 1st column
date_rgx <- "\\d+/\\d+/\\d+"

g_datefix <-
  g_in %>%
  as_tibble() %>%
  mutate(
    dates = str_extract(x.1, date_rgx),
    x.1 = str_remove(x.1, date_rgx)
  )

# Number of unique days
n_dates <- length(unique(g_datefix$dates))

# Create and ID column, change to long-form dataset,
# removing blanks, whitespace and duplicates
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

## make into list, then coerce to transactions object
g_list <- split(
  x = g_tidy$items,
  f = g_tidy$id_var
)

g_trans <- as(
  object = g_list,
  Class = "transactions"
)

## Quick exploration============================================================
# Use tidy data in-part for easy output

# Get total items per transaction, then generate summary stats and histogram
g_explore <-
  g_tidy %>%
  group_by(id_var) %>%
  summarise(n_by_id = n()) %>%
  ungroup()

g_sum_tab <-
  g_explore %>%
  summarise(
    Min = min(n_by_id),
    `25%` = quantile(n_by_id, 0.25),
    Median = median(n_by_id),
    `75%` = quantile(n_by_id, 0.75),
    Max = max(n_by_id),
    Mode = modeest::mlv(n_by_id)
  ) %>%
  ggtexttable(rows = NULL, theme = ttheme("light"))

ggsave(
  filename = "g_sum_tab.png",
  plot = g_sum_tab,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)

# Histogram uses Sturges rule for breaks
g_hist_plot <-
  g_explore %>%
  ggplot(aes(x = n_by_id)) +
  geom_histogram(
    fill = "grey75",
    colour = "grey25",
    bins = 1 + log2(length(g_trans))
  ) +
  labs(y = "Count of transactions", x = "N items per transaction")

ggsave(
  filename = "g_hist_plot.png",
  plot = g_hist_plot,
  dpi = 300,
  units = "cm",
  height = 8,
  width = 12
)

# Check data structure for data integrity issues
png(
  file = "g_integrity.png",
  width = 600
)
image(g_trans[1:25, ])
dev.off()

# use a rule-of-thumb for support (i.e. bought twice a day)
supp_rot <- (n_dates * 2) / nrow(g_tidy)

# Do relative item frequencies
png(
  file = "g_rel_freq.png",
  width = 600
)
itemFrequencyPlot(g_trans, support = supp_rot, topN = 10)
dev.off()

## Analysis=====================================================================
# Set up parameters and execute Apriori
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

# Explore results
summary(fit)

inspect(sort(fit, by = "confidence")[1:20])
inspect(sort(fit, by = "lift")[1:20])

# Plot positions of rules with support, confidence and lift
# Note these cant be plotted to same window and are combined manually

png(
  file = "g_scl_plot.png",
  height = 400,
  width = 400
)
plot(fit)
dev.off()

png(
  file = "g_two_plot.png",
  height = 400,
  width = 400
)
plot(fit, method = "two-key plot", new = T)
dev.off()

# Get top rules by lift, table and graph
top_20_rules <- head(fit, n = 20, by = "lift")
top_10_rules <- head(fit, n = 10, by = "lift")

top_rules_tab <-
  inspect(top_20_rules) %>%
  select(-2) %>%
  as_tibble() %>%
  mutate_at(
    vars(support, confidence, coverage, lift),
    function(x) {
      round(x, 2)
    }
  ) %>%
  ggtexttable(rows = NULL, theme = ttheme("light"))

ggsave(
  filename = "g_top_tab.png",
  plot = top_rules_tab,
  dpi = 300,
  units = "cm",
  height = 20,
  width = 20
)

png(
  file = "g_top_network.png",
  width = 600,
  height = 600,
  pointsize = 15
)
plot(top_10_rules, method = "graph")
dev.off()

## Get ones that exclude vegetables
`%notin%` <- Negate(`%in%`)
fit_noveg <- subset(fit, items %notin% "vegetables")

inspect(sort(fit_noveg, by = "lift")[1:20])
top_noveg_rules <- head(fit_noveg, n = 10, by = "lift")

png(
  file = "g_novegtop_network.png",
  width = 600,
  height = 600,
  pointsize = 15
)
plot(top_noveg_rules, method = "graph")
dev.off()

# Double the support threshold and re-run Apriori
ap_params_2 <- list(
  support = supp_rot * 2,
  confidence = 0.25,
  minlen = 2,
  maxlen = 2,
  maxtime = 20,
  target = "rules"
)

fit_2 <- apriori(g_trans,
  parameter = ap_params_2
)

# Repeat of some previous steps
summary(fit_2)
plot(fit_2, method = "two-key")
top20_2 <- head(fit_2, n = 20, by = "lift")

png(
  file = "g_top20_2_network.png",
  width = 600,
  height = 600,
  pointsize = 15
)
plot(top20_2, method = "graph")
dev.off()
