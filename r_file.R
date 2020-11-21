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


g_datefix <-
  g_in %>%
  as_tibble() %>%
  mutate(dates = str_extract(x.1, date_rgx),
         x.1 = str_remove(x.1, date_rgx))

#number of unique days
n_dates <- length(unique(g_datefix$dates))

g_tidy <-
  g_datefix %>%
    rowid_to_column("id_var") %>%
    pivot_longer(cols = x.1:x.54, values_to = "items") %>%
    mutate(items = trimws(items)) %>%
    select(id_var, items) %>%
    filter(items != "",
        items != "") %>%
    group_by(id_var) %>%
  distinct() %>%
    mutate(item_no = 1:n()) %>%
    ungroup()

##quick exploration
g_for_sum <-
    g_tidy %>%
    group_by(id_var) %>%
    summarise(n_by_id = n()) %>%
    summarise(Min = min(n_by_id),
              Median = median(n_by_id),
              Max = max(n_by_id),
              Mode = modeest::mlv(n_by_id)
              )

## quick hist (redo)
hist(g_tidy %>%
     group_by(id_var) %>%
     summarise(n_by_id = n()) %>% select(n_by_id) %>% as_vector(),
     breaks = 25, xlim = c(0,30))

## make into list, then coerce to transactions object
g_list <- split(g_tidy$items, g_tidy$id_var)

g_trans <- as(object = g_list,
              Class = "transactions")

g_summ <- summary(g_trans)
g_summ
## Do item frequencies

freq_params <- list(supp = 0.1, minlen = 2, maxlen = 4)

g_freq <- eclat(g_trans, parameter = freq_params)

##inspect(g_freq)
itemFrequencyPlot(g_trans, topN = 10, type = "absolute", main = "Item Frequency")
itemFrequencyPlot(g_trans, support = 0.1, topN = 10)
image(g_trans[1:200,],)

head(inspect(g_freq))

#support rule of thumb
supp_rot <- (n_dates * 2) / nrow(g_tidy)

ap_params <- list(support = 0.10,
                  confidence = supp_rot,
                  minlen = 2,
                  maxlen = 10,
                  maxtime = 20,
                  target = "rules")

fit <- apriori(g_trans,
               parameter = ap_params)

summary(fit)

inspect(fit)

inspect(sort(fit, by = "confidence")[1:20])
inspect(sort(fit, by = "lift")[1:20])

##most interesting rules lie on the support/confidence border (sc-optimal rules)Bayardo, Jr RJ, Agrawal R (1999)
plot(fit)
plot(fit, method = "two-key plot")


top_rules <- head(fit, n = 15, by = "lift")
plot(top_rules, method = "graph")

arulesViz::ruleExplorer(fit)
inspect(fit)

##get ones that aren't vegetables
`%notin%` <- Negate(`%in%`)
fit_noveg <-subset(fit, items %notin% "vegetables")

inspect(sort(fit_noveg, by = "lift")[1:20])
top_noveg_rules <- head(fit_noveg, n = 30, by = "confidence")
plot(top_noveg_rules, method = "graph")
#pasta and paper towels is a good one that might be actionable

### Part 2

## Data in
#
#df_in <- read_csv(here("adult.data"),
                  #col_names = F)
#
#rand_samp = sample(1:nrow(df_in),
                   #size = 10000,
                   #replace = F
                    #)
#
#df_samp <- df_in[rand_samp, ]
#
## column names come in a seperate information file
#col_names_in <- read_lines(
  #here("adult.names")
#)
#
#col_strings_raw <- str_match_all(col_names_in, "\\w+(?=.*:)")
#
#col_strings_clean <- c()
#
#for(i in 97:length(col_strings_raw)){
#
  #if(length(col_strings_raw[[i]]) > 1){
#
     #foo_string <- paste(col_strings_raw[[i]][1],
           #col_strings_raw[[i]][2],
           #sep = "_"
           #)
#
    #col_strings_clean[i-96] <- foo_string
    #} else{
#
    #col_strings_clean[i-96] <- col_strings_raw[[i]][1]
      #}
#}
#
#col_strings_clean
#
#glimpse(df_samp)
#
#colnames(df_samp) <- c(col_strings_clean, "target")
#
#df_samp
#
###inspect the data
#common_na_strings
#df_nas <-
  #df_samp %>% replace_with_na_all(
              #condition = ~.x %in% common_na_strings)
#
#
#vis_miss(x = df_nas)
#
#View(df_nas)


#
#g_wide_bin <-
  #g_tidy %>%
  ###rowid_to_column("id_var") %>%
#mutate(dummy = 1) %>%
  #pivot_wider(names_from = item_no,
              #values_from = items,
              #id_cols = c(id_var)) %>%
##mutate_at(vars(yogurt:sugar), ~ ifelse(is.na(.), NA, 1)) %>%
##mutate_at(vars(yogurt:sugar), ~ as.factor(.)) %>%
#select(everything())
#
##g_list <-
  ##g_tidy %>%
  ##select(-item_no) %>%
  ##group_by(id_var) %>%
  ##group_split()
#
##g_trans <-
  ##head(as.list(g_wide_bin)) %>%
  ##as(
              ##Class = "transactions")
#
#as.list(g_wide_bin %>% group_by(id_var))
#
#g_trans <- as(object = g_list,
              #Class = "transactions")
