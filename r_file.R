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

##inspect the data
common_na_strings
df_nas <-
  df_samp %>% replace_with_na_all(
              condition = ~.x %in% common_na_strings)


vis_miss(x = df_nas)

View(df_nas)
