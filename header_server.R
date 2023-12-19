#==============================================================
# File description
#==============================================================
# contents: 
#  header to run within the server
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Directories and data paths
#==============================================================

source("header_server_paths_PRIVATE.R")

# ---> this private file defines the following:
#      1. DOE_server_wd 
setwd(DOE_server_wd)
#      2. DOE_server_wd_data 
#      3. DOE_server_wd
#      4. DOE_server_wd_code
#      5. DOE_server_wd 
#      5. lib_path
#      6. paths to all datasets used

#==============================================================
# Load packages
#==============================================================

library("crayon", lib.loc = lib_path)
library("tidyr", lib.loc = lib_path)
library("dplyr", lib.loc = lib_path)
library("readr", lib.loc = lib_path)
library("haven", lib.loc = lib_path)
library("cli", lib.loc = lib_path)

#==============================================================
# Custom functions
#==============================================================

freq_table <- function(x){
  cnt <- table(x, useNA = "always")
  tbl <- cbind(cnt, round(prop.table(cnt)*100,2))
  colnames(tbl) <- c('Count','Percentage')
  tbl
}

agg <- function(dat, dat_var, final_name, dat_ID, ID_name){
  temp_data <- aggregate(dat_var, by = list(dat_ID), FUN = sum, na.rm = TRUE)
  temp_data <- temp_data %>%
    setNames(c(ID_name, final_name))
  new <- inner_join(dat, temp_data, by = ID_name)
  return(new)
}

agg_mean <- function(dat, dat_var, final_name, dat_ID, ID_name){
  temp_data <- aggregate(dat_var, by = list(dat_ID), FUN = mean, na.rm = TRUE)
  temp_data <- temp_data %>%
    setNames(c(ID_name, final_name))
  new <- inner_join(dat, temp_data, by = ID_name)
  return(new)
}

pct_missing_variable <- function(dataset){
  dataset %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = paste0(round(100 * pct_missing, 2), " %")) %>%
    as_tibble()
}

summary_of_data <- function(dataset){
  dataset %>% 
    gather(Variable, value) %>%
    mutate(Variable = factor(Variable, 
                             levels = names(dataset))) %>% # add this line to convert the key to a factor
    group_by(Variable) %>%
    summarise(Mean = mean(value, na.rm = T),
              S.d. = sd(value, na.rm = T),
              Min. = min(value, na.rm = T),
              Max. = max(value, na.rm = T),
              pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = round(100 * pct_missing, 1),
           Mean = round(Mean, 1),
           S.d. = round(S.d., 1),
           Max. = round(Max., 1),
           Min. = round(Min., 1)) %>%
    rename("% missing" = pct_missing)
}

########################################## ggplot
grayfill = "#E0E0E0"
graycolor ="#797D7F"