#==============================================================
# File description
#==============================================================
# contents: 
#  loads common packages
#  loads custom functions
#  sets custom ggplot themes
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Load packages
#==============================================================
library(stats)
library(tidyr)
library(texreg)
library(tidyverse)
library(RColorBrewer)

#==============================================================
# Directories and data paths
#==============================================================

source("../header_paths_PRIVATE.R")

# ---> this private file defines the following:
#      1. PC directory (wd)
setwd(wd)
#      2. RANYCS school-level master file (SCHMA).
#            A data usage agreement needs to be sign in order to access the file
#            instructions for how to gain access are available at:
#            https://steinhardt.nyu.edu/research-alliance/research/school-level-master-file
#      3. NYC DOE restricted data directory (DOE_wd)

# NYC open data: to crosswalk DOE IDs
# ---> publicly available at https://opendata.cityofnewyork.us/
NYC_open_data = "../data/NYC-open-data-2015-16.csv"

#==============================================================
# Custom functions
#==============================================================

# display frequency table
freq_table <- function(x){
  cnt <- table(x, useNA = "always")
  tbl <- cbind(cnt, round(prop.table(cnt)*100,2))
  colnames(tbl) <- c('Count','Percentage')
  r <- rownames(tbl)
  tbl <- tbl %>%
    as_tibble() %>%
    mutate(Percentage = paste0(Percentage, "%")) %>%
  add_column(r, .before = "Count")
  tbl
}

# sum values across a given ID
agg <- function(dat, dat_var, final_name, dat_ID, ID_name){
  temp_data <- aggregate(dat_var, by = list(dat_ID), FUN = sum, na.rm = TRUE)
  temp_data <- temp_data %>%
    setNames(c(ID_name, final_name))
  new <- inner_join(dat, temp_data, by = ID_name)
  return(new)
}

# mean values across a given ID
agg_mean <- function(dat, dat_var, final_name, dat_ID, ID_name){
  temp_data <- aggregate(dat_var, by = list(dat_ID), FUN = mean, na.rm = TRUE)
  temp_data <- temp_data %>%
    setNames(c(ID_name, final_name))
  new <- inner_join(dat, temp_data, by = ID_name)
  return(new)
}

# describe missing data
pct_missing_variable <- function(dataset){
  dataset %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise(pct_missing = mean(is.na(value))) %>%
    mutate(pct_missing = paste0(round(100 * pct_missing, 2), " %")) %>%
    as_tibble()
}

# summary table of data
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
           Mean = round(Mean, 2),
           S.d. = round(S.d., 2),
           Max. = round(Max., 2),
           Min. = round(Min., 2)) %>%
    rename("% missing" = pct_missing)
}

# additional functions
inv_logit <- stats::binomial()$linkinv
logit <- stats::binomial()$linkfun
vars_NA <- function(x) any(is.na(x))
vars_no_NA <- function(x) any(!is.na(x))

#==============================================================
#                Custom ggplot themes
#==============================================================
grayfill = "#E0E0E0"
graycolor ="#797D7F"
white = "#E7298A"
asian = "#7570B3" 
black = "#66A61E" 
hispanic = "#E6AB02"
other = graycolor

# standard plot
ggtheme =  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", color = "white"), ##F6FCF8"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 12),
        plot.title = element_text(size = 18),
        plot.caption = element_text(size = 8, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(15, 15, 15, 15)),
        axis.title.y = element_text(margin = margin(15, 15, 15, 15)),
        panel.border = element_rect(colour = "black", fill= NA, size=0.1),
        plot.margin=unit(c(.5,.5,.5,.5),"cm"))

# standard plot with legend
ggtheme_legend =  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", color = "white"), ##F6FCF8"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 12),
        plot.title = element_text(size = 18),
        plot.caption = element_text(size = 8, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(15, 15, 15, 15)),
        axis.title.y = element_text(margin = margin(15, 15, 15, 15)),
        panel.border = element_rect(colour = "black", fill= NA, size=0.1),
        plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        legend.title = element_blank(),
        legend.position = "top")

# standard plot with multiple panels
ggtheme_facet =  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", color = "white"), ##F6FCF8"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 12),
        plot.title = element_text(size = 18),
        plot.caption = element_text(size = 8, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(15, 15, 15, 15)),
        axis.title.y = element_text(margin = margin(15, 15, 15, 15)),
        panel.border = element_rect(colour = "black", fill= NA, size=0.1),
        plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid")) 

# nicer histogram
my_hist <- function(var, data, label){
  max = max(var)
  var_name = get_name(var)
  data %>%
    ggplot() +
    geom_histogram(aes_string(x = var_name), bins = 50, 
                   fill = grayfill, 
                   color = graycolor) +
    labs(title = paste0("Histogram of ", label),
         x = label,
         y = "Frequency") +
    ggtheme +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())
}

get_name <- function(x) {
  deparse(substitute(x))
}

# for output display
#options(dplyr.width = Inf)
#options(scipen = 999)
