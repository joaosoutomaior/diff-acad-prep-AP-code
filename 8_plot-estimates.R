#==============================================================
# File description
#==============================================================
# contents: 
#  functions to replicate the sensitivity analysis conducted in the paper
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up 
#==============================================================

rm(list=ls())

# additional packages
library(lme4)  
library(texreg)
library(RColorBrewer)
library(broom.mixed)
library(modelr)

# header
source("header_basic.R")
source("header_parameters.R")
useViewer = FALSE

# load data
load(paste0(DOE_wd,"/dataset_predictions.Rdata"))
load(paste0(DOE_wd,"/dataset_estimates-all.Rdata"))
load(paste0(DOE_wd,"/dataset_estimates-sens.Rdata"))

#==============================================================
# Disparities across models 
# Fig.
#==============================================================
mycols <- rep(c(black,asian,hispanic), 4)

p <-dat_estimates %>%
  filter(model_type == "School-level fixed effects") %>%
  ggplot(aes(race, estimate, 
             colour = race)) +
  geom_point(alpha = .8,
             size = 3,
             shape = 16,
             colour = mycols) +
  labs(title = NULL,
       x = "",
       y = "Estimated odds ratio of AP enrollment \n (relative to White students)") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = race), 
                width = 0.2,
                size = 1,
                linetype = "solid",
                alpha = 0.4,
                colour = mycols) +
  geom_hline(yintercept = 1,
             color = "black",
             size = 1,
             linetype = "dashed",
             alpha = 0.5) +
  facet_wrap(~models, 1, 5) +
  ggtheme +
  theme(strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
p  
ggsave("figures/fig_disparate-impact-models.png", width = 25, height = 15, units = "cm")

#==============================================================
# Sensitivity
# Fig.
#==============================================================
mycols <- c(black, asian, hispanic)

p <- dat_estimates_sens %>%
  ggplot(aes(race, estimate, 
             colour = race)) +
  #geom_point(alpha = 1,
   #          size = 3,
    #         shape = 16,
     #        colour = mycols) +
  labs(title = NULL,
       x = "") +
  geom_errorbar(aes(ymin = estimate_lb, ymax = estimate_ub, colour = race), 
                width = 0,
                size = 4,
                linetype = "solid",
                alpha = 0.8,
                colour = mycols) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = race), 
                width = 0.1,
                size = 1,
                linetype = "solid",
                alpha = 0.4,
                colour = mycols) +
  geom_hline(yintercept = 1,
             color = "black",
             size = 1,
             linetype = "dashed",
             alpha = 0.5) +
  scale_y_continuous('Estimated odds ratio of AP enrollment  \n (relative to White students)', limits=c(0.3, 2),
                     labels = function(x) paste0(x),
                     expand = c(0,0)) +
  ggtheme +
  theme(strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
p  
ggsave("figures/fig_sensitivity.png", width = 15, height = 15, units = "cm")
