#==============================================================
# File description
#==============================================================
# contents: 
#  course enrollment model
#  exam passage model
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up
#==============================================================
rm(list=ls())
options(kableExtra.auto_format = FALSE)

# usual header
source("header_basic.R")
source("header_parameters.R")
source("header_plotting-functions.R")

# additional packages
library(remotes)
library(ROCR)
library(xgboost)
library(Matrix)
library(caret)
useViewer = FALSE

# data
load(paste0(DOE_wd,"/dataset_final.Rdata"))
#==============================================================
# Train and test data
#==============================================================

# ---> One option is to train/test on the 2011/2012 student cohorts. This, however, leads to very small training samples. 
#      To improve our models, we train on 90% of our total data and test on the other 10%. 
#      Importantly, note that for the exam passage model, eligible students to train/test 
#      the data are only the ones which enrolled in at least one AP math course and that took at least one AP math exam.

# split train/test data
set.seed(seed)
dat_all$prob <- sample(runif(nrow(dat_all), min = 1, max = 1000))

dat_all <- dat_all %>%
  mutate(
    train_enrollment = case_when(
      prob >= cutoff_value ~ 1,
      TRUE ~ 0),
    test_enrollment = case_when(
      prob < cutoff_value ~ 1,
      TRUE ~ 0),
    train_passage = case_when(
      (AP_math_exam_logical == TRUE & AP_math_enroll_logical == TRUE & prob >= cutoff_value) ~ 1,
      TRUE ~ 0),
    test_passage = case_when(
      (AP_math_exam_logical == TRUE & AP_math_enroll_logical == TRUE & prob < cutoff_value) ~ 1,
      TRUE ~ 0)) #%>%
# if you do not have a train/test split
#  mutate(test_enrollment = 1,
#    test_passage = case_when(
#       (AP_math_exam_logical == TRUE & AP_math_enroll_logical == TRUE & prob >= cutoff_value) ~ 1,
#      TRUE ~ 0))

# full sample
nrow(dat_all)
length(unique(dat_all$ID_school))

# training data (enrollment model).
freq_table(dat_all$train_enrollment)

# testing data (enrollment model).
freq_table(dat_all$test_enrollment)

# training data (passage model).
freq_table(dat_all$train_passage)

# testing data (passage model).
freq_table(dat_all$test_passage)

#==============================================================
# Adjust selected covariates
#==============================================================

# coursework data
dat_all <- dat_all %>%
  mutate(HS_intalg = HS_math_intalg_n_pass_09 + HS_math_intalg_n_pass_10,
         HS_precalc = HS_math_precalc_n_pass_09 + HS_math_precalc_n_pass_10,
         HS_geom = HS_math_geometry_n_pass_09 + HS_math_geometry_n_pass_10,
         HS_alg2 = HS_math_alg2_n_pass_09 + HS_math_alg2_n_pass_10,
         HS_calc = HS_math_calc_n_pass_09 + HS_math_calc_n_pass_10,
         HS_math_precalc = case_when(
           HS_precalc >= 1 ~ 1,
           TRUE ~ 0),
         HS_math_intalg = case_when(
           HS_intalg >= 1 ~ 1,
           TRUE ~ 0),
         HS_math_geom = case_when(
           HS_geom >= 1 ~ 1,
           TRUE ~ 0),
         HS_math_alg2 = case_when(
           HS_alg2 >= 1 ~ 1,
           TRUE ~ 0),
         HS_math_calc = case_when(
           HS_calc >= 1 ~ 1,
           TRUE ~ 0),
         HS_math_adv = case_when(
           (HS_math_alg2 >= 1 & HS_math_geom >= 1) ~ 1,
           TRUE ~ 0))
           
# check distributions
freq_table(dat_all$HS_math_precalc)
freq_table(dat_all$HS_math_intalg)
freq_table(dat_all$HS_math_alg2)
freq_table(dat_all$HS_math_calc)
freq_table(dat_all$HS_math_geom)

# one-hot-encoding for the ID_school variable
dat_schools <- dat_all %>%
  select(ID_school)
  
# define one-hot encoding function
dummy <- dummyVars(" ~ .", data = dat_schools)

# perform one-hot encoding on data frame
dat_schools <- data.frame(predict(dummy, newdata = dat_schools))

# merge back into data
dat_all <- cbind(dat_all, dat_schools)

#==============================================================
# Define model covariates
#==============================================================

covariates <- dat_all %>%
  select(starts_with("MS_") & contains("exam"),
         starts_with("HS_"),
         starts_with("SCH_"),
         -starts_with("HS_math"),
         -contains("exam_grade"),
         -contains("nacad"),
         -contains("status"),
         -contains("performance"),
         -starts_with("BIO_"),
         -starts_with("DIST_"),
         -starts_with("ID"),
         -starts_with("SCH_n_"),
         -starts_with("SCH_RR"),
         -starts_with("SCH_pct"),
         -starts_with("k_"),
         -starts_with("n_"),
         -starts_with("sum_"),
         -starts_with("SCH_cohort"),
         -starts_with("AP"),
         -starts_with("ENR"),
         -ends_with("_11g"),
         -ends_with("_11"),
         -ends_with("_12"),
         -contains("_11_"),
         -contains("_ap"),
         -contains("_factor"),
         -contains("_prior"),
         -HS_4year_grad,
         -HS_transfer,
         -ones,
         -contains("train_"),
         -contains("test_"),
         starts_with("ID_school."),
         -HS_intalg,
         -HS_precalc,
         -HS_geom,
         -HS_alg2,
         -HS_calc,     
         HS_math_adv,
         cohort_2012,
         BIO_hlang_not_english,
         BIO_fr_lunch,
         BIO_age,
         BIO_female,
         SCH_pct_black,
         SCH_pct_ell,
         SCH_pct_female,
         SCH_HS_pct_ap_math_atm,
         SCH_HS_pct_ap_math_pass)

covariates <- as.matrix(covariates)
ncol(covariates)
colnames(covariates)

covariates <- colnames(covariates)

sink("output/covariates.txt")
print(as_tibble(covariates), n = Inf)
sink()

#==============================================================
# Datasets of interest
#==============================================================

# overall data
dat_all <- dat_all %>%
  select(race,
         all_of(covariates),
         contains("train_"),
         contains("test_"),
         AP_math_pass_logical,
         AP_math_enroll_logical,
         AP_math_exam_logical,
         ID_student,
         ID_geo_district,
         ID_district_name,
         ID_school,
         cohort,
         starts_with("SCH_")) %>%
  mutate(across(.cols = all_of(covariates),
                .fns = ~as.numeric(.x)))

# create train and test data
dat_train_enrollment <- dat_all %>%
  filter(train_enrollment == 1)
dat_test_enrollment <- dat_all %>%
  filter(test_enrollment == 1)
dat_train_passage <- dat_all %>%
  filter(train_passage == 1)
dat_test_passage <- dat_all %>%
  filter(test_passage == 1)

glimpse(dat_all)

#==============================================================
# Missing data
#==============================================================

# missing data for the full sample
x = pct_missing_variable(dat_all)
x %>% arrange(desc(pct_missing))

# input missing values with variable means
dat <- as.data.frame(dat_all)
impute_vars <- dat %>% 
  names()
for(i in impute_vars) {
  dat[is.na(dat[,i]), i] <- mean(dat[,i], na.rm = TRUE)
}
dat_all <- as_tibble(dat)

# check missing values again
x = pct_missing_variable(dat_all)
x %>% arrange(desc(pct_missing))

# missing data for training data (enrollment model)
dat <- as.data.frame(dat_train_enrollment)

# input missing values with variable means
impute_vars <- dat %>% 
  names()
for(i in impute_vars) {
  dat[is.na(dat[,i]), i] <- mean(dat[,i], na.rm = TRUE)
}
dat_train_enrollment<- as_tibble(dat)

# missing data for the testing data (enrollment model)
dat <- as.data.frame(dat_test_enrollment)

# input missing values with variable means
impute_vars <- dat %>% 
  names()
for(i in impute_vars) {
  dat[is.na(dat[,i]), i] <- mean(dat[,i], na.rm = TRUE)
}
dat_test_enrollment<- as_tibble(dat)

# missing data for the training data (exam passage model)
dat <- as.data.frame(dat_train_passage)

# input missing values with variable means
impute_vars <- dat %>% 
  names()
for(i in impute_vars) {
  dat[is.na(dat[,i]), i] <- mean(dat[,i], na.rm = TRUE)
}
dat_train_passage <- as_tibble(dat)

# missing data for the testing data (exam passage model)
dat <- as.data.frame(dat_test_passage)

# input missing values with variable means
impute_vars <- dat %>% 
  names()
for(i in impute_vars) {
  dat[is.na(dat[,i]), i] <- mean(dat[,i], na.rm = TRUE)
}
dat_test_passage <- as_tibble(dat)

#==============================================================
# Train/test table
# (Table 1)
#==============================================================

# load additional package
library(kableExtra)

# describe each of the samples of interest
sink("output/train-test-samples.txt")

paste("Data summary")

paste("Train: enrollment model")
t <- dat_all %>%
  group_by(AP_math_enroll_logical, AP_math_exam_logical) %>%
  summarise(N = n()) %>%
  mutate(total = nrow(dat_all),
         "Pct of cohort" = paste0(100 * round(N / total, 3), "%")) %>%
  select(-total) 
t <- kbl(t, "latex", booktabs = T, align = "l")
t

paste("Description of train/test samples")

paste("Train: enrollment model")
t <- dat_train_enrollment %>%
  group_by(AP_math_enroll_logical) %>%
  summarise(N = n()) %>%
  mutate(total = sum(N),
         "Pct of cohort" = paste0(100 * round(N / total, 3), "%")) %>%
  select(-total) 
t <- kbl(t, "latex", booktabs = T, align = "l")
t

paste("Train: passage model")
t <- dat_train_passage %>%
  group_by(AP_math_enroll_logical, AP_math_exam_logical,  AP_math_pass_logical) %>%
  summarise(N = n()) %>%
  mutate(total = sum(N),
         "Pct of cohort" = paste0(100 * round(N / total, 3), "%")) %>%
  select(-total) 
t <- kbl(t, "latex", booktabs = T, align = "l")
t

paste("Test: enrollment model")
t <- dat_test_enrollment %>%
  group_by(AP_math_enroll_logical) %>%
  summarise(N = n()) %>%
  mutate(total = sum(N),
         "Pct of cohort" = paste0(100 * round(N / total, 3), "%")) %>%
  select(-total) 
t <- kbl(t, "latex", booktabs = T, align = "l")
t

paste("Test: passage model")
t <- dat_test_passage %>%
  group_by(AP_math_enroll_logical, AP_math_exam_logical, AP_math_pass_logical) %>%
  summarise(N = n()) %>%
  mutate(total = sum(N),
         "Pct of cohort" = paste0(100 * round(N / total, 3), "%")) %>%
  select(-total) 
t <- kbl(t, "latex", booktabs = T, align = "l")
t

# close output
sink()

#==============================================================
# Objects needed to run the xgboost models
#==============================================================

# labels
label_train_enrollment = dat_train_enrollment[,"AP_math_enroll_logical"] == T
label_test_enrollment = dat_test_enrollment[,"AP_math_enroll_logical"] == T
label_enrollment = dat_all[,"AP_math_enroll_logical"] == T

label_train_passage = dat_train_passage[,"AP_math_pass_logical"] == T
label_test_passage = dat_test_passage[,"AP_math_pass_logical"] == T
label_passage = dat_all[,"AP_math_pass_logical"] == T

# covariates
cov_train_enrollment <- dat_train_enrollment %>%
  select(all_of(covariates)) %>%
  data.matrix()
cov_test_enrollment <- dat_test_enrollment %>%
  select(all_of(covariates)) %>%
  data.matrix()
cov_enrollment <- dat_all %>%
  select(all_of(covariates)) %>%
  data.matrix()

cov_train_passage <- dat_train_passage %>%
  select(all_of(covariates)) %>%
  data.matrix()
cov_test_passage <- dat_test_passage %>%
  select(all_of(covariates)) %>%
  data.matrix()
cov_passage <- dat_all %>%
  select(all_of(covariates)) %>%
  data.matrix()

# xgboost objects
xgb_train_enrollment <- xgb.DMatrix(data = cov_train_enrollment, label= label_train_enrollment)
xgb_test_enrollment <- xgb.DMatrix(data = cov_test_enrollment, label= label_test_enrollment)
xgb_enrollment <- xgb.DMatrix(data = cov_enrollment, label= label_enrollment)

xgb_train_passage <- xgb.DMatrix(data = cov_train_passage, label= label_train_passage)
xgb_test_passage <- xgb.DMatrix(data = cov_test_passage, label= label_test_passage)
xgb_passage <- xgb.DMatrix(data = cov_passage, label= label_passage)

#==============================================================
# Grid search with 5-fold cross validation 
# to choose the enrollment model
#==============================================================

# parameters chosen for a classification model
params <- list(booster = "gbtree", 
               objective = "binary:logistic")

# grid search function over selected parameters
grid_search_enrollment_model <- function(max_depth_range,
                                         max_delt_step_range,
                                         min_child_weight_range,
                                         eta_range,
                                         gamma_range) {
  
  # define a named list of parameter values to search over
  parameters_to_search <- list(max_depth = max_depth_range,
                               max_delt_step = max_delt_step_range,
                               min_child_weight = min_child_weight_range, 
                               eta = eta_range,
                               gamma = gamma_range) %>% 
    cross_df() # Convert to data frame grid
  
  # define data to store results  
  grid_search_data <- as.data.frame(matrix(nrow = nrow(parameters_to_search), 
                                           ncol = 7))
  names(grid_search_data)[1] <- "combination_number"
  names(grid_search_data)[2] <- "max_depth"
  names(grid_search_data)[3] <- "max_delt_step"
  names(grid_search_data)[4] <- "min_child_weight"
  names(grid_search_data)[5] <- "eta"
  names(grid_search_data)[6] <- "gamma"
  names(grid_search_data)[7] <- "outcome"

  # apply the sens function over all combinations of parameter values
  for(i in 1:nrow(parameters_to_search)){
    set.seed(seed)
    model <- xgb.cv(params = params,
                    data = xgb_train_enrollment, 
                    nround = 100,
                    max_depth  = parameters_to_search$max_depth[[i]],
                    max_delt_step = parameters_to_search$max_delt_step[[i]],
                    min_child_weight = parameters_to_search$min_child_weight[[i]],
                    eta = parameters_to_search$eta[[i]],
                    gamma = parameters_to_search$gamma[[i]],
                    eval_metric = "auc",
                    nfold = 5)
    
    best_auc <- as_tibble(model$evaluation_log) %>%
      arrange(desc(test_auc_mean))
    
    grid_search_data[i, 1] <- i
    grid_search_data[i, 2] <- parameters_to_search$max_depth[[i]]
    grid_search_data[i, 3] <- parameters_to_search$max_delt_step[[i]]
    grid_search_data[i, 4] <- parameters_to_search$min_child_weight[[i]]
    grid_search_data[i, 5] <- parameters_to_search$eta[[i]]
    grid_search_data[i, 6] <- parameters_to_search$gamma[[i]]
    grid_search_data[i, 7] <- best_auc[[1, "test_auc_mean"]]
  }
  
  # organize results
  grid_search_data <- as_tibble(grid_search_data)
  return(grid_search_data)
}

#==============================================================
# Grid search with 5-fold cross validation 
# to choose the passage model
#==============================================================

# grid search function over selected parameters
grid_search_passage_model <- function(max_depth_range,
                                      max_delt_step_range,
                                      min_child_weight_range,
                                      eta_range,
                                      gamma_range) {
  
  # define a named list of parameter values to search over
  parameters_to_search <- list(max_depth = max_depth_range,
                               max_delt_step = max_delt_step_range,
                               min_child_weight = min_child_weight_range, 
                               eta = eta_range,
                               gamma = gamma_range) %>% 
    cross_df() # Convert to data frame grid
  
  # define data to store results  
  grid_search_data <- as.data.frame(matrix(nrow = nrow(parameters_to_search), 
                                           ncol = 7))
  names(grid_search_data)[1] <- "combination_number"
  names(grid_search_data)[2] <- "max_depth"
  names(grid_search_data)[3] <- "max_delt_step"
  names(grid_search_data)[4] <- "min_child_weight"
  names(grid_search_data)[5] <- "eta"
  names(grid_search_data)[6] <- "gamma"
  names(grid_search_data)[7] <- "outcome"

  # apply the sens function over all combinations of parameter values
  for(i in 1:nrow(parameters_to_search)){
    set.seed(seed)
    model <- xgb.cv(params = params,
                    data = xgb_train_passage, 
                    nround = 100,
                    max_depth  = parameters_to_search$max_depth[[i]],
                    max_delt_step = parameters_to_search$max_delt_step[[i]],
                    min_child_weight = parameters_to_search$min_child_weight[[i]],
                    eta = parameters_to_search$eta[[i]],
                    gamma = parameters_to_search$gamma[[i]],
                    eval_metric = "auc",
                    nfold = 5)
    
    best_auc <- as_tibble(model$evaluation_log) %>%
      arrange(desc(test_auc_mean))
    
    grid_search_data[i, 1] <- i
    grid_search_data[i, 2] <- parameters_to_search$max_depth[[i]]
    grid_search_data[i, 3] <- parameters_to_search$max_delt_step[[i]]
    grid_search_data[i, 4] <- parameters_to_search$min_child_weight[[i]]
    grid_search_data[i, 5] <- parameters_to_search$eta[[i]]
    grid_search_data[i, 6] <- parameters_to_search$gamma[[i]]
    grid_search_data[i, 7] <- best_auc[[1, "test_auc_mean"]]
  }
  
  # organize results
  grid_search_data <- as_tibble(grid_search_data)
  return(grid_search_data)
}

#==============================================================
# Grid search to find best models (takes a long time)
#==============================================================
# 
# # run grid search
# results_enrollment <- grid_search_enrollment_model(max_depth_range = c(1, 3, 6, 9),
#                                                    max_delt_step_range = 0,
#                                                    min_child_weight_range = c(1, 3, 6, 9),
#                                                    eta_range = c(0.1, 0.3, 0.6, 0.9), 
#                                                    gamma_range = c(0, 2, 4))
# 
# # run grid search
# results_passage <- grid_search_passage_model(max_depth_range = c(1, 3, 6, 9),
#                                              max_delt_step_range = 0,
#                                              min_child_weight_range = c(1, 3, 6, 9),
#                                              eta_range = c(0.1, 0.3, 0.6, 0.9), 
#                                              gamma_range = c(0, 2, 4))
# 
# sink("output/enrollment-model_best.txt")
# results_enrollment <- results_enrollment %>%
#     arrange(desc(outcome))
# results_enrollment[1,]
# sink()
# 
# sink("output/passage-model_best.txt")
# results_passage <- results_passage %>%
#   arrange(desc(outcome))
# results_passage[1,]
# sink()

#==============================================================
# Enrollment model
#==============================================================

# run best model
set.seed(seed)
params <- list(booster = "gbtree", 
               objective = "binary:logistic")
enrollment_model <- xgb.train(params = params,
                           data = xgb_train_enrollment, # the data
                           nround = 500, # max number of boosting iterations
                           max_depth = 9, # higher value -> more likely to overfit
                           max_delt_step = 0, # (0, Inf), the larger, the more conservative (often not needed)
                           min_child_weight = 3, # (0,Inf)the larger, the more conservative
                           eta = 0.1, # (0,1), the larger, the more conservative
                           gamma = 2, # (0,Inf), the larger, the more conservative
                           eval_metric = "auc",
                           early_stopping_rounds = 50,
                           watchlist = list(train = xgb_train_enrollment, val = xgb_test_enrollment)) 

# save model                           
# saveRDS(enrollment_model, "output/fitted_enrollment_model.rds")

# quick check on performance and key variables (enrollment model)
sink("output/enrollment-model_importance-matrix.txt")

names <- dimnames(cov_train_enrollment)[[2]]
importance_matrix <- xgb.importance(names, model = enrollment_model)
print(as_tibble(importance_matrix), n = Inf)
sink()

#==============================================================
# Passage model
#==============================================================

# run best model
set.seed(seed)
params <- list(booster = "gbtree", 
               objective = "binary:logistic")
passage_model <- xgb.train(params = params,
                           data = xgb_train_passage, # the data
                           nround = 500, # max number of boosting iterations
                           max_depth = 3, # higher value -> more likely to overfit
                           max_delt_step = 0, # (0, Inf), the larger, the more conservative (often not needed)
                           min_child_weight = 1, # (0,Inf)the larger, the more conservative
                           eta = 0.3, # (0,1), the larger, the more conservative
                           gamma = 2, # (0,Inf), the larger, the more conservative
                           eval_metric = "auc",
                           early_stopping_rounds = 50,
                           watchlist = list(train = xgb_train_passage, val = xgb_test_passage))
                           
# save model                           
# saveRDS(passage_model, "output/fitted_passage_model.rds")                           

# quick check on performance and key variables (passage model)
sink("output/passage-model_importance-matrix.txt")

names <- dimnames(cov_train_passage)[[2]]
importance_matrix <- xgb.importance(names, model = passage_model)
print(as_tibble(importance_matrix), n = Inf)
sink()

#==============================================================
# Apply models to the entire sample
#==============================================================

# enrollment model
y <- as.data.frame(predict(enrollment_model, xgb_enrollment))
prob_enrollment <- y[,1]
dat_all$prob_enrollment <- prob_enrollment

# passage model
z <- as.data.frame(predict(passage_model, xgb_passage))
prob_passage <- z[,1]
dat_all$prob_passage <- prob_passage

# save predictions
dat_all <- dat_all %>%
  mutate(logit_prob_passage = logit(prob_passage),
         logit_prob_enrollment = logit(prob_enrollment),
         prob_enrollment_pct = 100 * prob_enrollment,
         prob_passage_pct = 100 * prob_passage)

# select sample of interest
# (this was useful when our data of interest was not the full data)
dat_interest <- dat_all 

dat_test_enrollment <- dat_interest %>%
  filter(test_enrollment == 1)

dat_test_passage <- dat_interest %>%
  filter(test_passage == 1)

#==============================================================
# Model checks (on the test data)
#==============================================================

# predicted vs true values (pass rates)
pass_rate_df <- dat_test_passage %>%
  group_by(race) %>%
  summarize(True = 100 * mean(AP_math_pass_logical),
            Estimated = 100 * mean(prob_passage),) %>%
  pivot_longer(c("True", "Estimated")) %>%
  mutate(type = "Exam passage")

# predicted vs true values (enrollment rates)
enroll_rate_df <- dat_test_enrollment %>%
  group_by(race) %>%
      summarize(Estimated = 100 * mean(prob_enrollment),
                True = 100 * mean(AP_math_enroll_logical)) %>%
  pivot_longer(c("True", "Estimated")) %>%
  mutate(type = "Course enrollment")

# combine results
y <- rbind(enroll_rate_df, pass_rate_df)

sink("output/model-checks.txt")
print(y, n = Inf)
sink()

#==============================================================
# Make plots
#==============================================================

# plot results for enrollment
p1_enrollment <- y %>%
  filter(type == "Course enrollment") %>%
  ggplot() +
  geom_point(aes(race, value, color = name, shape = name),
             alpha = 0.8, 
             size = 3) +
  scale_color_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(1,16)) +
  scale_y_continuous('Rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0)) +
  scale_x_discrete(name ="") +
  ggtheme +
    theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
# plot results for passage
p1_passage <- y %>%
  filter(type == "Exam passage") %>%
  ggplot() +
  geom_point(aes(race, value, color = name, shape = name),
             alpha = 0.8, 
             size = 3) +
  scale_color_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(1,16)) +
  scale_y_continuous('Rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0)) +
  scale_x_discrete(name ="") +
  ggtheme +
    theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))

#==============================================================
# Model checks (on the test data) across cohorts
#==============================================================

# true rates across cohorts (passage model)
pass_rate_df <- dat_interest %>%
  filter(AP_math_enroll_logical == TRUE,
         AP_math_exam_logical == TRUE) %>%
  group_by(cohort, race) %>%
  summarize(True = 100 * mean(AP_math_pass_logical)) %>%
  pivot_longer(c("True")) %>%
  mutate(type = "Exam passage")

# true rates across cohorts (enrollment model)
enroll_rate_df <- dat_interest %>%
  group_by(cohort, race) %>%
  summarize(True = 100 * mean(AP_math_enroll_logical)) %>%
  pivot_longer(c("True")) %>%
  mutate(type = "Course enrollment")

# combine results
y <- rbind(enroll_rate_df, pass_rate_df) %>%
  mutate(cohort = as.character(cohort))

sink("output/model-checks_2011-vs-2012.txt")
print(y, n = Inf)
sink()

#==============================================================
# Print plots
#==============================================================

p <- y %>%
  ggplot() +
  geom_point(aes(race, value, 
                 color = cohort,
                 shape = cohort),
             alpha = 0.8, 
             size = 3) +
  scale_color_manual(values = c("red", "black")) +
  scale_shape_manual(values = c(16,1)) +
  scale_y_continuous('Rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0)) +
  scale_x_discrete(name = "") +
  facet_grid(~type) +
  ggtheme +
    theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
p
ggsave("figures/fig_passage-model_checks_2011-vs-2012.png", width = 15, height = 15, units = "cm")

#==============================================================
# Model performance across schools
#==============================================================

# school-level true/predicted enrollment rates
dat_test_enrollment <- agg_mean(dat_test_enrollment, dat_test_enrollment$prob_enrollment_pct, "SCH_est_enroll_rate", dat_test_enrollment$ID_school, "ID_school")
dat_test_enrollment <- agg_mean(dat_test_enrollment, dat_test_enrollment$AP_math_enroll_logical, "SCH_true_enroll_rate", dat_test_enrollment$ID_school, "ID_school")

# school-level true/predicted passage rates
dat_test_passage <- agg_mean(dat_test_passage, dat_test_passage$prob_passage_pct, "SCH_est_pass_rate", dat_test_passage$ID_school, "ID_school")
dat_test_passage <- agg_mean(dat_test_passage, dat_test_passage$AP_math_pass_logical, "SCH_true_pass_rate", dat_test_passage$ID_school, "ID_school")

# plot of true/predicted enrollment rates within schools
dat_schools1 <- dat_test_enrollment %>%
  mutate(SCH_true_enroll_rate = 100 * SCH_true_enroll_rate,) %>%
  select(SCH_true_enroll_rate,
         SCH_est_enroll_rate,
         SCH_cohort_HS_size,
         ID_school) %>%
  rename("Course enrollment" = SCH_est_enroll_rate) %>%
  pivot_longer(cols = c("Course enrollment"), 
               names_to = "type", values_to = "est") %>%
  pivot_longer(cols = starts_with("SCH_tr"),
               names_to = "type2", values_to = "true") %>%
  
  distinct(ID_school, .keep_all = TRUE)


# plot of true/predicted passage rates within schools
dat_schools2 <- dat_test_passage %>%
  mutate(SCH_true_pass_rate = 100 * SCH_true_pass_rate) %>%
  select(SCH_true_pass_rate,
         SCH_est_pass_rate,
         SCH_cohort_HS_size,
         ID_school) %>%
  rename("Exam passage" = SCH_est_pass_rate,) %>%
  pivot_longer(cols = c("Exam passage"), 
               names_to = "type", values_to = "est") %>%
  pivot_longer(cols = starts_with("SCH_tr"),
               names_to = "type2", values_to = "true") %>%
  distinct(ID_school, .keep_all = TRUE)

# combine results
dat_schools <- rbind(dat_schools1, dat_schools2)

#==============================================================
# Print plots
#==============================================================

# plot for enrollment
p2_enrollment <- dat_schools %>%
  filter(type == "Course enrollment") %>%
  ggplot() + 
  geom_point(aes(x = true, 
                 y = est,
                 size = SCH_cohort_HS_size), 
             alpha = 0.5, 
             fill = grayfill,
             colour = "black",
             stroke = 1,
             shape = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', colour = "black", lwd = 0.8) +
  scale_y_continuous('Estimated rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.05,0.05)) +
  scale_x_continuous('True rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.05,0.05)) +
  scale_size_area(max_size = 15) +
  ggtheme +
  theme(legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
# plot for passage
p2_passage <- dat_schools %>%
  filter(type == "Exam passage") %>%
  ggplot() + 
  geom_point(aes(x = true, 
                 y = est,
                 size = SCH_cohort_HS_size), 
             alpha = 0.5, 
             fill = grayfill,
             colour = "black",
             stroke = 1,
             shape = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', colour = "black", lwd = 0.8) +
  scale_y_continuous('Estimated rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.05,0.05)) +
  scale_x_continuous('True rate', limits=c(0,100), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.05,0.05)) +
  scale_size_area(max_size = 15) +
  ggtheme +
  theme(legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))

#==============================================================
# Save combined plots
#==============================================================

plot_grid(p1_enrollment, p2_enrollment, labels = "AUTO", 
          label_y = 0.95, 
          label_x = 0.05, 
          label_size = 15)
ggsave("figures/fig_enrollment-model_checks.png", width = 25, height = 15, units = "cm")

plot_grid(p1_passage, p2_passage, labels = "AUTO", 
          label_y = 0.95, 
          label_x = 0.05, 
          label_size = 15)
ggsave("figures/fig_passage-model_checks.png", width = 25, height = 15, units = "cm")

#==============================================================
# Very low true exam-passage rates?
#==============================================================

# ---> What is happening with schools with very low true exam-passage rates? 
#      (note, below, that there are schools with a 0% exam-passage rate 
#      among those who enroll AND take the exam).

# brief view of passage rates across schools
hist(dat_schools2$true, breaks = 100)
summary(dat_schools2$true)

# get schools with 0% exam passage rates
low_passage_schools <- dat_schools2 %>%
  filter(true == 0) %>%
  distinct(ID_school)

# get IDs of these schools
low_passage_schools_IDs <- low_passage_schools$ID_school

# number of schools with 0% passage rates
nrow(low_passage_schools)

# compare characteristics of these schools with all other schools (calling "0% passage" vs "other" schools).
high_passage_schools <- dat_interest %>%
  filter(!(ID_school %in% low_passage_schools_IDs)) %>%
  distinct(ID_school, .keep_all = TRUE) %>%
  select(starts_with("SCH_")) %>%
  mutate(type = "other schools")

low_passage_schools <- dat_interest %>%
  filter(ID_school %in% low_passage_schools_IDs) %>%
  distinct(ID_school, .keep_all = TRUE) %>%
  select(starts_with("SCH_")) %>%
  mutate(type = "0% passage schools")

y <- rbind(low_passage_schools, high_passage_schools)

p1 <- y %>%
  ggplot(aes(x = SCH_ap_math_enrollment, color = type)) %>% + 
  geom_histogram(bins = 50, fill="white") +
  ggtheme_legend +
  xlab("Number of students enrolled in \nat least one AP math course")

p2 <- y %>%
  ggplot(aes(x = SCH_cohort_HS_size, color = type)) %>% + 
  geom_histogram(bins = 50, fill="white") +
  ggtheme_legend +
  xlab("Size of the high school cohort") 

# print if needed
#library(cowplot)
#plot_grid(p2, p1, labels = "AUTO", label_x = 0.1, hjust = 0, label_size=15)
#ggsave("figures/0-pct-passage-schools.png", width = 25, height = 15, units = "cm")

#==============================================================
# Calculate AUC (for the enrollment model)
#==============================================================

sink("output/enrollment-model_AUCs.txt")

#------------------ all
ROCR_pred <- prediction(dat_test_enrollment$prob_enrollment, dat_test_enrollment$AP_math_enroll_logical)
ROCR_perf <- performance(ROCR_pred, measure = "tpr", x.measure = "fpr")
ROCR_AUC <- performance(ROCR_pred, measure = "auc")
AUC <- ROCR_AUC@y.values[[1]]
paste0("All = ", AUC)

#------------------ White students
testing_dat_white <- dat_test_enrollment %>%
    filter(race == "White")

# ROC and AUC
ROCR_pred_white <- prediction(testing_dat_white$prob_enrollment, testing_dat_white$AP_math_enroll_logical)
ROCR_perf_white <- performance(ROCR_pred_white, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_white <- performance(ROCR_pred_white, measure = "auc")
AUC_white <- ROCR_AUC_white@y.values[[1]]
paste0("Whites = ", AUC_white)

#------------------ Black students
testing_dat_black <- dat_test_enrollment %>%
    filter(race == "Black")

# ROC and AUC
ROCR_pred_black <- prediction(testing_dat_black$prob_enrollment, testing_dat_black$AP_math_enroll_logical)
ROCR_perf_black <- performance(ROCR_pred_black, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_black <- performance(ROCR_pred_black, measure = "auc")
AUC_black <- ROCR_AUC_black@y.values[[1]]
paste0("Blacks = ", AUC_black)

#------------------ Asian students
testing_dat_asian <- dat_test_enrollment %>%
    filter(race == "Asian")

# ROC and AUC
ROCR_pred_asian <- prediction(testing_dat_asian$prob_enrollment, testing_dat_asian$AP_math_enroll_logical)
ROCR_perf_asian <- performance(ROCR_pred_asian, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_asian <- performance(ROCR_pred_asian, measure = "auc")
AUC_asian <- ROCR_AUC_asian@y.values[[1]]
paste0("Asians = ", AUC_asian)

#------------------ Hispanic students
testing_dat_hispanic <- dat_test_enrollment %>%
    filter(race == "Hispanic")

# ROC and AUC
ROCR_pred_hispanic <- prediction(testing_dat_hispanic$prob_enrollment, testing_dat_hispanic$AP_math_enroll_logical)
ROCR_perf_hispanic <- performance(ROCR_pred_hispanic, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_hispanic <- performance(ROCR_pred_hispanic, measure = "auc")
AUC_hispanic <- ROCR_AUC_hispanic@y.values[[1]]
paste0("Hispanics = ", AUC_hispanic)

sink()

#==============================================================
# Plot TP / FP rates (enrollment model)
#==============================================================

png("figures/fig_enrollment-model_ROC-testing.png")
par(mfrow=c(1,1),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    lwd = 1, 
    cex.main  = 1.2)
plot_roc_auc(ROCR_perf, AUC," ", "black")
mtext("False positive rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("True positive rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#------------------ By race
# scale_color_manual(values = c("White" = c1, "Black" = c3, "Hispanic" = c4, "Asian" = c2)) 
png("figures/fig_enrollment-model_ROC-testing_by-race.png", height = 600, width = 600)
par(mfrow=c(2,2),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    lwd = 1, 
    cex.main  = 1)
plot_roc_auc(ROCR_perf_white, AUC_white,"(a) White students", c1)
plot_roc_auc(ROCR_perf_black, AUC_black,"(b) Black students", c3)
plot_roc_auc(ROCR_perf_asian, AUC_asian,"(c) Asian students", c2)
plot_roc_auc(ROCR_perf_hispanic, AUC_hispanic, "(d) Hispanic students", c4)
mtext("False positive rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("True positive rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#==============================================================
# Plot precision / recall (enrollment model)
#==============================================================

ROCR_perf <- performance(ROCR_pred, measure = "prec", x.measure = "rec")
png("figures/fig_enrollment-model_precision-recall-testing.png")
par(mfrow=c(1,1),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    lwd = 1, 
    cex.main  = 1.2)
plot_pr_auc(ROCR_perf, AUC," ", "black")
mtext("Recall rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("Precision rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#------------------ By race

ROCR_perf_white <- performance(ROCR_pred_white,measure = "prec", x.measure = "rec")
ROCR_perf_black <- performance(ROCR_pred_black,measure = "prec", x.measure = "rec")
ROCR_perf_asian <- performance(ROCR_pred_asian,measure = "prec", x.measure = "rec")
ROCR_perf_hispanic <- performance(ROCR_pred_hispanic,measure = "prec", x.measure = "rec")

png("figures/fig_enrollment-model_precision-recall-testing_by-race.png", height = 600, width = 600)
par(mfrow=c(2,2),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    fg = "white",
    lwd = 1, 
    cex.main  = 1)
plot_pr_auc(ROCR_perf_white, AUC_white,"(a) White students", c1)
plot_pr_auc(ROCR_perf_black, AUC_black,"(b) Black students", c3)
plot_pr_auc(ROCR_perf_asian, AUC_asian,"(c) Asian students", c2)
plot_pr_auc(ROCR_perf_hispanic, AUC_hispanic, "(d) Hispanic students", c4)
mtext("Recall rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("Precision rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#==============================================================
#  Calculate ROC and AUC (for the exam passage model)
#==============================================================

sink("output/passage-model_AUCs.txt")

#------------------ all
ROCR_pred <- prediction(dat_test_passage$prob_passage, dat_test_passage$AP_math_pass_logical)
ROCR_perf <- performance(ROCR_pred, measure = "tpr", x.measure = "fpr")
ROCR_AUC <- performance(ROCR_pred, measure = "auc")
AUC <- ROCR_AUC@y.values[[1]]
paste0("All = ", AUC)

#------------------ White students
testing_dat_white <- dat_test_passage %>%
    filter(race == "White")

# ROC and AUC
ROCR_pred_white <- prediction(testing_dat_white$prob_passage, testing_dat_white$AP_math_pass_logical)
ROCR_perf_white <- performance(ROCR_pred_white, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_white <- performance(ROCR_pred_white, measure = "auc")
AUC_white <- ROCR_AUC_white@y.values[[1]]
paste0("Whites = ", AUC_white)

#------------------ Black students
testing_dat_black <- dat_test_passage %>%
    filter(race == "Black")

# ROC and AUC
ROCR_pred_black <- prediction(testing_dat_black$prob_passage, testing_dat_black$AP_math_pass_logical)
ROCR_perf_black <- performance(ROCR_pred_black, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_black <- performance(ROCR_pred_black, measure = "auc")
AUC_black <- ROCR_AUC_black@y.values[[1]]
paste0("Blacks = ", AUC_black)

#------------------ Asian students
testing_dat_asian <- dat_test_passage %>%
    filter(race == "Asian")

# ROC and AUC
ROCR_pred_asian <- prediction(testing_dat_asian$prob_passage, testing_dat_asian$AP_math_pass_logical)
ROCR_perf_asian <- performance(ROCR_pred_asian, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_asian <- performance(ROCR_pred_asian, measure = "auc")
AUC_asian <- ROCR_AUC_asian@y.values[[1]]
paste0("Asians = ", AUC_asian)

#------------------ Hispanic students
testing_dat_hispanic <- dat_test_passage %>%
    filter(race == "Hispanic")

# ROC and AUC
ROCR_pred_hispanic <- prediction(testing_dat_hispanic$prob_passage, testing_dat_hispanic$AP_math_pass_logical)
ROCR_perf_hispanic <- performance(ROCR_pred_hispanic, measure = "tpr", x.measure = "fpr")

# Calculate AUC
ROCR_AUC_hispanic <- performance(ROCR_pred_hispanic, measure = "auc")
AUC_hispanic <- ROCR_AUC_hispanic@y.values[[1]]
paste0("Hispanics = ", AUC_hispanic)
sink()

#==============================================================
# Plot TP / FP rates (passage model)
#==============================================================

png("figures/fig_passage-model_ROC-testing.png")
par(mfrow=c(1,1),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    lwd = 1, 
    cex.main  = 1.2)
plot_roc_auc(ROCR_perf, AUC," ", "black")
mtext("False positive rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("True positive rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#------------------ By race
# scale_color_manual(values = c("White" = c1, "Black" = c3, "Hispanic" = c4, "Asian" = c2)) 
png("figures/fig_passage-model_ROC-testing_by-race.png", height = 600, width = 600)
par(mfrow=c(2,2),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    lwd = 1, 
    cex.main  = 1)
plot_roc_auc(ROCR_perf_white, AUC_white,"(a) White students", c1)
plot_roc_auc(ROCR_perf_black, AUC_black,"(b) Black students", c3)
plot_roc_auc(ROCR_perf_asian, AUC_asian,"(c) Asian students", c2)
plot_roc_auc(ROCR_perf_hispanic, AUC_hispanic, "(d) Hispanic students", c4)
mtext("False positive rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("True positive rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#==============================================================
# Plot precision / recall (passage model)
#==============================================================

ROCR_perf <- performance(ROCR_pred, measure = "prec", x.measure = "rec")
png("figures/fig_passage-model_precision-recall-testing.png")
par(mfrow=c(1,1),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    lwd = 1, 
    cex.main  = 1.2)
plot_pr_auc(ROCR_perf, AUC," ", "black")
mtext("Recall rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("Precision rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#------------------ By race

ROCR_perf_white <- performance(ROCR_pred_white,measure = "prec", x.measure = "rec")
ROCR_perf_black <- performance(ROCR_pred_black,measure = "prec", x.measure = "rec")
ROCR_perf_asian <- performance(ROCR_pred_asian,measure = "prec", x.measure = "rec")
ROCR_perf_hispanic <- performance(ROCR_pred_hispanic,measure = "prec", x.measure = "rec")

png("figures/fig_passage-model_precision-recall-testing_by-race.png", height = 600, width = 600)
par(mfrow=c(2,2),
    mar = c(3,3,3,3), 
    oma = c(3,3,0,3),
    cex.axis = 0.8,
    cex.lab = 1.3,
    fg = "white",
    lwd = 1, 
    cex.main  = 1)
plot_pr_auc(ROCR_perf_white, AUC_white,"(a) White students", c1)
plot_pr_auc(ROCR_perf_black, AUC_black,"(b) Black students", c3)
plot_pr_auc(ROCR_perf_asian, AUC_asian,"(c) Asian students", c2)
plot_pr_auc(ROCR_perf_hispanic, AUC_hispanic, "(d) Hispanic students", c4)
mtext("Recall rate",
      side=1,
      line=0,
      outer=T,
      cex=1,
      col = "black")
mtext("Precision rate",
      side=2,
      line=0,
      outer=T,
      cex=1,
      col = "black")
dev.off()

#=============================================================
# Save results
#=============================================================

# ---> note that data of interest is "dat_interest"

# adjust outcome variables for the logit models
dat_interest <- dat_interest %>%
  mutate(AP_math_pass_factor = as.factor(AP_math_pass_logical),
         AP_math_enroll_factor = as.factor(AP_math_enroll_logical),
         AP_math_exam_factor = as.factor(AP_math_exam_logical))

# make sure "White" is the reference category
dat_interest <- dat_interest %>%
  mutate(race = relevel(race, ref = "White"))

# transform dummies into factors
dat_interest <- dat_interest %>%
  mutate(BIO_female = as.factor(BIO_female),
         BIO_fr_lunch = as.factor(BIO_fr_lunch),
         BIO_hlang_not_english = as.factor(BIO_hlang_not_english))

# save
save(dat_interest, file = paste0(DOE_wd,"/dataset_predictions.Rdata"))

load(paste0(DOE_wd,"/dataset_predictions.Rdata"))

#=============================================================
# Plot predicted probabilities (passage model)
#=============================================================

# remove race = "other" from plots.
dat_interest <- dat_interest %>%
  filter(race != "Other") %>%
  mutate(race = relevel(race, ref = "White"))

# compute means
mu <- dat_interest %>%
  select(prob_passage_pct, race) %>%
  group_by(race) %>%
  summarise(grp.mean = mean(prob_passage_pct))

sink("output/acad-prep-dist.txt")
print(mu, n = Inf)
sink()

# plot for all groups combined
mycols = c(white, black, asian, hispanic)
p <- ggplot() +
    geom_density(data = dat_interest, 
                 aes(x = prob_passage_pct, 
                     color = race)) +
    geom_vline(data = mu, 
               aes(xintercept = grp.mean, 
                   color = race),
               linetype = "dashed") +
    scale_color_manual(values = mycols) +
    scale_y_continuous("Density", expand = c(0,0), limits=c(0, 0.09)) +
    scale_x_continuous('Ex-ante probability of AP math success', limits=c(0,100), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0,0)) +
    ggtheme +
    theme(legend.position="top",
          legend.direction = "horizontal",
          legend.title = element_blank()) 
p
ggsave("figures/fig_acad-prep-dist.png", width = 15, height = 15, units = "cm")

#=============================================================
# Plot predicted probabilities across tracks (passage model)
#=============================================================

dat_interest <- dat_interest %>%
  mutate(subgroup = case_when(
    (AP_math_enroll_logical == T & AP_math_exam_logical == T) ~ "AP math enrollment = Yes;\nAP math exam participation = Yes",
    (AP_math_enroll_logical == T & AP_math_exam_logical == F) ~ "AP math enrollment = Yes;\nAP math exam participation = No",
    AP_math_enroll_logical == F ~ "AP math enrollment = No"))

mu <- dat_interest %>%
  select(prob_passage_pct, race, subgroup) %>%
  group_by(race, subgroup) %>%
  summarise(grp.mean = mean(prob_passage_pct))

sink("output/acad-prep-dist_across-samples.txt")
print(mu, n = Inf)
sink()

p <- 
  ggplot() +
  geom_density(data = dat_interest, 
               aes(x = prob_passage_pct,
                   color = race)) +
  geom_vline(data = mu, 
             aes(xintercept = grp.mean, 
                 color = race),
             linetype = "dashed") +
  scale_color_manual(values = mycols) +
  scale_y_continuous("Density", expand = c(0,0), limits=c(0, 0.09)) +
  scale_x_continuous('Ex-ante probability of AP math success', limits=c(0.001,99.999), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0)) +
  facet_wrap(~subgroup) +
  ggtheme +
  theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
p
ggsave("figures/fig_acad-prep-dist_across-samples.png", width = 25, height = 15, units = "cm")

#=============================================================
# Plot predicted probabilities (enrollment model)
#=============================================================

# compute means
mu <- dat_interest %>%
  select(prob_enrollment_pct, race) %>%
  group_by(race) %>%
  summarise(grp.mean = mean(prob_enrollment_pct))

sink("output/estimated-enroll-prop-dist.txt")
print(mu, n = Inf)
sink()

# plot for all groups combined
mycols = c(white, black, asian, hispanic)
p <- ggplot() +
    geom_density(data = dat_interest, 
                 aes(x = prob_enrollment_pct, 
                     color = race)) +
    geom_vline(data = mu, 
               aes(xintercept = grp.mean, 
                   color = race),
               linetype = "dashed") +
    scale_color_manual(values = mycols) +
    scale_y_continuous("Density", expand = c(0,0), limits=c(0, 0.25)) +
    scale_x_continuous('Ex-ante probability of AP math enrollment', limits=c(0,100), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0,0)) +
    ggtheme +
    theme(legend.position="top",
          legend.direction = "horizontal",
          legend.title = element_blank()) 
p
ggsave("figures/fig_estimated-enroll-prop-dist.png", width = 15, height = 15, units = "cm")

#=============================================================
# Plot predicted probabilities across tracks (enrollment model)
#=============================================================

dat_interest <- dat_interest %>%
  mutate(subgroup = case_when(
    (AP_math_enroll_logical == T & AP_math_exam_logical == T) ~ "AP math enrollment = Yes;\nAP math exam participation = Yes",
    (AP_math_enroll_logical == T & AP_math_exam_logical == F) ~ "AP math enrollment = Yes;\nAP math exam participation = No",
    AP_math_enroll_logical == F ~ "AP math enrollment = No"))

mu <- dat_interest %>%
  select(prob_enrollment_pct, race, subgroup) %>%
  group_by(race, subgroup) %>%
  summarise(grp.mean = mean(prob_enrollment_pct))

sink("output/estimated-enroll-prop-dist_across-samples.txt")
print(mu, n = Inf)
sink()

p <- 
  ggplot() +
  geom_density(data = dat_interest, 
               aes(x = prob_enrollment_pct,
                   color = race)) +
  geom_vline(data = mu, 
             aes(xintercept = grp.mean, 
                 color = race),
             linetype = "dashed") +
  scale_color_manual(values = mycols) +
  scale_y_continuous("Density", expand = c(0,0), limits=c(0, 0.25)) +
  scale_x_continuous('Ex-ante probability of AP math success', limits=c(0.001,99.999), 
                     labels = function(x) paste0(x, "%"),
                     expand = c(0,0)) +
  facet_wrap(~subgroup) +
  ggtheme +
  theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
p
ggsave("figures/fig_estimated-enroll-prop-dist_across-samples.png", width = 25, height = 15, units = "cm")


