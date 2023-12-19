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

# Packages
library(lme4)  
library(texreg)
library(RColorBrewer)
library(broom.mixed)
library(modelr)

# usual header
source("header_basic.R")
source("header_parameters.R")
source("header_sensitivity-functions.R")
useViewer = FALSE

# load data
load(paste0(DOE_wd,"/dataset_predictions.Rdata"))
# rename data
dat <- dat_interest
rm(dat_interest)

#==============================================================
# Define models 
#==============================================================

# traditional model I (kitchen sink regression)
c_dat1 <- dat %>%
  select(starts_with("BIO_"),
         starts_with("MS_") & contains("exam"),
         starts_with("HS"),
         -contains("total"),
         -contains("nacad"),
         -contains("acad"),
         -contains("status"),
         -contains("performance"),
         -contains("pct_cr_earned"),
         -contains("exam_grade"),
         -HS_gpa_09,
         -HS_gpa_10,
         HS_math_adv)

# Check full column rank
covariates1 <- as.matrix(c_dat1)
ncol(covariates1)
covariates1 <- covariates1[, qr(covariates1)$pivot[seq_len(qr(covariates1)$rank)]]
ncol(covariates1)
colnames(covariates1)

# traditional model II
c_dat2 <- dat %>%
  select(starts_with("BIO_"),
         starts_with("HS_gpa_math"),
         HS_math_adv)

# check full column rank
covariates2 <- as.matrix(c_dat2)
ncol(covariates2)
covariates2 <- covariates2[, qr(covariates2)$pivot[seq_len(qr(covariates2)$rank)]]
ncol(covariates2)
colnames(covariates2)

# variables of interest
outcome <- c("AP_math_enroll_factor")
predictors1 <- colnames(covariates1)
predictors2 <- colnames(covariates2)

# formulas
formula_raw <- as.formula(paste(outcome, 
                                  paste(c("race",
                                          "ID_school"), 
                                        collapse = " + "), 
                                  sep = " ~ "))
formula_trad1 <- as.formula(paste(outcome, 
                               paste(c("race",
                                       predictors1,
                                       "ID_school"), 
                                     collapse = " + "), 
                               sep = " ~ "))
formula_trad2 <- as.formula(paste(outcome, 
                                     paste(c("race",
                                             predictors2,
                                             "ID_school"), 
                                           collapse = " + "), 
                                     sep = " ~ "))
formula_rar <- as.formula(paste(outcome, 
                                   paste(c("race",
                                           "logit_prob_passage",
                                           "ID_school"), 
                                         collapse = " + "), 
                                   sep = " ~ "))

#==============================================================
# Run models
#==============================================================

set.seed(seed)
m_raw <- glm(formula = formula_raw, data = dat, family = binomial)
m_trad1 <- glm(formula = formula_trad1, data = dat, family = binomial)
m_trad2 <- glm(formula = formula_trad2, data = dat, family = binomial)
m_rar <- glm(formula = formula_rar, data = dat, family = binomial)

# from log-odds to odds-ratio
m1 <- exp(coef(m_raw))
m2 <- exp(coef(m_trad1))
m3 <- exp(coef(m_trad2))
m4 <- exp(coef(m_rar))

#==============================================================
# Print results in a table
#==============================================================

sink("output/disparate-impact-models.txt")

texreg(list(m_rar, m_raw, m_trad1, m_trad2), 
       override.coef = list(m4, m1, m2, m3),
       digits = 2,
       include.rsquared = F,
       include.adjrs = F,
       include.nobs = F,
       custom.model.names = c("Preparedness-adjusted", 
                              "Raw disparities", 
                              "Traditional II", 
                              "Traditional approach II"),
       custom.header = list("Odds" = 1:4),
       use.packages = T)
sink()

#==============================================================
# Compute CIs and SEs
#==============================================================

# model 1: estimates
est1 <- as.data.frame(m1) 
est1$term <- rownames(est1)
est1 <- as_tibble(est1) %>%
  rename(estimate = m1)

# model 1: confidence intervals
s1 <- as.data.frame(confint.default(m_raw))
s1$term <- rownames(s1)
s1 <- as_tibble(s1) %>%
  rename(conf.low = "2.5 %",
         conf.high = "97.5 %") %>%
  mutate(conf.low = round(exp(conf.low), 4),
         conf.high = round(exp(conf.high), 4))
s1 <- left_join(s1, est1, by = "term")

# model 2: estimates
est2 <- as.data.frame(m2) 
est2$term <- rownames(est2)
est2 <- as_tibble(est2) %>%
  rename(estimate = m2)

# model 2: confidence intervals
s2 <- as.data.frame(confint.default(m_trad1))
s2$term <- rownames(s2)
s2 <- as_tibble(s2) %>%
  rename(conf.low = "2.5 %",
         conf.high = "97.5 %") %>%
  mutate(conf.low = round(exp(conf.low), 4),
         conf.high = round(exp(conf.high), 4))
s2 <- left_join(s2, est2, by = "term")

# model 3: estimates
est3 <- as.data.frame(m3) 
est3$term <- rownames(est3)
est3 <- as_tibble(est3) %>%
  rename(estimate = m3)

# model 3: confidence intervals
s3 <- as.data.frame(confint.default(m_trad2))
s3$term <- rownames(s3)
s3 <- as_tibble(s3) %>%
  rename(conf.low = "2.5 %",
         conf.high = "97.5 %") %>%
  mutate(conf.low = round(exp(conf.low), 4),
         conf.high = round(exp(conf.high), 4))
s3 <- left_join(s3, est3, by = "term")

# model 4: estimates
est4 <- as.data.frame(m4) 
est4$term <- rownames(est4)
est4 <- as_tibble(est4) %>%
  rename(estimate = m4)

# model 4: confidence intervals
s4 <- as.data.frame(confint.default(m_rar))
s4$term <- rownames(s4)
s4 <- as_tibble(s4) %>%
  rename(conf.low = "2.5 %",
         conf.high = "97.5 %") %>%
  mutate(conf.low = round(exp(conf.low), 4),
         conf.high = round(exp(conf.high), 4))
s4 <- left_join(s4, est4, by = "term")

#==============================================================
# bootstrap standard erros for the 
# preparedness-adjusted model
#==============================================================

# use the boot package to find N samples
bs_data <-  dat %>%
  bootstrap(N_bootstrap)  

# apply the preparedness-adjusted model to each of the N samples
bs_models <- bs_data %>% 
  mutate(lm = map(strap, ~glm(formula = formula_rar, 
                              data =., 
                              family = binomial)), 
         tidy = map(lm, broom::tidy)) # ---> default "exponentiate" is false

# store estimates
bs_tidy <- bs_models %>%
  pull(tidy) %>%
  map2_df(.,seq(1, N_bootstrap),~mutate(.x, resample = .y))

# computed the bs std. errors
# ---> note: bootstrapped standard errors are the standard deviation 
#      of the coefficient estimate for each of the parameters in the model 
bs_se <- bs_tidy %>%
  group_by(term) %>%
  summarize(bs_se = sd(estimate))

# confidence level considered here
c_level_plots

# T-critical for a 2-tailed t-test
t_critical <- qt(((1 - c_level_plots)/2), (length(dat)-1), lower.tail = F) 

# compute new CIs and store estimates (model 4 only)
bs_se <- left_join(s4, bs_se, by = "term") %>%
  select(term, estimate, bs_se) 

# print results
sink("output/bootstrap-standard-erros.txt")
print(bs_se, n = Inf)
sink()

# apply the new SEs to the log odds to get the new CIs.
s4_bs <- bs_se %>%
  mutate(log_odds = log(estimate),
         conf.low = exp(log_odds - t_critical * bs_se),
         conf.high = exp(log_odds + t_critical * bs_se)) %>%
  select(estimate, conf.low, conf.high, term)

#==============================================================
# Save model results
#==============================================================

# create a dataset only with results from the preparedness-adjusted model
# (it will be helpful for the sensitivity analysis)
dat_pad_estimates <- bs_se %>%
  select(term, estimate, bs_se) %>%
  mutate(race = case_when(
           term == "raceBlack" ~ "Black",
           term == "raceAsian" ~ "Asian",
           term == "raceHispanic" ~ "Hispanic",
           term == "raceOther" ~ "Other",
           term == "logit_prob_passage" ~ "logit_prob_passage",
           term == "(Intercept)" ~ "(Intercept)")) %>%
  filter(race == "Black" | race == "Hispanic" | race == "Asian") 

# create a dataset with results from all models
dat_estimates <- rbind(s1, s2, s3, s4_bs) %>%
  mutate(race = case_when(
           term == "raceBlack" ~ "Black",
           term == "raceAsian" ~ "Asian",
           term == "raceHispanic" ~ "Hispanic",
           term == "raceOther" ~ "Other",
           term == "logit_prob_passage" ~ "logit_prob_passage",
           term == "(Intercept)" ~ "(Intercept)")) %>%
  filter(race == "Black" | race == "Hispanic" | race == "Asian")

# create a "models" variable and add to this dataset
models = data.frame(models = c(rep(c("Raw disparities"), 3),
                        rep(c("Traditional approach I"), 3),
                        rep(c("Traditional approach II"), 3),
                        rep(c("Preparedness-adjusted"), 3)))
dat_estimates <- dat_estimates %>%
  add_column(models) %>%
  select(-term) %>%
  mutate(model_type = "School-level fixed effects")

# save
save(dat_estimates, file = paste0(DOE_wd,"/dataset_estimates-all.Rdata"))

#==============================================================
# Defining search ranges for the sensitivity analysis
#==============================================================

# create a "high" category for HS math GPA (10th grade). 
# this is a variable which is highly relevant in both the enrollment and passage XGBoost models

# controls to include in the model
controls <- dat %>%
  select(HS_gpa_eng_10,
         HS_gpa_sci_10) %>%
  names()

# define a "high" GPA if it is higher than the mean
mu = mean(dat$HS_gpa_math_10)
low_gpa = mu - sd(dat$HS_gpa_math_10)
high_gpa = mu + sd(dat$HS_gpa_math_10)

# create the dummy variable based on this definition
dat_test <- dat %>%
  mutate(
    binary_test = case_when(
      HS_gpa_math_10 > high_gpa ~ 1,
      TRUE ~ 0),
    binary_test = as.factor(binary_test))

# define formula to run the enrollment model
formula <- as.formula(paste("AP_math_enroll_factor",
                            paste(c("binary_test",
                                    controls,
                                    "ID_school"),
                                  collapse = " + "),
                            sep = " ~ "))
# run model
set.seed(seed)
m_enrollment <- glm(formula = formula, 
                    data = dat_test, 
                    family = binomial)

sink("output/effect-of-gpa-dummy.txt")

# get coeff
gpa_coeff <- round(exp(m_enrollment$coefficients[["binary_test1"]]), 2)
paste0("Effect of dummy GPA covariate on enrollment = ", gpa_coeff)

# now do the same but for the exam passage model
dat_test_passage <- dat_test %>%
  filter(AP_math_enroll_logical == TRUE,
         AP_math_exam_logical == TRUE)

# define formula to run the enrollment model
formula <- as.formula(paste("AP_math_pass_factor",
                            paste(c("binary_test",
                                    controls,
                                    "ID_school"),
                                  collapse = " + "),
                            sep = " ~ "))

# run model
set.seed(seed)
m_passage <- glm(formula = formula, 
                 data = dat_test_passage, 
                 family = binomial)


# get coeff
gpa_coeff <- round(exp(m_passage$coefficients[["binary_test1"]]), 2)
paste0("Effect of dummy GPA covariate on exam passage = ", gpa_coeff)
sink()

#==============================================================
# Sensitivity analysis
#==============================================================

# create sensitivity data
dat_sensitivity <- dat %>% 
  select(risk = prob_passage,
         prop_score = prob_enrollment,
         race,
         ID_school) %>%
  as_tibble()

# check how long it takes to run all the sens functions
start_time <- Sys.time()

# apply sensitivity function to compare Black students to White students
sensitivity_results_black <- grid_search_sensitivity_fr(data = dat_sensitivity,
                                               q_group_reference_range = q_group_reference_range,
                                               q_group_interest_range = q_group_interest_range,
                                               race_group_reference = "White",
                                               race_group_interest = "Black",
                                               da_range = da_range,
                                               dr_range = dr_range,
                                               N = N_sensitivity,
                                               control = "ID_school",
                                               mlm = "fixed effects")

sensitivity_results_black[[1]]
sensitivity_results_black[[2]]
sensitivity_results_black[[3]] 

# apply sensitivity function to compare Asian students to White students.
sensitivity_results_asian <- grid_search_sensitivity_fr(data = dat_sensitivity,
                                               q_group_reference_range = q_group_reference_range,
                                               q_group_interest_range = q_group_interest_range,
                                               race_group_reference = "White",
                                               race_group_interest = "Asian",
                                               da_range = da_range,
                                               dr_range = dr_range,
                                               N = N_sensitivity,
                                               control = "ID_school",
                                               mlm = "fixed effects")
sensitivity_results_asian[[1]]
sensitivity_results_asian[[2]]
sensitivity_results_asian[[3]]

# apply sensitivity function to compare Hispanic students to White students.
sensitivity_results_hispanic <- grid_search_sensitivity_fr(data = dat_sensitivity,
                                               q_group_reference_range = q_group_reference_range,
                                               q_group_interest_range = q_group_interest_range,
                                               race_group_reference = "White",
                                               race_group_interest = "Hispanic",
                                               da_range = da_range,
                                               dr_range = dr_range,
                                               N = N_sensitivity,
                                               control = "ID_school",
                                               mlm = "fixed effects")

sensitivity_results_hispanic[[1]]
sensitivity_results_hispanic[[2]]
sensitivity_results_hispanic[[3]]

end_time <- Sys.time()
sink("output/run-time_sensitivity.txt")
end_time - start_time
sink()

# create data with all sensitivity estimates
dat_estimates_sens <- as_tibble(data.frame(race = c("Black", "Asian", "Hispanic"),
                                 estimate_lb = c(sensitivity_results_black[[2]]$outcome,
                                                 sensitivity_results_asian[[2]]$outcome, 
                                                 sensitivity_results_hispanic[[2]]$outcome),
                                 estimate_ub = c(sensitivity_results_black[[3]]$outcome,
                                                 sensitivity_results_asian[[3]]$outcome,
                                                 sensitivity_results_hispanic[[3]]$outcome)))

# t-critical for a 2-tailed t-test with alpha = 0.05
t_critical <- qt((0.05/2), (length(dat)-1), lower.tail = FALSE)

# now create a single dataset with original + sens estimates
# (here is where the dat_pad_estimates object will be helpful)
# apply the bs SEs to the log odds to get the correct CIs
dat_estimates_sens <- inner_join(dat_pad_estimates, 
                                 dat_estimates_sens, 
                                 by = "race") %>%
  mutate(log_odds_lb = log(estimate_lb),
         log_odds_ub = log(estimate_ub),
         conf.low = exp(log_odds_lb - t_critical * bs_se),
         conf.high = exp(log_odds_ub + t_critical * bs_se)) %>%
  select(race, 
         estimate, 
         estimate_lb, 
         estimate_ub, 
         conf.low, 
         conf.high)

sink("output/sensitivity-estimates.txt")
dat_estimates_sens
sink()

# save 
save(dat_estimates_sens, file = paste0(DOE_wd,"/dataset_estimates-sens.Rdata"))
