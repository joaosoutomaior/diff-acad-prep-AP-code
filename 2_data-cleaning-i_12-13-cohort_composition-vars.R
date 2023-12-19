#==============================================================
# File description
#==============================================================
# contents: 
#  create composition variables
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

# ---> Note that cohort composition variables are different from 
#      total high school composition variables
#      (E.g.: Pct black in the cohort is different from 
#      pct black in the high school)

# ---> These variables were created mostly for our description and
#      understanding of cohorts of interest and will be removed before
#      the construction of our models/analyses
#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#==============================================================
# Files
#==============================================================

NYC = paste0(DOE_server_wd_data,"/dataset_8_cohort-12-13.Rdata")
load(NYC)

# Final check at all pct variables before further calculations
vars <- dat %>%
  select(contains("pct"))
x = pct_missing_variable(vars)
print(x, n = 500)

impute_vars <- c(names(vars))
dat <- as.data.frame(dat)
for(i in impute_vars) {
  dat[is.na(dat[,i]), i] <- 0
}
dat <- as_tibble(dat)

# New variables
dat <- dat %>%
  mutate(AP_n_courses_total = AP_n_courses_09 +
           AP_n_courses_10 +
           AP_n_courses_11 +
           AP_n_courses_12,
         AP_math_exam_n_total = EXM_APMTHATM_09 + 
           EXM_APMTHATM_10 + 
           EXM_APMTHATM_11 + 
           EXM_APMTHATM_12)
# races
dat <- agg(dat, dat$ones, "SCH_cohort_HS_size", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_race_other, "SCH_cohort_n_other", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_asian, "SCH_cohort_n_asian", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_hispanic, "SCH_cohort_n_hispanic", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_white, "SCH_cohort_n_white", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_black, "SCH_cohort_n_black", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_male, "SCH_cohort_n_male", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_female, "SCH_cohort_n_female", dat$ID_school, "ID_school")
dat <- agg(dat, dat$BIO_fr_lunch, "SCH_cohort_n_fr_lunch", dat$ID_school, "ID_school")

# ap enrollment
dat <- agg(dat, dat$black_ap, "SCH_cohort_n_ap_black", dat$ID_school, "ID_school")
dat <- agg(dat, dat$white_ap, "SCH_cohort_n_ap_white", dat$ID_school, "ID_school")
dat <- agg(dat, dat$hispanic_ap, "SCH_cohort_n_ap_hispanic", dat$ID_school, "ID_school")
dat <- agg(dat, dat$other_ap, "SCH_cohort_n_ap_other", dat$ID_school, "ID_school")
dat <- agg(dat, dat$asian_ap, "SCH_cohort_n_ap_asian", dat$ID_school, "ID_school")
dat <- agg(dat, dat$male_ap, "SCH_cohort_n_ap_male", dat$ID_school, "ID_school")
dat <- agg(dat, dat$female_ap, "SCH_cohort_n_ap_female", dat$ID_school, "ID_school")

# ap pass rate
dat <- agg(dat, dat$black_ap_pass, "SCH_cohort_n_ap_pass_black", dat$ID_school, "ID_school")
dat <- agg(dat, dat$white_ap_pass, "SCH_cohort_n_ap_pass_white", dat$ID_school, "ID_school")
dat <- agg(dat, dat$hispanic_ap_pass, "SCH_cohort_n_ap_pass_hispanic", dat$ID_school, "ID_school")
dat <- agg(dat, dat$other_ap_pass, "SCH_cohort_n_ap_pass_other", dat$ID_school, "ID_school")
dat <- agg(dat, dat$asian_ap_pass, "SCH_cohort_n_ap_pass_asian", dat$ID_school, "ID_school")
dat <- agg(dat, dat$male_ap_pass, "SCH_cohort_n_ap_pass_male", dat$ID_school, "ID_school")
dat <- agg(dat, dat$female_ap_pass, "SCH_cohort_n_ap_pass_female", dat$ID_school, "ID_school")

# ap exam taking rate
dat <- agg(dat, dat$black_ap_exam, "SCH_cohort_n_ap_exam_black", dat$ID_school, "ID_school")
dat <- agg(dat, dat$white_ap_exam, "SCH_cohort_n_ap_exam_white", dat$ID_school, "ID_school")
dat <- agg(dat, dat$hispanic_ap_exam, "SCH_cohort_n_ap_exam_hispanic", dat$ID_school, "ID_school")
dat <- agg(dat, dat$other_ap_exam, "SCH_cohort_n_ap_exam_other", dat$ID_school, "ID_school")
dat <- agg(dat, dat$asian_ap_exam, "SCH_cohort_n_ap_exam_asian", dat$ID_school, "ID_school")
dat <- agg(dat, dat$male_ap_exam, "SCH_cohort_n_ap_exam_male", dat$ID_school, "ID_school")
dat <- agg(dat, dat$female_ap_exam, "SCH_cohort_n_ap_exam_female", dat$ID_school, "ID_school")

# ap course pass rate
dat <- agg(dat, dat$black_ap_pass_course, "SCH_cohort_n_ap_pass_course_black", dat$ID_school, "ID_school")
dat <- agg(dat, dat$white_ap_pass_course, "SCH_cohort_n_ap_pass_course_white", dat$ID_school, "ID_school")
dat <- agg(dat, dat$hispanic_ap_pass_course, "SCH_cohort_n_ap_pass_course_hispanic", dat$ID_school, "ID_school")
dat <- agg(dat, dat$other_ap_pass_course, "SCH_cohort_n_ap_pass_course_other", dat$ID_school, "ID_school")
dat <- agg(dat, dat$asian_ap_pass_course, "SCH_cohort_n_ap_pass_course_asian", dat$ID_school, "ID_school")
dat <- agg(dat, dat$male_ap_pass_course, "SCH_cohort_n_ap_pass_course_male", dat$ID_school, "ID_school")
dat <- agg(dat, dat$female_ap_pass_course, "SCH_cohort_n_ap_pass_course_female", dat$ID_school, "ID_school")

# total ap enrollment
dat <- agg(dat, dat$AP_n_courses_total, "SCH_cohort_ap_math_courses", dat$ID_school, "ID_school")
dat <- agg(dat, dat$AP_math_enroll, "SCH_cohort_ap_math_enrollment", dat$ID_school, "ID_school")

dat <- dat %>%
  mutate(# AP courses
    SCH_cohort_pct_ap_hispanic =  SCH_cohort_n_ap_hispanic / SCH_cohort_n_hispanic,
    SCH_cohort_pct_ap_other = SCH_cohort_n_ap_other / SCH_cohort_n_other,
    SCH_cohort_pct_ap_asian = SCH_cohort_n_ap_asian / SCH_cohort_n_asian,
    SCH_cohort_pct_ap_black = SCH_cohort_n_ap_black / SCH_cohort_n_black, 
    SCH_cohort_pct_ap_white = SCH_cohort_n_ap_white / SCH_cohort_n_white,
    SCH_cohort_pct_ap_female = SCH_cohort_n_ap_female / SCH_cohort_n_female,
    SCH_cohort_pct_ap_male = SCH_cohort_n_ap_male / SCH_cohort_n_male,
    # AP exams
    SCH_cohort_pct_ap_exam_hispanic =  SCH_cohort_n_ap_exam_hispanic / SCH_cohort_n_ap_hispanic,
    SCH_cohort_pct_ap_exam_other = SCH_cohort_n_ap_exam_other / SCH_cohort_n_ap_other,
    SCH_cohort_pct_ap_exam_asian = SCH_cohort_n_ap_exam_asian / SCH_cohort_n_ap_asian,
    SCH_cohort_pct_ap_exam_black = SCH_cohort_n_ap_exam_black / SCH_cohort_n_ap_black, 
    SCH_cohort_pct_ap_exam_white = SCH_cohort_n_ap_exam_white / SCH_cohort_n_ap_white,
    SCH_cohort_pct_ap_exam_female = SCH_cohort_n_ap_exam_female / SCH_cohort_n_ap_female,
    SCH_cohort_pct_ap_exam_male = SCH_cohort_n_ap_exam_male / SCH_cohort_n_ap_male,
    # AP exams passed
    SCH_cohort_pct_ap_pass_hispanic =  SCH_cohort_n_ap_pass_hispanic / SCH_cohort_n_ap_exam_hispanic,
    SCH_cohort_pct_ap_pass_other = SCH_cohort_n_ap_pass_other / SCH_cohort_n_ap_exam_other,
    SCH_cohort_pct_ap_pass_asian = SCH_cohort_n_ap_pass_asian / SCH_cohort_n_ap_exam_asian,
    SCH_cohort_pct_ap_pass_black = SCH_cohort_n_ap_pass_black / SCH_cohort_n_ap_exam_black, 
    SCH_cohort_pct_ap_pass_white = SCH_cohort_n_ap_pass_white / SCH_cohort_n_ap_exam_white,
    SCH_cohort_pct_ap_pass_female = SCH_cohort_n_ap_pass_female / SCH_cohort_n_ap_exam_female,
    SCH_cohort_pct_ap_pass_male = SCH_cohort_n_ap_pass_male / SCH_cohort_n_ap_exam_male,
    # AP courses passed
    SCH_cohort_pct_ap_pass_course_hispanic =  SCH_cohort_n_ap_pass_course_hispanic / SCH_cohort_n_ap_hispanic,
    SCH_cohort_pct_ap_pass_course_other = SCH_cohort_n_ap_pass_course_other / SCH_cohort_n_ap_other,
    SCH_cohort_pct_ap_pass_course_asian = SCH_cohort_n_ap_pass_course_asian / SCH_cohort_n_ap_asian,
    SCH_cohort_pct_ap_pass_course_black = SCH_cohort_n_ap_pass_course_black / SCH_cohort_n_ap_black, 
    SCH_cohort_pct_ap_pass_course_white = SCH_cohort_n_ap_pass_course_white / SCH_cohort_n_ap_white,
    SCH_cohort_pct_ap_pass_course_female = SCH_cohort_n_ap_pass_course_female / SCH_cohort_n_ap_female,
    SCH_cohort_pct_ap_pass_course_male = SCH_cohort_n_ap_pass_course_male / SCH_cohort_n_ap_male,
    # AP courses 
    SCH_cohort_pct_apmath_enroll = SCH_cohort_ap_math_enrollment / SCH_cohort_HS_size,
    SCH_cohort_ap_math_courses_st = SCH_cohort_ap_math_courses / SCH_cohort_HS_size)

# summarize percentage variables
vars <- dat %>%
  select(starts_with("SCH_cohort_pct"))
lapply(vars, summary)
dat[sapply(dat, is.infinite)] <- NA

# Multiply pct variables by 100.
multiply_var = function(x){
  x <- 100 * x
  return(x)
}
dat <- dat %>%
  mutate(across(.cols = starts_with("SCH_pct"),
                .fns = ~as.numeric(.x))) %>%
  mutate(across(.cols = starts_with("SCH_pct"),
                .fns = ~multiply_var(.x)))

# Adjust (not needed for exams)
#vars <- dat %>%
#  select(starts_with("SCH_cohort_pct"),
 #        -contains("exam")) %>%
#  names()
#dat <- dat %>%
 # mutate(across(.cols = all_of(c(vars)),
  #              .fns = ~replace(., . > 100, NA)))

# relative risk variables
dat <- dat %>%
  mutate(SCH_cohort_RR_ap_W_b = SCH_cohort_pct_ap_white / SCH_cohort_pct_ap_black,
         SCH_cohort_RR_ap_w_a = SCH_cohort_pct_ap_white / SCH_cohort_pct_ap_asian,
         SCH_cohort_RR_ap_w_h = SCH_cohort_pct_ap_white / SCH_cohort_pct_ap_hispanic,
         SCH_cohort_RR_ap_w_o = SCH_cohort_pct_ap_white / SCH_cohort_pct_ap_other,
         SCH_cohort_RR_ap_f_m = SCH_cohort_pct_ap_female / SCH_cohort_pct_ap_male)

# check RR variables
dat[sapply(dat, is.infinite)] <- NA
vars <- dat %>%
  select(contains("RR_"))
lapply(vars,summary)
    
dat <- dat %>%
  mutate(k_asian = ifelse(SCH_cohort_n_asian > 0, 1, 0),
         k_hispanic = ifelse(SCH_cohort_n_hispanic > 0, 1, 0),
         k_other = ifelse(SCH_cohort_n_other > 0, 1, 0),
         k_black = ifelse(SCH_cohort_n_black > 0, 1, 0),
         k_white = ifelse(SCH_cohort_n_white > 0, 1, 0),
         sum_f_k_squared =
           (SCH_cohort_n_asian * SCH_cohort_n_asian) + 
           (SCH_cohort_n_hispanic * SCH_cohort_n_hispanic) + 
           (SCH_cohort_n_other * SCH_cohort_n_other) + 
           (SCH_cohort_n_black * SCH_cohort_n_black) + 
           (SCH_cohort_n_white * SCH_cohort_n_white),
         n_k = k_asian + k_hispanic + k_other + k_black + k_white,
         n_squared = SCH_cohort_HS_size * SCH_cohort_HS_size,
         SCH_cohort_diversity_index = (n_k * (n_squared - sum_f_k_squared)) / (n_squared * (n_k - 1)))

dat <- dat %>%
  mutate(SCH_cohort_diversity_index = replace(SCH_cohort_diversity_index,is.na(SCH_cohort_diversity_index), 0))
dat <- dat %>%
  mutate(SCH_cohort_diversity_index = replace(SCH_cohort_diversity_index, 
                                       SCH_cohort_diversity_index > 1,
                                       1))
dat <- dat %>%
  mutate(SCH_cohort_pct_asian = 100 * SCH_cohort_n_asian / SCH_cohort_HS_size,
         SCH_cohort_pct_hispanic = 100 * SCH_cohort_n_hispanic / SCH_cohort_HS_size,
         SCH_cohort_pct_other = 100 * SCH_cohort_n_other / SCH_cohort_HS_size, 
         SCH_cohort_pct_black = 100 * SCH_cohort_n_black / SCH_cohort_HS_size,
         SCH_cohort_pct_white = 100 * SCH_cohort_n_white / SCH_cohort_HS_size,
         SCH_cohort_pct_black_white = 100 * (SCH_cohort_n_white + SCH_cohort_n_black) / SCH_cohort_HS_size,
         SCH_cohort_pct_female = 100 * SCH_cohort_n_female / SCH_cohort_HS_size,
         SCH_cohort_pct_fr_lunch = 100 * SCH_cohort_n_fr_lunch / SCH_cohort_HS_size)

dat <- dat %>%
  mutate(SCH_cohort_diversity_factor = 
           cut(SCH_cohort_diversity_index,
               breaks = 5,
               labels = c("0-0.2", 
                          "0.2-0.4", 
                          "0.4-0.6", 
                          "0.6-0.8",
                          "0.8-1"),
               right  = FALSE))
dat <- dat %>%
  mutate(SCH_cohort_pct_white_factor = 
           cut(SCH_cohort_pct_white,
               breaks = 5,
               labels = c("0-20%", 
                          "20-40%", 
                          "40-60%", 
                          "60-80%",
                          "80-100%"),
               right  = FALSE))
dat <- dat %>%
  mutate(SCH_cohort_pct_black_factor = 
           cut(SCH_cohort_pct_black,
               breaks = 5,
               labels = c("0-20%", 
                          "20-40%", 
                          "40-60%", 
                          "60-80%",
                          "80-100%"),
               right  = FALSE))

#==============================================================
# Check percentage variables
#==============================================================

# Check pct vars
vars <- dat %>% 
  select(contains("_pct"))
x = pct_missing_variable(vars)
print(x, n =200)

#Any > 100 pct?
rows_higher_than_100 <- function(x){
  length(which(dat[ ,x] > 100))}

vars <- dat %>% 
  select(contains("_pct"))
lapply(names(vars), rows_higher_than_100)
names(vars)

#==============================================================
# Save
#==============================================================
save(dat, file = paste0(DOE_server_wd_data,"/dataset_9_cohort-12-13.Rdata"))
