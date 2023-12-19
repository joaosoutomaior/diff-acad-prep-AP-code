#==============================================================
# File description
#==============================================================
# contents: 
#  adjust variables and handle missing data
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#==============================================================
# Data
#==============================================================

NYC = paste0(DOE_server_wd_data,"/dataset_7_cohort-12-13.Rdata")
load(NYC)

# print to visualize missing
#sink("output/missing-12-13.txt")

#==============================================================
# Missing data
#==============================================================

x = pct_missing_variable(dat)
print(as_tibble(x), n=400)

#==============================================================
# Covariates
#==============================================================

dat <- dat %>%
  rename(######### MS exams
    # MATH
    MS_math_exam_grade_07 = MS_MTHGRD_07,
    MS_math_exam_grade_08 = MS_MTHGRD_08,
    MS_math_exam_performance_07 = MS_MTHPLV_07,
    MS_math_exam_performance_08 = MS_MTHPLV_08,
    MS_math_exam_percentile_08 = MS_MTHPCTCAL_08,
    MS_math_exam_percentile_07 = MS_MTHPCTCAL_07,
    MS_math_exam_status_07 = MS_MTHTSD_07,
    MS_math_exam_status_08 = MS_MTHTSD_08,
    MS_math_poverty_level_07 = MS_MTHPOV_08,
    MS_math_poverty_level_08 = MS_MTHPOV_07,
    # ELA
    MS_ela_exam_grade_07 = MS_ELAGRD_07,
    MS_ela_exam_grade_08 = MS_ELAGRD_08,
    MS_ela_exam_performance_07 = MS_ELAPLV_07,
    MS_ela_exam_performance_08 = MS_ELAPLV_08,
    MS_ela_exam_percentile_08 = MS_ELAPCTCAL_08,
    MS_ela_exam_percentile_07 = MS_ELAPCTCAL_07,
    MS_ela_exam_status_07 = MS_ELATSD_07,
    MS_ela_exam_status_08 = MS_ELATSD_08,
    MS_ela_poverty_level_07 = MS_ELAPOV_08,
    MS_ela_poverty_level_08 = MS_ELAPOV_07,
    
    ####### MS coursework
    # 7th grade
    MS_total_cr_earned_07 = MS_CREDERNTOT_07,
    MS_total_cr_earned_acad_07 = MS_CREDERNACD_07,
    MS_gpa_07 = MS_CRSCGPAWTOT_07,
    MS_gpa_acad_07 = MS_CRSCGPAWACD_07,
    # 8th grade
    MS_total_cr_earned_08 = MS_CREDERNTOT_08,
    MS_gpa_08 = MS_CRSCGPAWTOT_08,
    MS_total_cr_earned_acad_08 = MS_CREDERNACD_08,
    MS_gpa_acad_08 = MS_CRSCGPAWACD_08,
    
    ######## HS coursework
    # 9th grade
    HS_total_cr_earned_09 = HS_CRDTOTERN_09,
    HS_total_cr_earned_09_acad = HS_CRDACDERN_09,
    HS_total_cr_earned_09_math = HS_CRDMTHERN_09,
    HS_total_cr_earned_09_eng = HS_CRDENGERN_09,
    HS_total_cr_earned_09_sci = HS_CRDSCIERN_09,
    HS_gpa_09 = HS_CRSTOTGPAW_09,
    HS_gpa_nacad_09 = HS_CRSNACGPAW_09,
    HS_gpa_acad_09 = HS_CRSACDGPAW_09,
    HS_gpa_math_09 = HS_CRSMTHGPAW_09,
    HS_gpa_eng_09 = HS_CRSENGGPAW_09,
    HS_gpa_sci_09 = HS_CRSSCIGPAW_09,
    # 10th grade
    HS_total_cr_earned_10 = HS_CRDTOTERN_10,
    HS_total_cr_earned_10_acad = HS_CRDACDERN_10,
    HS_total_cr_earned_10_math = HS_CRDMTHERN_10,
    HS_total_cr_earned_10_eng = HS_CRDENGERN_10,
    HS_total_cr_earned_10_sci = HS_CRDSCIERN_10,
    HS_gpa_10 = HS_CRSTOTGPAW_10,
    HS_gpa_nacad_10 = HS_CRSNACGPAW_10,
    HS_gpa_acad_10 = HS_CRSACDGPAW_10,
    HS_gpa_math_10 = HS_CRSMTHGPAW_10,
    HS_gpa_eng_10 = HS_CRSENGGPAW_10,
    HS_gpa_sci_10 = HS_CRSSCIGPAW_10,
    # 11th grade
    HS_total_cr_earned_11 = HS_CRDTOTERN_11,
    HS_total_cr_earned_11_acad = HS_CRDACDERN_11,
    HS_total_cr_earned_11_math = HS_CRDMTHERN_11,
    HS_total_cr_earned_11_eng = HS_CRDENGERN_11,
    HS_total_cr_earned_11_sci = HS_CRDSCIERN_11,
    HS_gpa_11 = HS_CRSTOTGPAW_11,
    HS_gpa_nacad_11 = HS_CRSNACGPAW_11,
    HS_gpa_acad_11 = HS_CRSACDGPAW_11,
    HS_gpa_math_11 = HS_CRSMTHGPAW_11,
    HS_gpa_eng_11 = HS_CRSENGGPAW_11,
    HS_gpa_sci_11 = HS_CRSSCIGPAW_11,
    
    ####### HS suspensions (total days)
    HS_susp_09 = BEHAV_SUSTOTDAYS_09,
    HS_susp_10 = BEHAV_SUSTOTDAYS_10)

dat <- dat %>%
  mutate(MS_pct_cr_earned_07 = MS_total_cr_earned_07 / MS_CREDATMTOT_07,
         MS_pct_cr_earned_acad_07 = MS_total_cr_earned_acad_07 / MS_CREDATMACD_07,
         MS_pct_acad_total_07 = MS_total_cr_earned_acad_07 / MS_total_cr_earned_07,
         
         # 8th grade
         MS_pct_cr_earned_08 = MS_total_cr_earned_08 / MS_CREDATMTOT_08,
         MS_pct_cr_earned_acad_08 = MS_total_cr_earned_acad_08 / MS_CREDATMACD_08,
         MS_pct_acad_total_08 = MS_total_cr_earned_acad_08 / MS_total_cr_earned_08,
         
         # 9th grade
         HS_pct_cr_earned_09 = HS_total_cr_earned_09 / HS_CRDTOTATM_09,
         HS_pct_cr_earned_math_09 = HS_total_cr_earned_09_math / HS_CRDMTHATM_09,
         HS_pct_cr_earned_eng_09 = HS_total_cr_earned_09_eng / HS_CRDENGATM_09,
         HS_pct_cr_earned_sci_09 = HS_total_cr_earned_09_sci / HS_CRDSCIATM_09,
         HS_pct_cr_earned_nacad_09 = HS_CRDNACERN_09 / HS_CRDNACATM_09,
         HS_pct_cr_earned_acad_09 = HS_total_cr_earned_09_acad / HS_CRDACDATM_09,
         HS_pct_acad_total_09 = HS_total_cr_earned_09_acad / HS_total_cr_earned_09,
         HS_pct_math_total_09 = HS_total_cr_earned_09_math / HS_total_cr_earned_09,
         HS_pct_eng_total_09 = HS_total_cr_earned_09_eng / HS_total_cr_earned_09,
         HS_pct_sci_total_09 = HS_total_cr_earned_09_sci / HS_total_cr_earned_09,
         
         # 10th grad
         HS_pct_cr_earned_10 = HS_total_cr_earned_10 / HS_CRDTOTATM_10,
         HS_pct_cr_earned_math_10 = HS_total_cr_earned_10_math / HS_CRDMTHATM_10,
         HS_pct_cr_earned_eng_10 = HS_total_cr_earned_10_eng / HS_CRDENGATM_10,
         HS_pct_cr_earned_sci_10 = HS_total_cr_earned_10_sci / HS_CRDSCIATM_10,
         HS_pct_cr_earned_nacad_10 = HS_CRDNACERN_10 / HS_CRDNACATM_10,
         HS_pct_cr_earned_acad_10 = HS_total_cr_earned_10_acad / HS_CRDACDATM_10,
         HS_pct_acad_total_10 = HS_total_cr_earned_10_acad / HS_total_cr_earned_10,
         HS_pct_math_total_10 = HS_total_cr_earned_10_math / HS_total_cr_earned_10,
         HS_pct_eng_total_10 = HS_total_cr_earned_10_eng / HS_total_cr_earned_10,
         HS_pct_sci_total_10 = HS_total_cr_earned_10_sci / HS_total_cr_earned_10,
         
         # 11th grad
         HS_pct_cr_earned_11 = HS_total_cr_earned_11 / HS_CRDTOTATM_11,
         HS_pct_cr_earned_math_11 = HS_total_cr_earned_11_math / HS_CRDMTHATM_11,
         HS_pct_cr_earned_eng_11 = HS_total_cr_earned_11_eng / HS_CRDENGATM_11,
         HS_pct_cr_earned_sci_11 = HS_total_cr_earned_11_sci / HS_CRDSCIATM_11,
         HS_pct_cr_earned_nacad_11 = HS_CRDNACERN_11 / HS_CRDNACATM_11,
         HS_pct_cr_earned_acad_11 = HS_total_cr_earned_11_acad / HS_CRDACDATM_11,
         HS_pct_acad_total_11 = HS_total_cr_earned_11_acad / HS_total_cr_earned_11,
         HS_pct_math_total_11 = HS_total_cr_earned_11_math / HS_total_cr_earned_11,
         HS_pct_eng_total_11 = HS_total_cr_earned_11_eng / HS_total_cr_earned_11,
         HS_pct_sci_total_11 = HS_total_cr_earned_11_sci / HS_total_cr_earned_11)

# Multiply pct variables by 100.
multiply_var = function(x){
  x <- 100 * x
  return(x)
}
dat <- dat %>%
  mutate(across(.cols = starts_with("HS_pct"),
                .fns = ~multiply_var(.x)))
dat <- dat %>%
  mutate(across(.cols = starts_with("MS_pct"),
                .fns = ~multiply_var(.x)))

#==============================================================
# Missing values in the new variables
#==============================================================

x = pct_missing_variable(dat)
print(as_tibble(x), n=400)

dat[sapply(dat, is.infinite)] <- NA

#==============================================================
# Delete variables which have not been renames
#==============================================================

dat <- dat %>%
  select(-starts_with("HS_CR"),
         -starts_with("MS_CR"),
         -starts_with("MS_MTH"),
         -starts_with("MS_ELAG"),
         -starts_with("MS_ELAS"),
         -starts_with("MS_ELAP"),
         -starts_with("MS_ELAT"))

dat <- dat %>%
  select(-starts_with("BEHAV"))

# to end printed output
#sink()

#==============================================================
# Save
#==============================================================
save(dat, file = paste0(DOE_server_wd_data,"/dataset_8_cohort-12-13.Rdata"))