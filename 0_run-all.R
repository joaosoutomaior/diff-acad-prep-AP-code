#==============================================================
# File description
#==============================================================
# contents: 
#  runs all code to replicate results in the paper
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Stored functions, values, and paths
#==============================================================

source("header_basic.R")

#==============================================================
# Selection of samples of high schools
#==============================================================

source(paste0(wd,"/1_high-school-sample-selection.R"), print.eval = T)

#==============================================================
# DOE data cleaning
#==============================================================

# ---> run the following code through the server for faster results
#      make sure to set the correct directory before running

# server-specific header (it is an old version of R --- limited set of packages available)
setwd(DOE_server_wd)
source("header_server.R")

# variables 2011-12 cohort
source(paste0(DOE_server_wd_code,"/2_data-cleaning-a_11-12-cohort_enrollment-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-b_11-12-cohort_test-scores-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-c_11-12-cohort_courses-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-d_11-12-cohort_more-acad-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-e_11-12-cohort_attendance-vars.R"), print.eval = T)

# variables 2012-13 cohort
source(paste0(DOE_server_wd_code,"/2_data-cleaning-a_12-13-cohort_enrollment-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-b_12-13-cohort_test-scores-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-c_12-13-cohort_courses-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-d_12-13-cohort_more-acad-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-e_12-13-cohort_attendance-vars.R"), print.eval = T)

# school-level filter
source(paste0(DOE_server_wd_code,"/2_data-cleaning-f_both-cohorts_school-level-filter.R"), print.eval = T)

# student-level filter --- 2011-12 cohort
source(paste0(DOE_server_wd_code,"/2_data-cleaning-g_11-12-cohort_student-level-filter.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-h_11-12-cohort_adjust-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-i_11-12-cohort_composition-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-j_11-12_save-full-grades.R"), print.eval = T)

# student-level filter --- 2012-13 cohort 
source(paste0(DOE_server_wd_code,"/2_data-cleaning-g_12-13-cohort_student-level-filter.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-h_12-13-cohort_adjust-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-i_12-13-cohort_composition-vars.R"), print.eval = T)
source(paste0(DOE_server_wd_code,"/2_data-cleaning-j_12-13_save-full-grades.R"), print.eval = T)

#============================================================================
# Assessment of sample restrictions
#============================================================================

source(paste0(wd,"/3_assessment-of-sample-restrictions.R"), print.eval = T)

#============================================================================
# Examination of sample
#============================================================================

source(paste0(wd,"/4_sample-description.R"), print.eval = T)

#============================================================================
# Additional checks
#============================================================================

source(paste0(wd,"/5_courses-exams-distributions.R"), print.eval = T)

#============================================================================
# Estimation of course enrollment and exam passage machine learning models
#============================================================================

source(paste0(wd,"/6_enrollment-and-passage-models.R"), print.eval = T)

#============================================================================
# Preparedness-adjusted model and comparisons
#============================================================================

source(paste0(wd,"/7_prep-adjusted-model-and-comparisons.R"), print.eval = T)

#============================================================================
# Preparedness-adjusted model and comparisons
#============================================================================

source(paste0(wd,"/8_plot-estimates.R"), print.eval = T)


