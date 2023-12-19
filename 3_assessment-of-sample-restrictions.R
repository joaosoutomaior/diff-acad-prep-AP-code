#==============================================================
# File description
#==============================================================
# contents: 
#  assessment of our sample restrictions
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up 
#==============================================================

rm(list=ls())

# additional packages
library(kableExtra)
library(mltools)
library(data.table)
library(modelr)
library(caret)

# usual header
source("header_basic.R")
source("header_parameters.R")

#==============================================================
# Assessment of our sample restrictions (seniors in 2016)
#==============================================================

# final sample
load(paste0(DOE_wd,"/dataset_final.Rdata"))

# look only at the 2012 cohort
dat_all <- dat_all %>%
  filter(cohort == 2012)

school_IDs <- dat_all %>%
  select(ID_school) %>%
  distinct(ID_school)

school_vector <- school_IDs$ID_school

# full senior data for 2015-16
load(paste0(DOE_wd,"/dataset_check_all-g12-2015-16.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g12, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g12_2015-16.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2016 = ", mean(dat_all$AP_math_enroll_12))
paste0("number of students in the sample who take at least one AP math in 2016 = ", sum(dat_all$AP_math_enroll_12))
paste0("rate at which all 2016 seniors take at least one AP math = ", mean(dat_check$AP_math_enroll_12))
paste0("number of all 2016 seniors who take at least one AP math = ", sum(dat_check$AP_math_enroll_12))
paste0("fraction of sample AP math takers over all seniors AP math takers = ", sum(dat_all$AP_math_enroll_12) / sum(dat_check$AP_math_enroll_12))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2016 = ", mean(dat_all$AP_math_exam_12))
paste0("number of students in the sample who take at least one AP math exam in 2016 = ", sum(dat_all$AP_math_exam_12))
paste0("rate at which all 2016 seniors take at least one AP math exam = ", mean(dat_check$AP_math_exam_12))
paste0("number of all 2016 seniors who take at least one AP math exam = ", sum(dat_check$AP_math_exam_12))
paste0("fraction of students in the sample who take (...) over all seniors in 2016 (...) = ", sum(dat_all$AP_math_exam_12) / sum(dat_check$AP_math_exam_12))

sink()

#==============================================================
# Assessment of our sample restrictions (juniors in 2015)
#==============================================================

# full junior data for 2013-14
load(paste0(DOE_wd,"/dataset_check_all-g11-2014-15.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g11, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g11_2014-15.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2015 = ", mean(dat_all$AP_math_enroll_11))
paste0("number of students in the sample who take at least one AP math in 2015 = ", sum(dat_all$AP_math_enroll_11))
paste0("rate at which all 2015 juniors take at least one AP math = ", mean(dat_check$AP_math_enroll_11))
paste0("number of all 2015 juniors who take at least one AP math = ", sum(dat_check$AP_math_enroll_11))
paste0("fraction of sample AP math takers over all juniors AP math takers = ", sum(dat_all$AP_math_enroll_11) / sum(dat_check$AP_math_enroll_11))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2015 = ", mean(dat_all$AP_math_exam_11))
paste0("number of students in the sample who take at least one AP math exam in 2015 = ", sum(dat_all$AP_math_exam_11))
paste0("rate at which all 2015 juniors take at least one AP math exam = ", mean(dat_check$AP_math_exam_11))
paste0("number of all 2015 juniors who take at least one AP math exam = ", sum(dat_check$AP_math_exam_11))
paste0("fraction of students in the sample who take (...) over all juniors in 2015 (...) = ", sum(dat_all$AP_math_exam_11) / sum(dat_check$AP_math_exam_11))

sink()

#==============================================================
# Assessment of our sample restrictions (sophomores in 2014)
#==============================================================

# full sophomore data for 2013-14
load(paste0(DOE_wd,"/dataset_check_all-g10-2013-14.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g10, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g10_2013-14.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2014 = ", mean(dat_all$AP_math_enroll_10))
paste0("number of students in the sample who take at least one AP math in 2014 = ", sum(dat_all$AP_math_enroll_10))
paste0("rate at which all 2014 sophomores take at least one AP math = ", mean(dat_check$AP_math_enroll_10))
paste0("number of all 2014 sophomores who take at least one AP math = ", sum(dat_check$AP_math_enroll_10))
paste0("fraction of sample AP math takers over all sophomores AP math takers = ", sum(dat_all$AP_math_enroll_10) / sum(dat_check$AP_math_enroll_10))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2014 = ", mean(dat_all$AP_math_exam_10))
paste0("number of students in the sample who take at least one AP math exam in 2014 = ", sum(dat_all$AP_math_exam_10))
paste0("rate at which all 2014 sophomores take at least one AP math exam = ", mean(dat_check$AP_math_exam_10))
paste0("number of all 2014 sophomores who take at least one AP math exam = ", sum(dat_check$AP_math_exam_10))
paste0("fraction of students in the sample who take (...) over all sophomores in 2014 (...) = ", sum(dat_all$AP_math_exam_10) / sum(dat_check$AP_math_exam_10))

sink()

#==============================================================
# Assessment of our sample restrictions (freshmen in 2013)
#==============================================================

# full freshmen data for 2012-13
load(paste0(DOE_wd,"/dataset_check_all-g09-2012-13.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g09, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g09_2012-13.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2013 = ", mean(dat_all$AP_math_enroll_09))
paste0("number of students in the sample who take at least one AP math in 2013 = ", sum(dat_all$AP_math_enroll_09))
paste0("rate at which all 2013 freshmen take at least one AP math = ", mean(dat_check$AP_math_enroll_09))
paste0("number of all 2013 freshmen who take at least one AP math = ", sum(dat_check$AP_math_enroll_09))
paste0("fraction of sample AP math takers over all freshmen AP math takers = ", sum(dat_all$AP_math_enroll_09) / sum(dat_check$AP_math_enroll_09))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2013 = ", mean(dat_all$AP_math_exam_09))
paste0("number of students in the sample who take at least one AP math exam in 2013 = ", sum(dat_all$AP_math_exam_09))
paste0("rate at which all 2013 freshmen take at least one AP math exam = ", mean(dat_check$AP_math_exam_09))
paste0("number of all 2013 freshmen who take at least one AP math exam = ", sum(dat_check$AP_math_exam_09))
paste0("fraction of students in the sample who take (...) over all freshmen in 2013 (...) = ", sum(dat_all$AP_math_exam_09) / sum(dat_check$AP_math_exam_09))

sink()

#==============================================================
# Begin for 2011 cohort
#==============================================================

# ---> year-based subscripts for variables created in below (e.g., "_13") are copied from the 2012 cohort. 
#      for the correct years, subtract 1.

#==============================================================
# Assessment of our sample restrictions (seniors in 2015)
#==============================================================

# final sample
load(paste0(DOE_wd,"/dataset_final.Rdata"))

# look only at the 2012 cohort
dat_all <- dat_all %>%
  filter(cohort == 2012)

school_IDs <- dat_all %>%
  select(ID_school) %>%
  distinct(ID_school)

school_vector <- school_IDs$ID_school

# full senior data for 2015-16
load(paste0(DOE_wd,"/dataset_check_all-g12-2014-15.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g12, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g12_2014-15.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2016 = ", mean(dat_all$AP_math_enroll_12))
paste0("number of students in the sample who take at least one AP math in 2016 = ", sum(dat_all$AP_math_enroll_12))
paste0("rate at which all 2016 seniors take at least one AP math = ", mean(dat_check$AP_math_enroll_12))
paste0("number of all 2016 seniors who take at least one AP math = ", sum(dat_check$AP_math_enroll_12))
paste0("fraction of sample AP math takers over all seniors AP math takers = ", sum(dat_all$AP_math_enroll_12) / sum(dat_check$AP_math_enroll_12))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2016 = ", mean(dat_all$AP_math_exam_12))
paste0("number of students in the sample who take at least one AP math exam in 2016 = ", sum(dat_all$AP_math_exam_12))
paste0("rate at which all 2016 seniors take at least one AP math exam = ", mean(dat_check$AP_math_exam_12))
paste0("number of all 2016 seniors who take at least one AP math exam = ", sum(dat_check$AP_math_exam_12))
paste0("fraction of students in the sample who take (...) over all seniors in 2016 (...) = ", sum(dat_all$AP_math_exam_12) / sum(dat_check$AP_math_exam_12))

sink()

#==============================================================
# Assessment of our sample restrictions (juniors in 2014)
#==============================================================

# full junior data for 2013-14
load(paste0(DOE_wd,"/dataset_check_all-g11-2013-14.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g11, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g11_2013-14.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2015 = ", mean(dat_all$AP_math_enroll_11))
paste0("number of students in the sample who take at least one AP math in 2015 = ", sum(dat_all$AP_math_enroll_11))
paste0("rate at which all 2015 juniors take at least one AP math = ", mean(dat_check$AP_math_enroll_11))
paste0("number of all 2015 juniors who take at least one AP math = ", sum(dat_check$AP_math_enroll_11))
paste0("fraction of sample AP math takers over all juniors AP math takers = ", sum(dat_all$AP_math_enroll_11) / sum(dat_check$AP_math_enroll_11))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2015 = ", mean(dat_all$AP_math_exam_11))
paste0("number of students in the sample who take at least one AP math exam in 2015 = ", sum(dat_all$AP_math_exam_11))
paste0("rate at which all 2015 juniors take at least one AP math exam = ", mean(dat_check$AP_math_exam_11))
paste0("number of all 2015 juniors who take at least one AP math exam = ", sum(dat_check$AP_math_exam_11))
paste0("fraction of students in the sample who take (...) over all juniors in 2015 (...) = ", sum(dat_all$AP_math_exam_11) / sum(dat_check$AP_math_exam_11))

sink()

#==============================================================
# Assessment of our sample restrictions (sophomores in 2013)
#==============================================================

# full sophomore data for 2013-14
load(paste0(DOE_wd,"/dataset_check_all-g10-2012-13.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g10, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g10_2012-13.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2014 = ", mean(dat_all$AP_math_enroll_10))
paste0("number of students in the sample who take at least one AP math in 2014 = ", sum(dat_all$AP_math_enroll_10))
paste0("rate at which all 2014 sophomores take at least one AP math = ", mean(dat_check$AP_math_enroll_10))
paste0("number of all 2014 sophomores who take at least one AP math = ", sum(dat_check$AP_math_enroll_10))
paste0("fraction of sample AP math takers over all sophomores AP math takers = ", sum(dat_all$AP_math_enroll_10) / sum(dat_check$AP_math_enroll_10))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2014 = ", mean(dat_all$AP_math_exam_10))
paste0("number of students in the sample who take at least one AP math exam in 2014 = ", sum(dat_all$AP_math_exam_10))
paste0("rate at which all 2014 sophomores take at least one AP math exam = ", mean(dat_check$AP_math_exam_10))
paste0("number of all 2014 sophomores who take at least one AP math exam = ", sum(dat_check$AP_math_exam_10))
paste0("fraction of students in the sample who take (...) over all sophomores in 2014 (...) = ", sum(dat_all$AP_math_exam_10) / sum(dat_check$AP_math_exam_10))

sink()

#==============================================================
# Assessment of our sample restrictions (freshmen in 2012)
#==============================================================

# full freshmen data for 2012-13
load(paste0(DOE_wd,"/dataset_check_all-g09-2011-12.Rdata"))

# merge with final sample of schools
dat_check <- left_join(school_IDs, dat_g09, by = "ID_school")

# compare sample in 2016 (seniors) and all 2016 seniors
sink("output/assessment-of-sample-restrictions_g09_2011-12.txt")

# course enrollment
paste0("rate at which students in the sample take at least one AP math in 2013 = ", mean(dat_all$AP_math_enroll_09))
paste0("number of students in the sample who take at least one AP math in 2013 = ", sum(dat_all$AP_math_enroll_09))
paste0("rate at which all 2013 freshmen take at least one AP math = ", mean(dat_check$AP_math_enroll_09))
paste0("number of all 2013 freshmen who take at least one AP math = ", sum(dat_check$AP_math_enroll_09))
paste0("fraction of sample AP math takers over all freshmen AP math takers = ", sum(dat_all$AP_math_enroll_09) / sum(dat_check$AP_math_enroll_09))

# exam participation
paste0("rate at which students in the sample take at least one AP math exam in 2013 = ", mean(dat_all$AP_math_exam_09))
paste0("number of students in the sample who take at least one AP math exam in 2013 = ", sum(dat_all$AP_math_exam_09))
paste0("rate at which all 2013 freshmen take at least one AP math exam = ", mean(dat_check$AP_math_exam_09))
paste0("number of all 2013 freshmen who take at least one AP math exam = ", sum(dat_check$AP_math_exam_09))
paste0("fraction of students in the sample who take (...) over all freshmen in 2013 (...) = ", sum(dat_all$AP_math_exam_09) / sum(dat_check$AP_math_exam_09))

sink()