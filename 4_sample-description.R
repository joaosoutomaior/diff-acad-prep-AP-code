#==============================================================
# File description
#==============================================================
# contents: 
#  examination of student-level filters
#  description of resulting samples of interest
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up 
#==============================================================

rm(list=ls())
useViewer = FALSE

# additional packages
library(kableExtra)
library(mltools)
library(data.table)
library(modelr)
library(caret)

# usual header
source("header_basic.R")
source("header_parameters.R")
source("header_plotting-functions.R")

#==============================================================
# Load clean DOE data
#==============================================================

# --> Note that the "anti" subscript defines the students 
#     not included in the cohort of interest 

load(paste0(DOE_wd,"/dataset_7_cohort-11-12_anti.Rdata"))
dat_ret_11 <- dat_ret
load(paste0(DOE_wd,"/dataset_7_cohort-12-13_anti.Rdata"))
dat_ret_12 <- dat_ret
load(paste0(DOE_wd,"/dataset_9_cohort-11-12.Rdata"))
dat_11 <- dat
load(paste0(DOE_wd,"/dataset_9_cohort-12-13.Rdata"))
dat_12 <- dat
rm(dat)
rm(dat_ret)

# useful to rename the race variable
dat_12 <- dat_12 %>%
  mutate(cohort = 2012,
         race = factor(BIO_race_factor,
                       levels = c("White", "Black", "Asian", "Hispanic", "Other")))
dat_11 <- dat_11 %>%
  mutate(cohort = 2011,
         race = factor(BIO_race_factor,
                       levels = c("White", "Black", "Asian", "Hispanic", "Other")))

#==============================================================
# Number of students and schools in the final sample
#==============================================================

# report number of schools
paste("Number of schools in the 2012 cohort: ", length(unique(dat_12$ID_school)))
paste("Number of schools in the 2011 cohort: ", length(unique(dat_11$ID_school)))

# combine samples again
dat_all <- rbind(dat_12, dat_11)

# why do the number of students within schools vary so much?
freq_table(dat_all$HS_math_alg2_n_courses_09)
freq_table(dat_all$HS_math_alg2_n_pass_10)

dat_check <- dat_all %>%
  mutate(cohort_over_total_9g = SCH_cohort_HS_size / SCH_HS_size_9g)

my_hist(dat_check$cohort_over_total_9g, dat_check, "N. students in selected HS cohort / \nN. students in 9th grade")

# print results if needed
# ggsave("figures/fig_cohort-over-total_9g.png", width = 25, height = 15, units = "cm")

#==============================================================
# Description of curriculum trajectories
#==============================================================

# description of those following a standard grade promotion trajectory
paste("Regular curriculum trajectory")

paste("2012")

tot = nrow(dat_12)
tab <- data.frame(
  AP_enroll = c(sum(dat_12$AP_math_enroll_09),
             sum(dat_12$AP_math_enroll_10),
             sum(dat_12$AP_math_enroll_11),
             sum(dat_12$AP_math_enroll_12)),
  AP_exam = c(sum(dat_12$AP_math_exam_09),
             sum(dat_12$AP_math_exam_10),
             sum(dat_12$AP_math_exam_11),
             sum(dat_12$AP_math_exam_12)),
  group = c("2012-13",
            "2013-14",
            "2014-15",
            "2015-16"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2), "%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2),"%"))%>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

paste("2011")

tot = nrow(dat_11)
tab <- data.frame(
  AP_enroll = c(sum(dat_11$AP_math_enroll_09),
             sum(dat_11$AP_math_enroll_10),
             sum(dat_11$AP_math_enroll_11),
             sum(dat_11$AP_math_enroll_12)),
  AP_exam = c(sum(dat_11$AP_math_exam_09),
             sum(dat_11$AP_math_exam_10),
             sum(dat_11$AP_math_exam_11),
             sum(dat_11$AP_math_exam_12)),
  group = c("2011-12",
            "2012-13",
            "2013-14",
            "2014-15"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2), "%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2),"%"))%>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

# description of those following other curriculum trajectories
paste("Non-regular curriculum trajectory")
paste("2012")

tot = nrow(dat_ret_12)
tab <- data.frame(
  AP_enroll = c(sum(dat_ret_12$AP_math_enroll_09),
             sum(dat_ret_12$AP_math_enroll_10),
             sum(dat_ret_12$AP_math_enroll_11),
             sum(dat_ret_12$AP_math_enroll_12)),
    AP_exam = c(sum(dat_ret_12$AP_math_exam_09),
             sum(dat_ret_12$AP_math_exam_10),
             sum(dat_ret_12$AP_math_exam_11),
             sum(dat_ret_12$AP_math_exam_12)),
  group = c("2012-13",
            "2013-14",
            "2014-15",
            "2015-16"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2), "%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2), "%"))%>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

paste("2011")

tot = nrow(dat_ret_11)
tab <- data.frame(
  AP_enroll = c(sum(dat_ret_11$AP_math_enroll_09),
             sum(dat_ret_11$AP_math_enroll_10),
             sum(dat_ret_11$AP_math_enroll_11),
             sum(dat_ret_11$AP_math_enroll_12)),
    AP_exam = c(sum(dat_ret_11$AP_math_exam_09),
             sum(dat_ret_11$AP_math_exam_10),
             sum(dat_ret_11$AP_math_exam_11),
             sum(dat_ret_11$AP_math_exam_12)),
  group = c("2011-12",
            "2012-13",
            "2013-14",
            "2014-15"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2), "%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2), "%"))%>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

#==============================================================
# AP exam and course taking rates across trajectories
#==============================================================

# save outputs
# sink("output/descriptives-across-trajectories.txt")
# ---> We do not provide the printed output due to small cell values
#      which need to be suppressed

paste("AP exam taking and enrollment among students enrolled")
paste("2012")

dat_s <- dat_12 %>%
  filter(AP_math_enroll == 1)

tot = nrow(dat_s)
tab <- data.frame(
  AP_enroll = c(sum(dat_s$AP_math_enroll_09),
             sum(dat_s$AP_math_enroll_10),
             sum(dat_s$AP_math_enroll_11),
             sum(dat_s$AP_math_enroll_12)),
  AP_exam = c(sum(dat_s$AP_math_exam_09),
             sum(dat_s$AP_math_exam_10),
             sum(dat_s$AP_math_exam_11),
             sum(dat_s$AP_math_exam_12)),
  group = c("2012-13",
            "2013-14",
            "2014-15",
            "2015-16"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2),"%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2),"%")) %>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

paste("2011")

dat_s <- dat_11 %>%
  filter(AP_math_enroll == 1)

tot = nrow(dat_s)
tab <- data.frame(
  AP_enroll = c(sum(dat_s$AP_math_enroll_09),
             sum(dat_s$AP_math_enroll_10),
             sum(dat_s$AP_math_enroll_11),
             sum(dat_s$AP_math_enroll_12)),
  AP_exam = c(sum(dat_s$AP_math_exam_09),
             sum(dat_s$AP_math_exam_10),
             sum(dat_s$AP_math_exam_11),
             sum(dat_s$AP_math_exam_12)),
  group = c("2011-12",
            "2012-13",
            "2013-14",
            "2014-15"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2),"%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2),"%")) %>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

paste("AP exam taking and enrollment among students not enrolled")
paste("2012")

dat_s <- dat_12 %>%
  filter(AP_math_enroll == 0)

tot = nrow(dat_s)
tab <- data.frame(
  AP_enroll = c(sum(dat_s$AP_math_enroll_09),
             sum(dat_s$AP_math_enroll_10),
             sum(dat_s$AP_math_enroll_11),
             sum(dat_s$AP_math_enroll_12)),
  AP_exam = c(sum(dat_s$AP_math_exam_09),
             sum(dat_s$AP_math_exam_10),
             sum(dat_s$AP_math_exam_11),
             sum(dat_s$AP_math_exam_12)),
  group = c("2012-13",
            "2013-14",
            "2014-15",
            "2015-16"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2),"%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2),"%")) %>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

paste("2011")

dat_s <- dat_11 %>%
  filter(AP_math_enroll == 0)

tot = nrow(dat_s)
tab <- data.frame(
  AP_enroll = c(sum(dat_s$AP_math_enroll_09),
             sum(dat_s$AP_math_enroll_10),
             sum(dat_s$AP_math_enroll_11),
             sum(dat_s$AP_math_enroll_12)),
  AP_exam = c(sum(dat_s$AP_math_exam_09),
             sum(dat_s$AP_math_exam_10),
             sum(dat_s$AP_math_exam_11),
             sum(dat_s$AP_math_exam_12)),
  group = c("2011-12",
            "2012-13",
            "2013-14",
            "2014-15"))
tab = as_tibble(tab)
tab = tab %>%
  mutate(pct_enroll = paste0(round(100 * AP_enroll / tot, 2),"%"),
         pct_exam = paste0(round(100 * AP_exam / tot, 2),"%")) %>%
  select(group, AP_enroll, pct_enroll, AP_exam, pct_exam)
t <- kbl(tab, "latex", booktabs = T, align = "l")
t

# sink()
# ---> We do not provide the printed output due to small cell values
#      which need to be suppressed

#==============================================================
# Check on alternative operationlizations
#==============================================================

# save outputs
sink("output/alternative-operationalization.txt")

dat_all_v2 <- dat_all %>%
  mutate(
    AP_condition1 = case_when(
    (AP_math_pass_ratio_12 == 1) ~ 1, 
    TRUE ~ 0),
    AP_condition2 = case_when(
    (AP_math_exam_n_12 == 0 & AP_math_pass_ratio_11 == 1) ~ 1, 
    TRUE ~ 0),
    AP_math_pass = case_when(
    (AP_condition1 == 1 | AP_condition2 == 1) ~ 1, 
    TRUE ~ 0),
    AP_math_pass_logical = as.logical(AP_math_pass))

dat_all_v3 <- dat_all %>%
  mutate(
    AP_math_pass = case_when(
    (AP_math_exam_n_12 > 0 & AP_math_exam_n_11 > 0 &
       AP_math_pass_ratio_11 == 1 & AP_math_pass_ratio_12 == 1) ~ 1,
    (AP_math_exam_n_12 > 0 & AP_math_exam_n_11 == 0 &
       AP_math_pass_ratio_12 == 1) ~ 1,
    (AP_math_exam_n_12 == 0 & AP_math_exam_n_11 > 0 &
       AP_math_pass_ratio_11 == 1) ~ 1,
    TRUE ~ 0),
    AP_math_pass_logical = as.logical(AP_math_pass))

dat_all_new <- dat_all[which(dat_all$AP_math_pass != dat_all_v2$AP_math_pass), ]
dat_all_new2 <- dat_all[which(dat_all$AP_math_pass != dat_all_v3$AP_math_pass), ]

# assessments of changes in the AP_math_pass variable
paste("AP_math_pass variable under alternative operationalizations")
freq_table(dat_all_v2$AP_math_pass)
freq_table(dat_all_v3$AP_math_pass)

paste("AP_math_pass variable under choosen operationalization")
freq_table(dat_all$AP_math_pass)

paste("Number of columns which change with the new operationalization 1: ", nrow(dat_all_new))
paste("Pct. of columns which change with the new operationalization 1: ", nrow(dat_all_new) / nrow(dat_all))
paste("Number of columns which change with the new operationalization 1: ", nrow(dat_all_new2))
paste("Pct. of columns which change with the new operationalization 1: ", nrow(dat_all_new2) / nrow(dat_all))

sink()

#==============================================================
# Enrollment rates both cohorts (standard curriculum trajectory)
#==============================================================

# Description of those following a regular curriculum trajectory
paste("Enrollment patterns")
paste("Both cohorts")

# all students
ap_enroll_all = dat_all %>%
  select(AP_math_enroll) %>%
  group_by(AP_math_enroll) %>%
  summarise(N = n()) %>%
  mutate("Pct. enrolled in at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_enroll == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. enrolled in at least one AP math course")

# by race + add a row for all
ap_enroll = dat_all %>%
  select(race, AP_math_enroll) %>%
  group_by(race, AP_math_enroll) %>%
  summarise(N = n()) %>%
  mutate("Pct. enrolled in at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_enroll == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. enrolled in at least one AP math course") %>%
  as_tibble() %>%
  add_row(ap_enroll_all, .before = 1)
ap_enroll

#==============================================================
# Exam-taking rates both cohorts (standard curriculum trajectory)
#==============================================================

# all students 
exam_part_all = dat_all %>%
  filter(AP_math_enroll == 1) %>%
  select(AP_math_exam) %>%
  group_by(AP_math_exam) %>%
  summarise(N = n()) %>%
  mutate("Pct. attempted at least one AP math exam, given course enrollment" = prop.table(N)) %>%
  filter(AP_math_exam == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. attempted at least one AP math exam, given course enrollment")

# by race + add a row for all
exam_part = dat_all %>%
  filter(AP_math_enroll == 1) %>%
  select(race, AP_math_exam) %>%
  group_by(race, AP_math_exam) %>%
  summarise(N = n()) %>%
  mutate("Pct. attempted at least one AP math exam, given course enrollment" = prop.table(N)) %>%
  filter(AP_math_exam == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. attempted at least one AP math exam, given course enrollment") %>%
  as_tibble() %>%
  add_row(exam_part_all, .before = 1)
exam_part

#==============================================================
# Exam passage rates both cohorts (standard curriculum trajectory)
#==============================================================

# all students
exam_pass_all = dat_all %>%
  filter(AP_math_enroll == 1,
         AP_math_exam == 1) %>%
  select(AP_math_pass) %>%
  group_by(AP_math_pass) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math exam, given exam participation" = prop.table(N)) %>%
  filter(AP_math_pass == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. passed at least one AP math exam, given exam participation")

# by race + add a row for all
exam_pass = dat_all %>%
  filter(AP_math_enroll == 1,
         AP_math_exam == 1) %>%
  select(race, AP_math_pass) %>%
  group_by(race, AP_math_pass) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math exam, given exam participation" = prop.table(N)) %>%
  filter(AP_math_pass == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. passed at least one AP math exam, given exam participation") %>%
  as_tibble() %>%
  add_row(exam_pass_all, .before = 1)
exam_pass

#==============================================================
# Course passage rates both cohorts (standard curriculum trajectory)
#==============================================================

# all students
course_pass_all = dat_all %>%
  filter(AP_math_enroll == 1) %>%
  select(AP_math_pass_course) %>%
  group_by(AP_math_pass_course) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_pass_course == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. passed at least one AP math course")
course_pass_all

# by race + add a row for race
course_pass = dat_all %>%
  filter(AP_math_enroll == 1)%>%
  select(race, AP_math_pass_course) %>%
  group_by(race, AP_math_pass_course) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_pass_course == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. passed at least one AP math course") %>%
  as_tibble() %>%
  add_row(course_pass_all, .before = 1)

#==============================================================
# Summary table both cohorts (standard curriculum trajectory)
#==============================================================

t1 <- left_join(ap_enroll, exam_part) 
t2 <- left_join(t1, exam_pass)
t <- left_join(t2, course_pass)
t_all <- t %>%
  pivot_longer(cols = starts_with("Pct."),
               names_to = "type") %>%
  mutate(cohort = "both")

# save outputs
sink("output/enrollment-patterns_full-sample.txt")
print(t_all, n = Inf)
sink()

#==============================================================
# Enrollment rates 2012 cohort (standard curriculum trajectory)
#==============================================================

paste("2012 cohort")

# all students
ap_enroll_all = dat_12 %>%
  select(AP_math_enroll) %>%
  group_by(AP_math_enroll) %>%
  summarise(N = n()) %>%
  mutate("Pct. enrolled in at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_enroll == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. enrolled in at least one AP math course")

# by race + add row for all
ap_enroll = dat_12 %>%
  select(race, AP_math_enroll) %>%
  group_by(race, AP_math_enroll) %>%
  summarise(N = n()) %>%
  mutate("Pct. enrolled in at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_enroll == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. enrolled in at least one AP math course") %>%
  as_tibble() %>%
  add_row(ap_enroll_all, .before = 1)
ap_enroll

#==============================================================
# Exam-taking rates 2012 cohort (standard curriculum trajectory)
#==============================================================

# all students
exam_part_all = dat_12 %>%
  filter(AP_math_enroll == 1) %>%
  select(AP_math_exam) %>%
  group_by(AP_math_exam) %>%
  summarise(N = n()) %>%
  mutate("Pct. attempted at least one AP math exam, given course enrollment" = prop.table(N)) %>%
  filter(AP_math_exam == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. attempted at least one AP math exam, given course enrollment")

# by race + add row for all
exam_part = dat_12 %>%
  filter(AP_math_enroll == 1) %>%
  select(race, AP_math_exam) %>%
  group_by(race, AP_math_exam) %>%
  summarise(N = n()) %>%
  mutate("Pct. attempted at least one AP math exam, given course enrollment" = prop.table(N)) %>%
  filter(AP_math_exam == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. attempted at least one AP math exam, given course enrollment") %>%
  as_tibble() %>%
  add_row(exam_part_all, .before = 1)
exam_part

#==============================================================
# Exam passage rates 2012 cohort (standard curriculum trajectory)
#==============================================================

# all students
exam_pass_all = dat_12 %>%
  filter(AP_math_enroll == 1,
         AP_math_exam == 1) %>%
  select(AP_math_pass) %>%
  group_by(AP_math_pass) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math exam, given exam participation" = prop.table(N)) %>%
  filter(AP_math_pass == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. passed at least one AP math exam, given exam participation")

# by race + add row for all
exam_pass = dat_12 %>%
  filter(AP_math_enroll == 1,
         AP_math_exam == 1) %>%
  select(race, AP_math_pass) %>%
  group_by(race, AP_math_pass) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math exam, given exam participation" = prop.table(N)) %>%
  filter(AP_math_pass == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. passed at least one AP math exam, given exam participation") %>%
  as_tibble() %>%
  add_row(exam_pass_all, .before = 1)
exam_pass

#==============================================================
# Course passage rates 2012 cohort (standard curriculum trajectory)
#==============================================================

# all
course_pass_all = dat_12 %>%
  filter(AP_math_enroll == 1) %>%
  select(AP_math_pass_course) %>%
  group_by(AP_math_pass_course) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_pass_course == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. passed at least one AP math course")

# by race + add row for all
course_pass = dat_12 %>%
  filter(AP_math_enroll == 1)%>%
  select(race, AP_math_pass_course) %>%
  group_by(race, AP_math_pass_course) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_pass_course == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. passed at least one AP math course") %>%
  as_tibble() %>%
  add_row(course_pass_all, .before = 1)

#==============================================================
# Combine results for 2011
#==============================================================

t1 <- left_join(ap_enroll, exam_part) 
t2 <- left_join(t1, exam_pass)
t <- left_join(t2, course_pass)
t_cohort1 <- t %>%
  pivot_longer(cols = starts_with("Pct."), 
               names_to = "type") %>%
  mutate(cohort = "2012 sample")

#==============================================================
# Enrollment rates 2011 cohort (standard curriculum trajectory)
#==============================================================

paste("2011")

# all students
ap_enroll_all = dat_11 %>%
  select(AP_math_enroll) %>%
  group_by(AP_math_enroll) %>%
  summarise(N = n()) %>%
  mutate("Pct. enrolled in at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_enroll == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. enrolled in at least one AP math course")

# by race + add row for all
ap_enroll = dat_11 %>%
  select(race, AP_math_enroll) %>%
  group_by(race, AP_math_enroll) %>%
  summarise(N = n()) %>%
  mutate("Pct. enrolled in at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_enroll == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. enrolled in at least one AP math course") %>%
  as_tibble() %>%
  add_row(ap_enroll_all, .before = 1)
ap_enroll

#==============================================================
# Exam-taking rates 2011 cohort (standard curriculum trajectory)
#==============================================================

# all students
exam_part_all = dat_11 %>%
  filter(AP_math_enroll == 1) %>%
  select(AP_math_exam) %>%
  group_by(AP_math_exam) %>%
  summarise(N = n()) %>%
  mutate("Pct. attempted at least one AP math exam, given course enrollment" = prop.table(N)) %>%
  filter(AP_math_exam == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. attempted at least one AP math exam, given course enrollment")

# by race + add row for all
exam_part = dat_11 %>%
  filter(AP_math_enroll == 1) %>%
  select(race, AP_math_exam) %>%
  group_by(race, AP_math_exam) %>%
  summarise(N = n()) %>%
  mutate("Pct. attempted at least one AP math exam, given course enrollment" = prop.table(N)) %>%
  filter(AP_math_exam == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. attempted at least one AP math exam, given course enrollment") %>%
  as_tibble() %>%
  add_row(exam_part_all, .before = 1)
exam_part

#==============================================================
# Exam passage rates 2011 cohort (standard curriculum trajectory)
#==============================================================

# all
exam_pass_all = dat_11 %>%
  filter(AP_math_enroll == 1,
         AP_math_exam == 1) %>%
  select(AP_math_pass) %>%
  group_by(AP_math_pass) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math exam, given exam participation" = prop.table(N)) %>%
  filter(AP_math_pass == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. passed at least one AP math exam, given exam participation")

# by race + add row for all
exam_pass = dat_11 %>%
  filter(AP_math_enroll == 1,
         AP_math_exam == 1) %>%
  select(race, AP_math_pass) %>%
  group_by(race, AP_math_pass) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math exam, given exam participation" = prop.table(N)) %>%
  filter(AP_math_pass == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. passed at least one AP math exam, given exam participation") %>%
  as_tibble() %>%
  add_row(exam_pass_all, .before = 1)
exam_pass

#==============================================================
# Course passage rates 2011 cohort (standard curriculum trajectory)
#==============================================================

# all
course_pass_all = dat_11 %>%
  filter(AP_math_enroll == 1) %>%
  select(AP_math_pass_course) %>%
  group_by(AP_math_pass_course) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_pass_course == 1) %>%
  mutate(Group = "All students") %>%
  select(Group, "Pct. passed at least one AP math course")

# by race + add row for all
course_pass = dat_11 %>%
  filter(AP_math_enroll == 1)%>%
  select(race, AP_math_pass_course) %>%
  group_by(race, AP_math_pass_course) %>%
  summarise(N = n()) %>%
  mutate("Pct. passed at least one AP math course" = prop.table(N)) %>%
  filter(AP_math_pass_course == 1) %>%
  rename(Group = race) %>%
  select(Group, "Pct. passed at least one AP math course") %>%
  as_tibble() %>%
  add_row(course_pass_all, .before = 1)

#==============================================================
# Combine results for 2012
#==============================================================

t1 <- left_join(ap_enroll, exam_part) 
t2 <- left_join(t1, exam_pass)
t <- left_join(t2, course_pass)
t_cohort2 <- t %>%
  pivot_longer(cols = starts_with("Pct."),
               names_to = "type") %>%
  mutate(cohort = "2011 sample")

t <- rbind(t_cohort1, t_cohort2)

#==============================================================
# Summary table by cohort (standard curriculum trajectory)
#==============================================================

sink("output/enrollment-patterns_by-cohort.txt")
print(t, n = Inf)
sink()

#==============================================================
# Complete description for both cohorts
#==============================================================
mycols = c(white, black, asian, hispanic, other)
p1 <- t_all %>%
  mutate(group = factor(Group,
                        levels = c("White", "Black", "Asian", "Hispanic", "Other")),
         type = factor(type, 
                       levels = c("Pct. enrolled in at least one AP math course",
                                  "Pct. attempted at least one AP math exam, given course enrollment",
                                  "Pct. passed at least one AP math exam, given exam participation",
                                  "Pct. passed at least one AP math course"))) %>%
  filter(group != "All students",
         group != "Other",
         type != "Pct. passed at least one AP math course") %>%
  ggplot() +
  geom_point(aes(type, value, 
                 color = group),
             shape = 19,
             stroke = 1,
             alpha = 0.5,
             size = 3) +
  #scale_shape_manual(values = c(0,1,2,3,4)) +
  scale_color_manual(values = mycols) +
  scale_y_continuous('', limits=c(0,1), 
                     labels = function(x) paste0(100 * x, "%"),
                     expand = c(0,0)) +
  scale_x_discrete(name = " ",
                   labels = c("Pct. enrolled\nin at least one\nAP math course",
                                  "Pct. attempted\nat least one\nAP math exam\n(given course\n enrollment)",
                                  "Pct. passed\nat least one\nAP math exam\n(given exam\n participation)")) +
  ggtheme_legend +
  theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))

#==============================================================
# Benchmark test
#==============================================================

# create a school-level data
dat_schools <- dat_all %>%
  select(ID_school, 
         SCH_cohort_pct_ap_white,
         SCH_cohort_pct_ap_black,
         SCH_cohort_n_black,
         SCH_cohort_n_white,
         SCH_cohort_n_ap_pass_black,
         SCH_cohort_n_ap_pass_course_black,
         SCH_cohort_n_ap_black,
         SCH_cohort_n_ap_pass_white,
         SCH_cohort_n_ap_pass_course_white,
         SCH_cohort_n_ap_white,
         SCH_cohort_pct_ap_exam_white,
         SCH_cohort_pct_ap_exam_black,
         SCH_cohort_pct_ap_pass_white,
         SCH_cohort_pct_ap_pass_black,
         SCH_cohort_pct_ap_pass_course_white,
         SCH_cohort_pct_ap_pass_course_black,
         SCH_cohort_HS_size) %>%
  distinct(ID_school, .keep_all = TRUE) %>%
  mutate(SCH_cohort_HS_size = SCH_cohort_HS_size / 50)

# multiply pct variables by 100.
multiply_var = function(x){
  x <- 100 * x
  return(x)
}
dat_schools <- dat_schools %>%
  mutate(across(.cols = starts_with("SCH_cohort_pct"),
                .fns = ~multiply_var(.x)))

# check number of rows with pct ap higher than 0
check <- dat_schools %>%
  filter(SCH_cohort_pct_ap_black > 0,
         SCH_cohort_pct_ap_white > 0)
nrow(check)

# check number of rows with pct ap pass higher than 0
check2 <- dat_schools %>%
  filter(SCH_cohort_pct_ap_pass_black > 0,
         SCH_cohort_pct_ap_pass_white > 0)
nrow(check2)

# plot benchmark
p2 <- plot_benchmark_test(dat_schools,
                    dat_schools$SCH_cohort_pct_ap_white, 
                    dat_schools$SCH_cohort_pct_ap_black, 
                    lim=100,
                    size = dat_schools$SCH_cohort_HS_size)
                    
#==============================================================
# Save fig for AP enrollment patterns + benchmark test
#==============================================================

library(cowplot)
plot_grid(p1, p2, labels = "AUTO", label_y = 1, label_x = 0.05, label_size=15)
ggsave("figures/fig_ap-descriptives_full-sample.png", width = 30, height = 15, units = "cm")

#==============================================================
# Racial composition of the sample 
#==============================================================

# by cohort
comp2 = dat_11 %>%
  select(race) %>%
  group_by(race) %>%
  summarise(N = n()) %>%
  mutate(value = prop.table(N),
         cohort = "2011 sample")
comp1 = dat_12 %>%
  select(race) %>%
  group_by(race) %>%
  summarise(N = n()) %>%
  mutate(value = prop.table(N),
         cohort = "2012 sample")
comp <- rbind(comp1, comp2)

# for the entire data
comp_all = dat_all %>%
  select(race) %>%
  group_by(race) %>%
  summarise(N = n()) %>%
  mutate(value = prop.table(N))


# save outputs
sink("output/racial-composition_full-sample.txt")
print(comp_all, n = Inf)
sink()

sink("output/racial-composition_by-cohort.txt")
print(comp, n = Inf)
sink()

# plot for the entire data
p <- comp_all %>%
  ggplot() +
  geom_bar(stat="identity", 
           aes(x = race, y = value),
           fill =  grayfill,
           color = graycolor) +
  scale_y_continuous('Pct. of students in the sample', limits=c(0,0.5), 
                     labels = function(x) paste0(100 * x, "%")) +
  scale_x_discrete(name ="Ethnoracial group") +
  ggtheme +
  theme(legend.position="top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.background = element_rect(color="black", fill = grayfill, size=1,
                                        linetype="solid"))
p
ggsave("figures/fig_racial-composition_full-sample.png", width = 15, height = 15, units = "cm")

#==============================================================
# Sample adjustment before running model
#==============================================================

# create variable identifying students which took the course only in freshman and sophomore years
dat_all <- dat_all %>%
  mutate(filter_var = case_when(
    (AP_math_enroll == 1 & AP_math_enroll_11 == 0 & AP_math_enroll_12 == 0 ) ~ 1,
    TRUE ~ 0))

paste("Total number of students taking AP math only in one of the first two years of high school:", sum(dat_all$filter_var), ". They were removed from the sample of interest.")

# remove these students from the sample
dat_all <- dat_all %>%
  filter(filter_var == 0) %>%
  select(-filter_var)

# adjust selected variables
dat_all <- dat_all %>%
  mutate(ID_school = as.factor(ID_school))
summary(dat_all$HS_att_09)
summary(dat_all$HS_susp_09)

# create dummies for attendance and suspensions
dat_all <- dat_all %>%
  mutate(
    HS_att_09 = case_when(
      HS_att_09 < 0.95 ~ 1,
      TRUE ~ 0),
    HS_att_10 = case_when(
      HS_att_10 < 0.95 ~ 1,
      TRUE ~ 0),
    HS_att_11 = case_when(
      HS_att_11 < 0.95 ~ 1,
      TRUE ~ 0),
    HS_susp_09 = case_when(
      HS_susp_09 > 0 ~ 1,
      TRUE ~ 0),
    HS_susp_10 = case_when(
      HS_susp_10 > 0 ~ 1,
      TRUE ~ 0))
freq_table(dat_all$HS_att_09)
freq_table(dat_all$HS_susp_10)

# dummy for cohort
dat_all <- dat_all %>%
  mutate(
    cohort_2012 = case_when(
      cohort == "2012" ~ 1,
      TRUE ~ 0))
#==============================================================
# Save
#==============================================================
save(dat_all, file = paste0(DOE_wd,"/dataset_final.Rdata"))


