#==============================================================
# File description
#==============================================================
# contents: 
#  filter data at the student level
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

# ---> Here, given the sample of high schools selected,
#      I filter the data for students who follow a regular
#      high school trajectory up untill their senior year

# ---> Note that to use the language of the paper,
#      whenever I refer to the "sample", I mean to refer to the "cohort"  

#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

load(paste0(DOE_server_wd_data,"/dataset_6_all_schools.Rdata"))

dat <- df %>%
  filter(cohort == 2011)
dat_all <- dat
s = nrow(dat_all)

school_IDs <- dat %>%
  select(ID_school) %>%
  distinct(ID_school)

school_vector <- school_IDs$ID_school

# Saving output for quicker access
sink("output/student-level-filter_cohort-11-12.txt")
#==============================================================
# 7th grade
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_10, 
                    cols_only = c("RANYCSID", 
                                   "DBNOCT",
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "AGDDCATOCT"))

# filter
dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "07",
         AGDDCATOCT == 1)

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "07", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(ID_student_07 = ENR_RANYCSID_07)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_07)
dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_07"))

a = nrow(dat)
paste("Total number of students enrolled in 7th grade in the Fall 2010 after merge", nrow(dat)) 
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# 8th grade
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_11, 
                    cols_only = c("RANYCSID", 
                                   "DBNOCT",
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "AGDDCATOCT"))

dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "08",
         AGDDCATOCT == 1)

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "08", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(ID_student_08 = ENR_RANYCSID_08)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_08)

dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_08"))

a = nrow(dat)
paste("Total number of students enrolled in 8th grade in the Fall 2011 after merge", nrow(dat)) 
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# Freshman year
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_12, 
                    cols_only = c("RANYCSID", 
                                   "DBNOCT", # school ID number
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "AGDDCATOCT"))
# filter
dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "09",
         AGDDCATOCT == 1)

dat_check <- left_join(school_IDs, dat_nyc, by = "ID_school")
g = nrow(dat_check)

paste("Total number of students enrolled in 9th grade in the Fall 2013 before filter", nrow(dat_check)) 
paste("Total number of students enrolled in 9th grade in the Fall 2013 after merge", nrow(dat)) 

a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# Sophomore year
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_13, 
                    cols_only = c("RANYCSID", 
                                   "DBNOCT", # school ID number
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "AGDDCATOCT"))
# filter
dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "10",
         AGDDCATOCT == 1)

dat_check <- left_join(school_IDs, dat_nyc, by = "ID_school")
g = nrow(dat_check)
paste("Total number of students enrolled in 10th grade in the Fall 2013 before filter", nrow(dat_check)) 

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "10", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(ID_student_10 = ENR_RANYCSID_10)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_10)
dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_10"))

paste("Total number of students enrolled in 10th grade in the Fall 2013 after merge", nrow(dat)) 

a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# Junior year
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_14, 
                    cols_only = c("RANYCSID",
                                   "DBNOCT", # school ID number
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "AGDDCATOCT"))

# filter
dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "11",
         AGDDCATOCT == 1)

dat_check <- left_join(school_IDs, dat_nyc, by = "ID_school")
g = nrow(dat_check)
paste("Total number of students enrolled in 11th grade in the Fall 2014 before filter", nrow(dat_check)) 

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "11", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(ID_student_11 = ENR_RANYCSID_11)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_11)
dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_11"))

paste("Total number of students enrolled in 11th grade in the Fall 2014 after merge", nrow(dat)) 

a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")

paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# Senior year 1
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_15, 
                    cols_only = c("RANYCSID", 
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "DBNOCT", # school ID number
                                   "AGDDCATOCT", #Active/graduated/dropped out/discharged category
                                   "AGDDCATJUN"))

# filter
dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "12",
         AGDDCATOCT == 1)

dat_check <- left_join(school_IDs, dat_nyc, by = "ID_school")
g = nrow(dat_check)
paste("Total number of students enrolled in 12th grade in the Fall 2015 before filter", nrow(dat_check)) 

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "12", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(ID_student_12 = ENR_RANYCSID_12)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_12)
dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_12"))

paste("Total number of students enrolled in 12th grade in the Fall 2015 after merge", nrow(dat)) 

a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# If transferred, transferred within select sample of schools
#==============================================================
dat_nyc <- dat_nyc %>%
  filter(ENR_ID_school_12 %in% school_vector)

dat_nyc <- dat_nyc %>%
  select(-ENR_ID_school_12)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_12)
dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_12"))

paste("Total number of students after transfer check", nrow(dat)) 
paste("Total number of schools after transfer check", length(unique(dat$ID_school)))

a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# Senior year 2
#==============================================================
dat_nyc <- read_sas(DOE_dataset_enr_16, 
                    cols_only = c("RANYCSID", 
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "AGDDCATOCT",
                                   "DBNOCT"))

# Check total number of students before filter
dat_nyc <- dat_nyc %>%
  rename(ID_school = DBNOCT) 

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "13", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(ID_student_13 = ENR_RANYCSID_13)

# Merge files
x <- intersect(dat$ID_student, dat_nyc$ID_student_13)
dat <- inner_join(dat, dat_nyc, by = c("ID_student" = "ID_student_13"))

paste("Total number of students in the Fall 2016 after merge", nrow(dat)) 
paste("Total number of schools after merge", length(unique(dat$ID_school)))

#==============================================================
# Final file
#==============================================================

# Variable establishing if the student graduated by Oct 2017
dat <- dat %>%
  mutate(HS_4year_grad = case_when(
    ENR_AGDDCATOCT_13 == 2 ~ 1,
    TRUE ~ 0))

paste("High school graduates")
freq_table(dat$HS_4year_grad)

dat <- agg_mean(dat, dat$HS_4year_grad, "SCH_pct_cohort_grad_4years", dat$ID_school, "ID_school")
dat <- agg(dat, dat$HS_4year_grad, "SCH_n_cohort_grad_4years", dat$ID_school, "ID_school")

# Check students who start and end HS in the same school
all(dat$ID_school == dat$ENR_ID_school_12)

dat <- dat %>%
  mutate(HS_transfer = case_when(
    ID_school == ENR_ID_school_12 ~ 1,
    TRUE ~ 0))
summary(dat$HS_transfer)

dat_ret <- anti_join(dat_all, dat, by = c("ID_student" = "ID_student"))
sink()

#==============================================================
# Save NYC file 
#============================================================== 
save(dat, file = paste0(DOE_server_wd_data,"/dataset_7_cohort-11-12.Rdata"))
save(dat_ret, file = paste0(DOE_server_wd_data,"/dataset_7_cohort-11-12_anti.Rdata"))
