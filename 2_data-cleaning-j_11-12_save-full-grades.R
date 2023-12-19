#==============================================================
# File description
#==============================================================
# contents: 
#  select students by grade
#  get their AP records
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023 

#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

# ---> year-based subscripts for variables created in this file (e.g., "_13") are copied from the 2012 cohort. 
#      for the correct years, subtract 1.

#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#=============================================================
# All Seniors year (2015)
#=============================================================
dat_g12 <- read_sas(DOE_dataset_enr_15, 
                    cols_only = c("RANYCSID", 
                                  "DOEGLVOCT", # student grade oct. e.g: "12"
                                  "DBNOCT", # school ID number
                                  "AGDDCATOCT", #Active/graduated/dropped out/discharged category
                                  "AGDDCATJUN"))

# filter by grade
dat_g12 <- dat_g12 %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "12",
         AGDDCATOCT == 1)

#==============================================================
# 2016 AP courses
#==============================================================

dat_ap <- read_sas(DOE_dataset_courses_15,
                   cols_only = c("COURSETITLE",
                                 "TermCD",
                                 "CourseCD",
                                 "Mark",
                                 "NumericEquivalent",
                                 "PassFailEquivalent",
                                 "RANYCSID"))
# Check course codes
class(dat_ap$CourseCD)
max(nchar(dat_ap$CourseCD))

#============ Select courses of interest

# Spit course code variable
x <- strsplit(dat_ap$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_ap <- as_tibble(cbind(dat_ap, y))

# Filter by AP courses with a math subject
dat_ap <- dat_ap %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_ap$X2)

# Filter out course which are not correctly coded (n = 1)
dat_ap <- dat_ap %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#============ Rename and recode variables
dat_ap <- dat_ap %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#============ Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_ap_c <- dat_ap %>%
  filter(X2 == "C",
         X5 == 1)

dat_ap_s <- dat_ap %>%
  filter(X2 == "S",
         X5 == 1)

dat_ap_t <- dat_ap %>%
  filter(X2 == "K",
         X5 == 1)

#============ Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# Calculus
dat_ap_c <- agg(dat_ap_c, dat_ap_c$enrollment, "AP_n_courses_calc", dat_ap_c$RANYCSID, "RANYCSID")
dat_ap_c <- agg(dat_ap_c, dat_ap_c$course_grade_pf, "AP_n_pass_calc", dat_ap_c$RANYCSID, "RANYCSID")
dat_ap_c <- dat_ap_c %>%
  select(RANYCSID,
         AP_n_courses_calc,
         AP_n_pass_calc) %>%
  distinct(RANYCSID, .keep_all = T)

# Stats
dat_ap_s <- agg(dat_ap_s, dat_ap_s$enrollment, "AP_n_courses_stats", dat_ap_s$RANYCSID, "RANYCSID")
dat_ap_s <- agg(dat_ap_s, dat_ap_s$course_grade_pf, "AP_n_pass_stats", dat_ap_s$RANYCSID, "RANYCSID")
dat_ap_s <- dat_ap_s %>%
  select(RANYCSID,
         AP_n_courses_stats,
         AP_n_pass_stats) %>%
  distinct(RANYCSID, .keep_all = T)

# Technology
dat_ap_t <- agg(dat_ap_t, dat_ap_t$enrollment, "AP_n_courses_tech", dat_ap_t$RANYCSID, "RANYCSID")
dat_ap_t <- agg(dat_ap_t, dat_ap_t$course_grade_pf, "AP_n_pass_tech", dat_ap_t$RANYCSID, "RANYCSID")
dat_ap_t <- dat_ap_t %>%
  select(RANYCSID,
         AP_n_courses_tech,
         AP_n_pass_tech) %>%
  distinct(RANYCSID, .keep_all = T)

#============ Merge course data with original data
# Var names
colnames(dat_ap_c) <- paste(colnames(dat_ap_c), "16", sep = "_")
colnames(dat_ap_s) <- paste(colnames(dat_ap_s), "16", sep = "_")
colnames(dat_ap_t) <- paste(colnames(dat_ap_t), "16", sep = "_")

# check intersection
x <- intersect(dat_g12$RANYCSID, dat_ap_c$RANYCSID_16) 
length(x)
x <- intersect(dat_g12$RANYCSID, dat_ap_s$RANYCSID_16) 
length(x)
x <- intersect(dat_g12$RANYCSID, dat_ap_t$RANYCSID_16) 
length(x)

# merge data
dat_g12 <- left_join(dat_g12, dat_ap_c, by = c("RANYCSID" = "RANYCSID_16"))
dat_g12 <- left_join(dat_g12, dat_ap_s, by = c("RANYCSID" = "RANYCSID_16"))
dat_g12 <- left_join(dat_g12, dat_ap_t, by = c("RANYCSID" = "RANYCSID_16"))

#============ Check and adjust aggregated variables
# Check
length(unique(dat_ap_c$RANYCSID_16))
length(unique(dat_ap_s$RANYCSID_16))
length(unique(dat_ap_t$RANYCSID_16))
length(unique(dat_g12$RANYCSID))
nrow(dat_g12)
ncol(dat_g12)

# Transform course values with NAs into 0s
vars <- dat_g12 %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat_g12 <- dat_g12 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

dat_g12 <- dat_g12 %>%
  mutate(AP_n_courses_12 = AP_n_courses_calc_16 + AP_n_courses_stats_16,
         AP_math_enroll_12 = case_when(AP_n_courses_12 >= 1 ~ 1, 
                                       TRUE ~ 0))
#==============================================================
# 2015 AP exams
#==============================================================

dat_exams <- read_sas(DOE_dataset_exams_15)

# select variables of interest
dat_exams <- dat_exams %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

vars <- dat_exams %>%
  select(-RANYCSID) %>%
  names()

dat_g12 <- left_join(dat_g12, dat_exams, by = c("RANYCSID" = "RANYCSID"))

dat_g12 <- dat_g12 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0))) %>%
  mutate(AP_math_exam_12 = case_when(APMTHATM >= 1 ~ 1,
                                     TRUE ~ 0))

save(dat_g12, file = paste0(DOE_server_wd_data,"/dataset_check_all-g12-2014-15.Rdata"))

#=============================================================
# All juniors year (2014)
#=============================================================
dat_g11 <- read_sas(DOE_dataset_enr_14, 
                    cols_only = c("RANYCSID", 
                                  "DOEGLVOCT", # student grade oct. e.g: "11"
                                  "DBNOCT", # school ID number
                                  "AGDDCATOCT", #Active/graduated/dropped out/discharged category
                                  "AGDDCATJUN"))

# filter by grade
dat_g11 <- dat_g11 %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "11",
         AGDDCATOCT == 1)

#==============================================================
# 2014 AP courses
#==============================================================

dat_ap <- read_sas(DOE_dataset_courses_14,
                   cols_only = c("COURSETITLE",
                                 "TermCD",
                                 "CourseCD",
                                 "Mark",
                                 "NumericEquivalent",
                                 "PassFailEquivalent",
                                 "RANYCSID"))
# Check course codes
class(dat_ap$CourseCD)
max(nchar(dat_ap$CourseCD))

#============ Select courses of interest

# Spit course code variable
x <- strsplit(dat_ap$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_ap <- as_tibble(cbind(dat_ap, y))

# Filter by AP courses with a math subject
dat_ap <- dat_ap %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_ap$X2)

# Filter out course which are not correctly coded (n = 1)
dat_ap <- dat_ap %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#============ Rename and recode variables
dat_ap <- dat_ap %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#============ Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_ap_c <- dat_ap %>%
  filter(X2 == "C",
         X5 == 1)

dat_ap_s <- dat_ap %>%
  filter(X2 == "S",
         X5 == 1)

dat_ap_t <- dat_ap %>%
  filter(X2 == "K",
         X5 == 1)

#============ Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# Calculus
dat_ap_c <- agg(dat_ap_c, dat_ap_c$enrollment, "AP_n_courses_calc", dat_ap_c$RANYCSID, "RANYCSID")
dat_ap_c <- agg(dat_ap_c, dat_ap_c$course_grade_pf, "AP_n_pass_calc", dat_ap_c$RANYCSID, "RANYCSID")
dat_ap_c <- dat_ap_c %>%
  select(RANYCSID,
         AP_n_courses_calc,
         AP_n_pass_calc) %>%
  distinct(RANYCSID, .keep_all = T)

# Stats
dat_ap_s <- agg(dat_ap_s, dat_ap_s$enrollment, "AP_n_courses_stats", dat_ap_s$RANYCSID, "RANYCSID")
dat_ap_s <- agg(dat_ap_s, dat_ap_s$course_grade_pf, "AP_n_pass_stats", dat_ap_s$RANYCSID, "RANYCSID")
dat_ap_s <- dat_ap_s %>%
  select(RANYCSID,
         AP_n_courses_stats,
         AP_n_pass_stats) %>%
  distinct(RANYCSID, .keep_all = T)

# Technology
dat_ap_t <- agg(dat_ap_t, dat_ap_t$enrollment, "AP_n_courses_tech", dat_ap_t$RANYCSID, "RANYCSID")
dat_ap_t <- agg(dat_ap_t, dat_ap_t$course_grade_pf, "AP_n_pass_tech", dat_ap_t$RANYCSID, "RANYCSID")
dat_ap_t <- dat_ap_t %>%
  select(RANYCSID,
         AP_n_courses_tech,
         AP_n_pass_tech) %>%
  distinct(RANYCSID, .keep_all = T)

#============ Merge course data with original data
# Var names
colnames(dat_ap_c) <- paste(colnames(dat_ap_c), "15", sep = "_")
colnames(dat_ap_s) <- paste(colnames(dat_ap_s), "15", sep = "_")
colnames(dat_ap_t) <- paste(colnames(dat_ap_t), "15", sep = "_")

# check intersection
x <- intersect(dat_g11$RANYCSID, dat_ap_c$RANYCSID_15) 
length(x)
x <- intersect(dat_g11$RANYCSID, dat_ap_s$RANYCSID_15) 
length(x)
x <- intersect(dat_g11$RANYCSID, dat_ap_t$RANYCSID_15) 
length(x)

# merge data
dat_g11 <- left_join(dat_g11, dat_ap_c, by = c("RANYCSID" = "RANYCSID_15"))
dat_g11 <- left_join(dat_g11, dat_ap_s, by = c("RANYCSID" = "RANYCSID_15"))
dat_g11 <- left_join(dat_g11, dat_ap_t, by = c("RANYCSID" = "RANYCSID_15"))

#============ Check and adjust aggregated variables
# Check
length(unique(dat_ap_c$RANYCSID_15))
length(unique(dat_ap_s$RANYCSID_15))
length(unique(dat_ap_t$RANYCSID_15))
length(unique(dat_g11$RANYCSID))
nrow(dat_g11)
ncol(dat_g11)

# Transform course values with NAs into 0s
vars <- dat_g11 %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat_g11 <- dat_g11 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

dat_g11 <- dat_g11 %>%
  mutate(AP_n_courses_11 = AP_n_courses_calc_15 + AP_n_courses_stats_15,
         AP_math_enroll_11 = case_when(AP_n_courses_11 >= 1 ~ 1, 
                                       TRUE ~ 0))
#==============================================================
# 2014 AP exams
#==============================================================

dat_exams <- read_sas(DOE_dataset_exams_14)

# select variables of interest
dat_exams <- dat_exams %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

vars <- dat_exams %>%
  select(-RANYCSID) %>%
  names()

dat_g11 <- left_join(dat_g11, dat_exams, by = c("RANYCSID" = "RANYCSID"))

dat_g11 <- dat_g11 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0))) %>%
  mutate(AP_math_exam_11 = case_when(APMTHATM >= 1 ~ 1,
                                     TRUE ~ 0))

save(dat_g11, file = paste0(DOE_server_wd_data,"/dataset_check_all-g11-2013-14.Rdata"))

#=============================================================
# All sophomores year (2013)
#=============================================================
dat_g10 <- read_sas(DOE_dataset_enr_13, 
                    cols_only = c("RANYCSID", 
                                  "DOEGLVOCT", # student grade oct. e.g: "10"
                                  "DBNOCT", # school ID number
                                  "AGDDCATOCT", #Active/graduated/dropped out/discharged category
                                  "AGDDCATJUN"))

# filter by grade
dat_g10 <- dat_g10 %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "10",
         AGDDCATOCT == 1)

#==============================================================
# 2013 AP courses
#==============================================================

dat_ap <- read_sas(DOE_dataset_courses_13,
                   cols_only = c("COURSETITLE",
                                 "TermCD",
                                 "CourseCD",
                                 "Mark",
                                 "NumericEquivalent",
                                 "PassFailEquivalent",
                                 "RANYCSID"))
# Check course codes
class(dat_ap$CourseCD)
max(nchar(dat_ap$CourseCD))

#============ Select courses of interest

# Spit course code variable
x <- strsplit(dat_ap$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_ap <- as_tibble(cbind(dat_ap, y))

# Filter by AP courses with a math subject
dat_ap <- dat_ap %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_ap$X2)

# Filter out course which are not correctly coded (n = 1)
dat_ap <- dat_ap %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#============ Rename and recode variables
dat_ap <- dat_ap %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#============ Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_ap_c <- dat_ap %>%
  filter(X2 == "C",
         X5 == 1)

dat_ap_s <- dat_ap %>%
  filter(X2 == "S",
         X5 == 1)

dat_ap_t <- dat_ap %>%
  filter(X2 == "K",
         X5 == 1)

#============ Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# Calculus
dat_ap_c <- agg(dat_ap_c, dat_ap_c$enrollment, "AP_n_courses_calc", dat_ap_c$RANYCSID, "RANYCSID")
dat_ap_c <- agg(dat_ap_c, dat_ap_c$course_grade_pf, "AP_n_pass_calc", dat_ap_c$RANYCSID, "RANYCSID")
dat_ap_c <- dat_ap_c %>%
  select(RANYCSID,
         AP_n_courses_calc,
         AP_n_pass_calc) %>%
  distinct(RANYCSID, .keep_all = T)

# Stats
dat_ap_s <- agg(dat_ap_s, dat_ap_s$enrollment, "AP_n_courses_stats", dat_ap_s$RANYCSID, "RANYCSID")
dat_ap_s <- agg(dat_ap_s, dat_ap_s$course_grade_pf, "AP_n_pass_stats", dat_ap_s$RANYCSID, "RANYCSID")
dat_ap_s <- dat_ap_s %>%
  select(RANYCSID,
         AP_n_courses_stats,
         AP_n_pass_stats) %>%
  distinct(RANYCSID, .keep_all = T)

# Technology
dat_ap_t <- agg(dat_ap_t, dat_ap_t$enrollment, "AP_n_courses_tech", dat_ap_t$RANYCSID, "RANYCSID")
dat_ap_t <- agg(dat_ap_t, dat_ap_t$course_grade_pf, "AP_n_pass_tech", dat_ap_t$RANYCSID, "RANYCSID")
dat_ap_t <- dat_ap_t %>%
  select(RANYCSID,
         AP_n_courses_tech,
         AP_n_pass_tech) %>%
  distinct(RANYCSID, .keep_all = T)

#============ Merge course data with original data
# Var names
colnames(dat_ap_c) <- paste(colnames(dat_ap_c), "14", sep = "_")
colnames(dat_ap_s) <- paste(colnames(dat_ap_s), "14", sep = "_")
colnames(dat_ap_t) <- paste(colnames(dat_ap_t), "14", sep = "_")

# check intersection
x <- intersect(dat_g10$RANYCSID, dat_ap_c$RANYCSID_14) 
length(x)
x <- intersect(dat_g10$RANYCSID, dat_ap_s$RANYCSID_14) 
length(x)
x <- intersect(dat_g10$RANYCSID, dat_ap_t$RANYCSID_14) 
length(x)

# merge data
dat_g10 <- left_join(dat_g10, dat_ap_c, by = c("RANYCSID" = "RANYCSID_14"))
dat_g10 <- left_join(dat_g10, dat_ap_s, by = c("RANYCSID" = "RANYCSID_14"))
dat_g10 <- left_join(dat_g10, dat_ap_t, by = c("RANYCSID" = "RANYCSID_14"))

#============ Check and adjust aggregated variables
# Check
length(unique(dat_ap_c$RANYCSID_14))
length(unique(dat_ap_s$RANYCSID_14))
length(unique(dat_ap_t$RANYCSID_14))
length(unique(dat_g10$RANYCSID))
nrow(dat_g10)
ncol(dat_g10)

# Transform course values with NAs into 0s
vars <- dat_g10 %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat_g10 <- dat_g10 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

dat_g10 <- dat_g10 %>%
  mutate(AP_n_courses_10 = AP_n_courses_calc_14 + AP_n_courses_stats_14,
         AP_math_enroll_10 = case_when(AP_n_courses_10 >= 1 ~ 1, 
                                       TRUE ~ 0))
#==============================================================
# 2013 AP exams
#==============================================================

dat_exams <- read_sas(DOE_dataset_exams_13)

# select variables of interest
dat_exams <- dat_exams %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

vars <- dat_exams %>%
  select(-RANYCSID) %>%
  names()

dat_g10 <- left_join(dat_g10, dat_exams, by = c("RANYCSID" = "RANYCSID"))

dat_g10 <- dat_g10 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0))) %>%
  mutate(AP_math_exam_10 = case_when(APMTHATM >= 1 ~ 1,
                                     TRUE ~ 0))

save(dat_g10, file = paste0(DOE_server_wd_data,"/dataset_check_all-g10-2012-13.Rdata"))

#=============================================================
# All freshman year (2012)
#=============================================================
dat_g09 <- read_sas(DOE_dataset_enr_12, 
                    cols_only = c("RANYCSID", 
                                  "DOEGLVOCT", # student grade oct. e.g: "09"
                                  "DBNOCT", # school ID number
                                  "AGDDCATOCT", #Active/graduated/dropped out/discharged category
                                  "AGDDCATJUN"))

# filter by grade
dat_g09 <- dat_g09 %>%
  rename(ID_school = DBNOCT) %>%
  filter(DOEGLVOCT == "09",
         AGDDCATOCT == 1)

#==============================================================
# 2013 AP courses
#==============================================================

dat_ap <- read_sas(DOE_dataset_courses_12,
                   cols_only = c("COURSETITLE",
                                 "TermCD",
                                 "CourseCD",
                                 "Mark",
                                 "NumericEquivalent",
                                 "PassFailEquivalent",
                                 "RANYCSID"))
# Check course codes
class(dat_ap$CourseCD)
max(nchar(dat_ap$CourseCD))

#============ Select courses of interest

# Spit course code variable
x <- strsplit(dat_ap$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_ap <- as_tibble(cbind(dat_ap, y))

# Filter by AP courses with a math subject
dat_ap <- dat_ap %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_ap$X2)

# Filter out course which are not correctly coded (n = 1)
dat_ap <- dat_ap %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#============ Rename and recode variables
dat_ap <- dat_ap %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#============ Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_ap_c <- dat_ap %>%
  filter(X2 == "C",
         X5 == 1)

dat_ap_s <- dat_ap %>%
  filter(X2 == "S",
         X5 == 1)

dat_ap_t <- dat_ap %>%
  filter(X2 == "K",
         X5 == 1)

#=============== comment lines start here =========================
# ---> deleting this code because of no rows to aggregate
#=============== Aggregate variables at the student level
# # the code here transforms the dataset from a course ID format to a student ID format

# # Calculus
# dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$enrollment, "AP_n_courses_calc", dat_nyc_c$RANYCSID, "RANYCSID")
# dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$course_grade_pf, "AP_n_pass_calc", dat_nyc_c$RANYCSID, "RANYCSID")
# dat_nyc_c <- dat_nyc_c %>%
#   select(RANYCSID,
#          AP_n_courses_calc,
#          AP_n_pass_calc) %>%
#   distinct(RANYCSID, .keep_all = T)
# 
# # The following have no rows to aggregate
# # Stats
# dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$enrollment, "AP_n_courses_stats", dat_nyc_s$RANYCSID, "RANYCSID")
# dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$course_grade_pf, "AP_n_pass_stats", dat_nyc_s$RANYCSID, "RANYCSID")
# dat_nyc_s <- dat_nyc_s %>%
#   select(RANYCSID,
#          AP_n_courses_stats,
#          AP_n_pass_stats) %>%
#   distinct(RANYCSID, .keep_all = T)
# 
# # Technology
# dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$enrollment, "AP_n_courses_tech", dat_nyc_t$RANYCSID, "RANYCSID")
# dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$course_grade_pf, "AP_n_pass_tech", dat_nyc_t$RANYCSID, "RANYCSID")
# dat_nyc_t <- dat_nyc_t %>%
#   select(RANYCSID,
#          AP_n_courses_tech,
#          AP_n_pass_tech) %>%
#   distinct(RANYCSID, .keep_all = T)
# 
# #=============== Merge course data with original data
# # Var names
# colnames(dat_nyc_c) <- paste(colnames(dat_nyc_c), "13", sep = "_")
# colnames(dat_nyc_s) <- paste(colnames(dat_nyc_s), "13", sep = "_")
# colnames(dat_nyc_t) <- paste(colnames(dat_nyc_t), "13", sep = "_")
# 
# # check intersection
# x <- intersect(dat$RANYCSID_13, dat_nyc_c$RANYCSID_13)
# length(x)
# x <- intersect(dat$RANYCSID_13, dat_nyc_s$RANYCSID_13)
# length(x)
# x <- intersect(dat$RANYCSID_13, dat_nyc_t$RANYCSID_13)
# length(x)
# 
# # merge data
# dat <- left_join(dat, dat_nyc_c, by = c("RANYCSID_13" = "RANYCSID_13"))
# dat <- left_join(dat, dat_nyc_s, by = c("RANYCSID_13" = "RANYCSID_13"))
# dat <- left_join(dat, dat_nyc_t, by = c("RANYCSID_13" = "RANYCSID_13"))
#=================== comment lines end here =======================

dat_g09 <- dat_g09 %>%
  mutate(AP_n_courses_tech_13 = 0,
         AP_n_pass_tech_13 = 0,
         AP_n_courses_stats_13 = 0,
         AP_n_pass_stats_13 = 0,
         AP_n_courses_calc_13 = 0,
         AP_n_pass_calc_13 = 0)

#============ Check and adjust aggregated variables

# Transform course values with NAs into 0s
vars <- dat_g09 %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat_g09 <- dat_g09 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

dat_g09 <- dat_g09 %>%
  mutate(AP_n_courses_09 = AP_n_courses_calc_13 + AP_n_courses_stats_13,
         AP_math_enroll_09 = case_when(AP_n_courses_09 >= 1 ~ 1, 
                                       TRUE ~ 0))
#==============================================================
# 2012 AP exams
#==============================================================

dat_exams <- read_sas(DOE_dataset_exams_12)

# select variables of interest
dat_exams <- dat_exams %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

vars <- dat_exams %>%
  select(-RANYCSID) %>%
  names()

dat_g09 <- left_join(dat_g09, dat_exams, by = c("RANYCSID" = "RANYCSID"))

dat_g09 <- dat_g09 %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0))) %>%
  mutate(AP_math_exam_09 = case_when(APMTHATM >= 1 ~ 1,
                                     TRUE ~ 0))

save(dat_g09, file = paste0(DOE_server_wd_data,"/dataset_check_all-g09-2011-12.Rdata"))