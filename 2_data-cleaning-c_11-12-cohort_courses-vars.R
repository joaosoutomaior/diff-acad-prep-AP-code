#==============================================================
# File description
#==============================================================
# contents: 
#  cleaning of AP variables
#  cleaning of other coursework variables (HS and MS)
#  cleaning of behavior-related variables
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
# Previous version of the school data
#==============================================================

load(paste0(DOE_server_wd_data,"/dataset_2_cohort-11-12.Rdata"))

#==============================================================
# Dictionary of course codes
#==============================================================
# X1: department
# X2: area within the department (eg. geometry, calculus)
# X3: course duration
# X4: number of many courses in the sequence
# X5: position of the course in the sequence
# X6: curriculum and rigor of the class (eg. "X" = AP course)
# X7: delivery mechanism of the course
# X8: for school use

#==============================================================
# 2015 AP courses
#==============================================================

dat_nyc <- read_sas(DOE_dataset_courses_15,
                    cols_only = c("COURSETITLE",
                                   "TermCD",
                                   "CourseCD",
                                   "Mark",
                                   "NumericEquivalent",
                                   "PassFailEquivalent",
                                   "RANYCSID"))
# Check course codes
class(dat_nyc$CourseCD)
max(nchar(dat_nyc$CourseCD))

#=============== Select courses of interest

# Spit course code variable
x <- strsplit(dat_nyc$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_nyc <- as_tibble(cbind(dat_nyc, y))


# Filter by AP courses with a math subject
dat_nyc <- dat_nyc %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_nyc$X2)

# Filter out course which are not correctly coded (n = 1)
dat_nyc <- dat_nyc %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#=============== Rename and recode variables
dat_nyc <- dat_nyc %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#=============== Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_nyc_c <- dat_nyc %>%
  filter(X2 == "C",
         X5 == 1)

dat_nyc_s <- dat_nyc %>%
  filter(X2 == "S",
         X5 == 1)

dat_nyc_t <- dat_nyc %>%
  filter(X2 == "K",
         X5 == 1)

#=============== Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# Calculus
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$enrollment, "AP_n_courses_calc", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$course_grade_pf, "AP_n_pass_calc", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- dat_nyc_c %>%
  select(RANYCSID,
         AP_n_courses_calc,
         AP_n_pass_calc) %>%
  distinct(RANYCSID, .keep_all = T)

# Stats
dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$enrollment, "AP_n_courses_stats", dat_nyc_s$RANYCSID, "RANYCSID")
dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$course_grade_pf, "AP_n_pass_stats", dat_nyc_s$RANYCSID, "RANYCSID")
dat_nyc_s <- dat_nyc_s %>%
  select(RANYCSID,
         AP_n_courses_stats,
         AP_n_pass_stats) %>%
  distinct(RANYCSID, .keep_all = T)

# Technology
dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$enrollment, "AP_n_courses_tech", dat_nyc_t$RANYCSID, "RANYCSID")
dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$course_grade_pf, "AP_n_pass_tech", dat_nyc_t$RANYCSID, "RANYCSID")
dat_nyc_t <- dat_nyc_t %>%
  select(RANYCSID,
         AP_n_courses_tech,
         AP_n_pass_tech) %>%
  distinct(RANYCSID, .keep_all = T)

#=============== Merge course data with original data
# Var names
colnames(dat_nyc_c) <- paste(colnames(dat_nyc_c), "16", sep = "_")
colnames(dat_nyc_s) <- paste(colnames(dat_nyc_s), "16", sep = "_")
colnames(dat_nyc_t) <- paste(colnames(dat_nyc_t), "16", sep = "_")

# check intersection
x <- intersect(dat$RANYCSID_13, dat_nyc_c$RANYCSID_16) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_s$RANYCSID_16) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_t$RANYCSID_16) 
length(x)

# merge data
dat <- left_join(dat, dat_nyc_c, by = c("RANYCSID_13" = "RANYCSID_16"))
dat <- left_join(dat, dat_nyc_s, by = c("RANYCSID_13" = "RANYCSID_16"))
dat <- left_join(dat, dat_nyc_t, by = c("RANYCSID_13" = "RANYCSID_16"))

#=============== Check and adjust aggregated variables
# Check
length(unique(dat_nyc_c$RANYCSID_16))
length(unique(dat_nyc_s$RANYCSID_16))
length(unique(dat_nyc_t$RANYCSID_16))
length(unique(dat$RANYCSID_13))
nrow(dat)
ncol(dat)

# Transform course values with NAs into 0s
vars <- dat %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#==============================================================
# 2014 AP courses
#==============================================================

dat_nyc <- read_sas(DOE_dataset_courses_14,
                    cols_only = c("COURSETITLE",
                                   "TermCD",
                                   "CourseCD",
                                   "Mark",
                                   "NumericEquivalent",
                                   "PassFailEquivalent",
                                   "RANYCSID"))

# Check course codes
class(dat_nyc$CourseCD)
max(nchar(dat_nyc$CourseCD))

#=============== Select courses of interest

# Spit course code variable
x <- strsplit(dat_nyc$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_nyc <- as_tibble(cbind(dat_nyc, y))

# Filter by AP courses with a math subject
dat_nyc <- dat_nyc %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_nyc$X2)

# Filter out course which are not correctly coded (n = 1)
dat_nyc <- dat_nyc %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#=============== Rename and recode variables
dat_nyc <- dat_nyc %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#=============== Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_nyc_c <- dat_nyc %>%
  filter(X2 == "C",
         X5 == 1)

dat_nyc_s <- dat_nyc %>%
  filter(X2 == "S",
         X5 == 1)

dat_nyc_t <- dat_nyc %>%
  filter(X2 == "K",
         X5 == 1)

#=============== Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# Calculus
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$enrollment, "AP_n_courses_calc", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$course_grade_pf, "AP_n_pass_calc", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- dat_nyc_c %>%
  select(RANYCSID,
         AP_n_courses_calc,
         AP_n_pass_calc) %>%
  distinct(RANYCSID, .keep_all = T)

# Stats
dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$enrollment, "AP_n_courses_stats", dat_nyc_s$RANYCSID, "RANYCSID")
dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$course_grade_pf, "AP_n_pass_stats", dat_nyc_s$RANYCSID, "RANYCSID")
dat_nyc_s <- dat_nyc_s %>%
  select(RANYCSID,
         AP_n_courses_stats,
         AP_n_pass_stats) %>%
  distinct(RANYCSID, .keep_all = T)

# Technology
dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$enrollment, "AP_n_courses_tech", dat_nyc_t$RANYCSID, "RANYCSID")
dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$course_grade_pf, "AP_n_pass_tech", dat_nyc_t$RANYCSID, "RANYCSID")
dat_nyc_t <- dat_nyc_t %>%
  select(RANYCSID,
         AP_n_courses_tech,
         AP_n_pass_tech) %>%
  distinct(RANYCSID, .keep_all = T)

#=============== Merge course data with original data
# Var names
colnames(dat_nyc_c) <- paste(colnames(dat_nyc_c), "15", sep = "_")
colnames(dat_nyc_s) <- paste(colnames(dat_nyc_s), "15", sep = "_")
colnames(dat_nyc_t) <- paste(colnames(dat_nyc_t), "15", sep = "_")

# check intersection
x <- intersect(dat$RANYCSID_13, dat_nyc_c$RANYCSID_15) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_s$RANYCSID_15) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_t$RANYCSID_15) 
length(x)

# merge data
dat <- left_join(dat, dat_nyc_c, by = c("RANYCSID_13" = "RANYCSID_15"))
dat <- left_join(dat, dat_nyc_s, by = c("RANYCSID_13" = "RANYCSID_15"))
dat <- left_join(dat, dat_nyc_t, by = c("RANYCSID_13" = "RANYCSID_15"))

#=============== Check and adjust aggregated variables
# Check
length(unique(dat_nyc_c$RANYCSID_15))
length(unique(dat_nyc_s$RANYCSID_15))
length(unique(dat_nyc_t$RANYCSID_15))
length(unique(dat$RANYCSID_13))
nrow(dat)
ncol(dat)

# Transform course values with NAs into 0s
vars <- dat %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#==============================================================
# 2013 AP courses
#==============================================================

dat_nyc <- read_sas(DOE_dataset_courses_13,
                    cols_only = c("COURSETITLE",
                                   "TermCD",
                                   "CourseCD",
                                   "Mark",
                                   "NumericEquivalent",
                                   "PassFailEquivalent",
                                   "RANYCSID"))
# Check course codes
class(dat_nyc$CourseCD)
max(nchar(dat_nyc$CourseCD))

#=============== Select courses of interest

# Spit course code variable
x <- strsplit(dat_nyc$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_nyc <- as_tibble(cbind(dat_nyc, y))


# Filter by AP courses with a math subject
dat_nyc <- dat_nyc %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_nyc$X2)

# Filter out course which are not correctly coded (n = 1)
dat_nyc <- dat_nyc %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#=============== Rename and recode variables
dat_nyc <- dat_nyc %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#=============== Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_nyc_c <- dat_nyc %>%
  filter(X2 == "C",
         X5 == 1)

dat_nyc_s <- dat_nyc %>%
  filter(X2 == "S",
         X5 == 1)

dat_nyc_t <- dat_nyc %>%
  filter(X2 == "K",
         X5 == 1)

#=============== Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# Calculus
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$enrollment, "AP_n_courses_calc", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$course_grade_pf, "AP_n_pass_calc", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- dat_nyc_c %>%
  select(RANYCSID,
         AP_n_courses_calc,
         AP_n_pass_calc) %>%
  distinct(RANYCSID, .keep_all = T)

# Stats
dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$enrollment, "AP_n_courses_stats", dat_nyc_s$RANYCSID, "RANYCSID")
dat_nyc_s <- agg(dat_nyc_s, dat_nyc_s$course_grade_pf, "AP_n_pass_stats", dat_nyc_s$RANYCSID, "RANYCSID")
dat_nyc_s <- dat_nyc_s %>%
  select(RANYCSID,
         AP_n_courses_stats,
         AP_n_pass_stats) %>%
  distinct(RANYCSID, .keep_all = T)

# Technology
dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$enrollment, "AP_n_courses_tech", dat_nyc_t$RANYCSID, "RANYCSID")
dat_nyc_t <- agg(dat_nyc_t, dat_nyc_t$course_grade_pf, "AP_n_pass_tech", dat_nyc_t$RANYCSID, "RANYCSID")
dat_nyc_t <- dat_nyc_t %>%
  select(RANYCSID,
         AP_n_courses_tech,
         AP_n_pass_tech) %>%
  distinct(RANYCSID, .keep_all = T)

#=============== Merge course data with original data
# Var names
colnames(dat_nyc_c) <- paste(colnames(dat_nyc_c), "14", sep = "_")
colnames(dat_nyc_s) <- paste(colnames(dat_nyc_s), "14", sep = "_")
colnames(dat_nyc_t) <- paste(colnames(dat_nyc_t), "14", sep = "_")

# check intersection
x <- intersect(dat$RANYCSID_13, dat_nyc_c$RANYCSID_14) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_s$RANYCSID_14) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_t$RANYCSID_14) 
length(x)

# merge data
dat <- left_join(dat, dat_nyc_c, by = c("RANYCSID_13" = "RANYCSID_14"))
dat <- left_join(dat, dat_nyc_s, by = c("RANYCSID_13" = "RANYCSID_14"))
dat <- left_join(dat, dat_nyc_t, by = c("RANYCSID_13" = "RANYCSID_14"))

#=============== Check and adjust aggregated variables
# Check
length(unique(dat_nyc_c$RANYCSID_14))
length(unique(dat_nyc_s$RANYCSID_14))
length(unique(dat_nyc_t$RANYCSID_14))
length(unique(dat$RANYCSID_13))
nrow(dat)
ncol(dat)

# Transform course values with NAs into 0s
vars <- dat %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#==============================================================
# 2012 AP courses
#==============================================================

dat_nyc <- read_sas(DOE_dataset_courses_12,
                    cols_only = c("COURSETITLE",
                                   "TermCD",
                                   "CourseCD",
                                   "Mark",
                                   "NumericEquivalent",
                                   "PassFailEquivalent",
                                   "RANYCSID"))

# Check course codes
class(dat_nyc$CourseCD)
max(nchar(dat_nyc$CourseCD))

#=============== Select courses of interest

# Spit course code variable
x <- strsplit(dat_nyc$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_nyc <- as_tibble(cbind(dat_nyc, y))


# Filter by AP courses with a math subject
dat_nyc <- dat_nyc %>%
  filter(X6 == "X",
         X1 == "M")
freq_table(dat_nyc$X2)

# Filter out course which are not correctly coded (n = 1)
dat_nyc <- dat_nyc %>%
  filter(X2 == "C" | X2 == "K" |X2 == "Q" | X2 == "S")

#=============== Rename and recode variables
dat_nyc <- dat_nyc %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#=============== Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

dat_nyc_c <- dat_nyc %>%
  filter(X2 == "C",
         X5 == 1)

dat_nyc_s <- dat_nyc %>%
  filter(X2 == "S",
         X5 == 1)

dat_nyc_t <- dat_nyc %>%
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

dat <- dat %>%
  mutate(AP_n_courses_tech_13 = 0,
         AP_n_pass_tech_13 = 0,
         AP_n_courses_stats_13 = 0,
         AP_n_pass_stats_13 = 0,
         AP_n_courses_calc_13 = 0,
         AP_n_pass_calc_13 = 0)

#=============== Check and adjust aggregated variables
# Check
length(unique(dat$RANYCSID_13))
nrow(dat)
ncol(dat)

# Transform course values with NAs into 0s
vars <- dat %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass")) %>%
  names()
dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))


#==============================================================
# 2013 Mathematics courses
#==============================================================

dat_nyc <- read_sas(DOE_dataset_courses_13,
                    cols_only = c("COURSETITLE",
                                  "TermCD",
                                  "CourseCD",
                                  "Mark",
                                  "NumericEquivalent",
                                  "PassFailEquivalent",
                                  "RANYCSID"))

# Check course codes
class(dat_nyc$CourseCD)
max(nchar(dat_nyc$CourseCD))

#=============== Select courses of interest

# Spit course code variable
x <- strsplit(dat_nyc$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_nyc <- as_tibble(cbind(dat_nyc, y))


# Filter only math subjects
dat_nyc <- dat_nyc %>%
  filter(X1 == "M")
freq_table(dat_nyc$X2)

#=============== Rename and recode variables
dat_nyc <- dat_nyc %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#=============== Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

# Definition of advanced math in 9th grade: geometry, algebra II, trigonometry, integrated math II through IV, or higher level math courses such as precalculus or calculus

# integrated alg
dat_nyc_i <- dat_nyc %>%
  filter(X2 == "E",
         X5 == 1)

# pre_calc
dat_nyc_p <- dat_nyc %>%
  filter(X2 == "P",
         X5 == 1)

# Geometry
dat_nyc_g <- dat_nyc %>%
  filter(X2 == "G",
         X5 == 1)

# Algebra 2 / trigonometry
dat_nyc_a <- dat_nyc %>%
  filter(X2 == "R",
         X5 == 1)

# Calc
dat_nyc_c <- dat_nyc %>%
  filter(X2 == "C",
         X5 == 1)

#=============== Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# I
dat_nyc_i <- agg(dat_nyc_i, dat_nyc_i$enrollment, "HS_math_intalg_n_courses", dat_nyc_i$RANYCSID, "RANYCSID")
dat_nyc_i <- agg(dat_nyc_i, dat_nyc_i$course_grade_pf, "HS_math_intalg_n_pass", dat_nyc_i$RANYCSID, "RANYCSID")
dat_nyc_i <- dat_nyc_i %>%
  select(RANYCSID,
         HS_math_intalg_n_courses,
         HS_math_intalg_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)

# p
dat_nyc_p <- agg(dat_nyc_p, dat_nyc_p$enrollment, "HS_math_precalc_n_courses", dat_nyc_p$RANYCSID, "RANYCSID")
dat_nyc_p <- agg(dat_nyc_p, dat_nyc_p$course_grade_pf, "HS_math_precalc_n_pass", dat_nyc_p$RANYCSID, "RANYCSID")
dat_nyc_p <- dat_nyc_p %>%
  select(RANYCSID,
         HS_math_precalc_n_courses,
         HS_math_precalc_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)


# G
dat_nyc_g <- agg(dat_nyc_g, dat_nyc_g$enrollment, "HS_math_geometry_n_courses", dat_nyc_g$RANYCSID, "RANYCSID")
dat_nyc_g <- agg(dat_nyc_g, dat_nyc_g$course_grade_pf, "HS_math_geometry_n_pass", dat_nyc_g$RANYCSID, "RANYCSID")
dat_nyc_g <- dat_nyc_g %>%
  select(RANYCSID,
         HS_math_geometry_n_courses,
         HS_math_geometry_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)


# A
dat_nyc_a <- agg(dat_nyc_a, dat_nyc_a$enrollment, "HS_math_alg2_n_courses", dat_nyc_a$RANYCSID, "RANYCSID")
dat_nyc_a <- agg(dat_nyc_a, dat_nyc_a$course_grade_pf, "HS_math_alg2_n_pass", dat_nyc_a$RANYCSID, "RANYCSID")
dat_nyc_a <- dat_nyc_a %>%
  select(RANYCSID,
         HS_math_alg2_n_courses,
         HS_math_alg2_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)


# C
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$enrollment, "HS_math_calc_n_courses", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$course_grade_pf, "HS_math_calc_n_pass", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- dat_nyc_c %>%
  select(RANYCSID,
         HS_math_calc_n_courses,
         HS_math_calc_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)

#=============== Merge course data with original data
# Var names
colnames(dat_nyc_i) <- paste(colnames(dat_nyc_i), "14", sep = "_")
colnames(dat_nyc_p) <- paste(colnames(dat_nyc_p), "14", sep = "_")
colnames(dat_nyc_g) <- paste(colnames(dat_nyc_g), "14", sep = "_")
colnames(dat_nyc_a) <- paste(colnames(dat_nyc_a), "14", sep = "_")
colnames(dat_nyc_c) <- paste(colnames(dat_nyc_c), "14", sep = "_")

# check intersection
x <- intersect(dat$RANYCSID_13, dat_nyc_i$RANYCSID_14) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_p$RANYCSID_14) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_g$RANYCSID_14) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_a$RANYCSID_14) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_c$RANYCSID_14) 
length(x)

# merge data
dat <- left_join(dat, dat_nyc_i, by = c("RANYCSID_13" = "RANYCSID_14"))
dat <- left_join(dat, dat_nyc_p, by = c("RANYCSID_13" = "RANYCSID_14"))
dat <- left_join(dat, dat_nyc_g, by = c("RANYCSID_13" = "RANYCSID_14"))
dat <- left_join(dat, dat_nyc_a, by = c("RANYCSID_13" = "RANYCSID_14"))
dat <- left_join(dat, dat_nyc_c, by = c("RANYCSID_13" = "RANYCSID_14"))

#=============== Check and adjust aggregated variables
# Check
length(unique(dat_nyc_i$RANYCSID_14))
length(unique(dat_nyc_p$RANYCSID_14))
length(unique(dat_nyc_g$RANYCSID_14))
length(unique(dat_nyc_a$RANYCSID_14))
length(unique(dat_nyc_c$RANYCSID_14))
length(unique(dat$RANYCSID_13))
nrow(dat)
ncol(dat)

# Transform course values with NAs into 0s
vars <- dat %>%
  select(starts_with("HS_math_")) %>%
  names()
dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#==============================================================
# 2012 Mathematics courses
#==============================================================

dat_nyc <- read_sas(DOE_dataset_courses_12,
                    cols_only = c("COURSETITLE",
                                  "TermCD",
                                  "CourseCD",
                                  "Mark",
                                  "NumericEquivalent",
                                  "PassFailEquivalent",
                                  "RANYCSID"))

# Check course codes
class(dat_nyc$CourseCD)
max(nchar(dat_nyc$CourseCD))

#=============== Select courses of interest

# Spit course code variable
x <- strsplit(dat_nyc$CourseCD, "", useBytes = T)
max.length <- max(sapply(x, length))
x <- lapply(x, function(v) { c(v, rep(NA, max.length-length(v)))})
y = data.frame(do.call(rbind, x))
dat_nyc <- as_tibble(cbind(dat_nyc, y))


# Filter only math subjects
dat_nyc <- dat_nyc %>%
  filter(X1 == "M")
freq_table(dat_nyc$X2)

#=============== Rename and recode variables
dat_nyc <- dat_nyc %>%
  rename(course_grade = Mark,
         course_grade_n = NumericEquivalent,
         course_grade_pf = PassFailEquivalent,
         course_code = CourseCD,
         course_title = COURSETITLE) %>%
  mutate(course_grade_pf = case_when(
    course_grade_pf == "P" ~ 1,
    TRUE ~ 0),
    enrollment = 1)

#=============== Create one dataset for each course.
# Note that I am selecting only the first course in the sequence 
# to avoid a duplicated count

# Definition of advanced math in 9th grade: geometry, algebra II, trigonometry, integrated math II through IV, or higher level math courses such as precalculus or calculus

# integrated alg
dat_nyc_i <- dat_nyc %>%
  filter(X2 == "E",
         X5 == 1)

# pre_calc
dat_nyc_p <- dat_nyc %>%
  filter(X2 == "P",
         X5 == 1)

# Geometry
dat_nyc_g <- dat_nyc %>%
  filter(X2 == "G",
         X5 == 1)

# Algebra 2 / trigonometry
dat_nyc_a <- dat_nyc %>%
  filter(X2 == "R",
         X5 == 1)

# Calc
dat_nyc_c <- dat_nyc %>%
  filter(X2 == "C",
         X5 == 1)

#=============== Aggregate variables at the student level
# the code here transforms the dataset from a course ID format to a student ID format

# I
dat_nyc_i <- agg(dat_nyc_i, dat_nyc_i$enrollment, "HS_math_intalg_n_courses", dat_nyc_i$RANYCSID, "RANYCSID")
dat_nyc_i <- agg(dat_nyc_i, dat_nyc_i$course_grade_pf, "HS_math_intalg_n_pass", dat_nyc_i$RANYCSID, "RANYCSID")
dat_nyc_i <- dat_nyc_i %>%
  select(RANYCSID,
         HS_math_intalg_n_courses,
         HS_math_intalg_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)

# p
dat_nyc_p <- agg(dat_nyc_p, dat_nyc_p$enrollment, "HS_math_precalc_n_courses", dat_nyc_p$RANYCSID, "RANYCSID")
dat_nyc_p <- agg(dat_nyc_p, dat_nyc_p$course_grade_pf, "HS_math_precalc_n_pass", dat_nyc_p$RANYCSID, "RANYCSID")
dat_nyc_p <- dat_nyc_p %>%
  select(RANYCSID,
         HS_math_precalc_n_courses,
         HS_math_precalc_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)


# G
dat_nyc_g <- agg(dat_nyc_g, dat_nyc_g$enrollment, "HS_math_geometry_n_courses", dat_nyc_g$RANYCSID, "RANYCSID")
dat_nyc_g <- agg(dat_nyc_g, dat_nyc_g$course_grade_pf, "HS_math_geometry_n_pass", dat_nyc_g$RANYCSID, "RANYCSID")
dat_nyc_g <- dat_nyc_g %>%
  select(RANYCSID,
         HS_math_geometry_n_courses,
         HS_math_geometry_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)


# A
dat_nyc_a <- agg(dat_nyc_a, dat_nyc_a$enrollment, "HS_math_alg2_n_courses", dat_nyc_a$RANYCSID, "RANYCSID")
dat_nyc_a <- agg(dat_nyc_a, dat_nyc_a$course_grade_pf, "HS_math_alg2_n_pass", dat_nyc_a$RANYCSID, "RANYCSID")
dat_nyc_a <- dat_nyc_a %>%
  select(RANYCSID,
         HS_math_alg2_n_courses,
         HS_math_alg2_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)


# C
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$enrollment, "HS_math_calc_n_courses", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- agg(dat_nyc_c, dat_nyc_c$course_grade_pf, "HS_math_calc_n_pass", dat_nyc_c$RANYCSID, "RANYCSID")
dat_nyc_c <- dat_nyc_c %>%
  select(RANYCSID,
         HS_math_calc_n_courses,
         HS_math_calc_n_pass) %>%
  distinct(RANYCSID, .keep_all = T)

#=============== Merge course data with original data
# Var names
colnames(dat_nyc_i) <- paste(colnames(dat_nyc_i), "13", sep = "_")
colnames(dat_nyc_p) <- paste(colnames(dat_nyc_p), "13", sep = "_")
colnames(dat_nyc_g) <- paste(colnames(dat_nyc_g), "13", sep = "_")
colnames(dat_nyc_a) <- paste(colnames(dat_nyc_a), "13", sep = "_")
colnames(dat_nyc_c) <- paste(colnames(dat_nyc_c), "13", sep = "_")

# check intersection
x <- intersect(dat$RANYCSID_13, dat_nyc_i$RANYCSID_13) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_p$RANYCSID_13) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_g$RANYCSID_13) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_a$RANYCSID_13) 
length(x)
x <- intersect(dat$RANYCSID_13, dat_nyc_c$RANYCSID_13) 
length(x)

# merge data
dat <- left_join(dat, dat_nyc_i, by = c("RANYCSID_13" = "RANYCSID_13"))
dat <- left_join(dat, dat_nyc_p, by = c("RANYCSID_13" = "RANYCSID_13"))
dat <- left_join(dat, dat_nyc_g, by = c("RANYCSID_13" = "RANYCSID_13"))
dat <- left_join(dat, dat_nyc_a, by = c("RANYCSID_13" = "RANYCSID_13"))
dat <- left_join(dat, dat_nyc_c, by = c("RANYCSID_13" = "RANYCSID_13"))

#=============== Check and adjust aggregated variables
# Check
length(unique(dat_nyc_i$RANYCSID_13))
length(unique(dat_nyc_p$RANYCSID_13))
length(unique(dat_nyc_g$RANYCSID_13))
length(unique(dat_nyc_a$RANYCSID_13))
length(unique(dat_nyc_c$RANYCSID_13))
length(unique(dat$RANYCSID_13))
nrow(dat)
ncol(dat)

# Transform course values with NAs into 0s
vars <- dat %>%
  select(starts_with("HS_math_")) %>%
  names()
dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#==============================================================
# Course variables
#==============================================================

#=============== add course variables to the original file
dat_courses <- dat
NYC = paste0(DOE_server_wd_data,"/dataset_1_cohort-11-12.Rdata")
load(NYC)

dat <- left_join(dat, dat_courses, 
                 by = ("RANYCSID_13" = "RANYCSID_13"), 
                 suffix = c("", "_delete"))

# delete duplicated variables (non-course variables in the later file)
dat <- dat %>%
  select(-ends_with("_delete"))
nrow(dat)

#=============== Descriptive stats for course variables
vars <- dat %>%
  select(starts_with("AP_n_courses"),
         starts_with("AP_n_pass"),
         starts_with("HS_math_"),
         EXM_APMTHPAS_13,
         EXM_APMTHPAS_14,
         EXM_APMTHPAS_15,
         EXM_APMTHPAS_16,
         EXM_APMTHATM_13,
         EXM_APMTHATM_14,
         EXM_APMTHATM_15, 
         EXM_APMTHATM_16)

# missing variables
pct_missing_variable(vars)

#=============== Create course variables of interest
dat <- dat %>%
  mutate(AP_math_pass_exam_n_13 = EXM_APMTHPAS_13,
         AP_math_pass_exam_n_14 = EXM_APMTHPAS_14,
         AP_math_pass_exam_n_15 = EXM_APMTHPAS_15,
         AP_math_pass_exam_n_16 = EXM_APMTHPAS_16,
         AP_math_exam_n_13 = EXM_APMTHATM_13,
         AP_math_exam_n_14 = EXM_APMTHATM_14,
         AP_math_exam_n_15 = EXM_APMTHATM_15,
         AP_math_exam_n_16 = EXM_APMTHATM_16,
         AP_math_pass_ratio_13 = AP_math_pass_exam_n_13 / AP_math_exam_n_13,
         AP_math_pass_ratio_14 = AP_math_pass_exam_n_14 / AP_math_exam_n_14,
         AP_math_pass_ratio_15 = AP_math_pass_exam_n_15 / AP_math_exam_n_15,
         AP_math_pass_ratio_16 = AP_math_pass_exam_n_16 / AP_math_exam_n_16,
         AP_n_courses_13 = AP_n_courses_calc_13 + AP_n_courses_stats_13,# + AP_n_courses_tech_13,
         AP_n_courses_14 = AP_n_courses_calc_14 + AP_n_courses_stats_14,# + AP_n_courses_tech_14,
         AP_n_courses_15 = AP_n_courses_calc_15 + AP_n_courses_stats_15,# + AP_n_courses_tech_15,
         AP_n_courses_16 = AP_n_courses_calc_16 + AP_n_courses_stats_16,# + AP_n_courses_tech_16,
         AP_n_pass_13 = AP_n_pass_calc_13 + AP_n_pass_stats_13,# + AP_n_pass_tech_13,
         AP_n_pass_14 = AP_n_pass_calc_14 + AP_n_pass_stats_14,# + AP_n_pass_tech_14,
         AP_n_pass_15 = AP_n_pass_calc_15 + AP_n_pass_stats_15,# + AP_n_pass_tech_15,
         AP_n_pass_16 = AP_n_pass_calc_16 + AP_n_pass_stats_16)# + AP_n_pass_tech_16)

dat <- dat %>%
  mutate(
    AP_math_enroll_13 = case_when(
      AP_n_courses_13 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_enroll_14 = case_when(
      AP_n_courses_14 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_enroll_15 = case_when(
      AP_n_courses_15 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_enroll_16 = case_when(
      AP_n_courses_16 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_pass_exam_13 = case_when(
      AP_math_pass_exam_n_13 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_pass_exam_14 = case_when(
      AP_math_pass_exam_n_14 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_pass_exam_15 = case_when(
      AP_math_pass_exam_n_15 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_pass_exam_16 = case_when(
      AP_math_pass_exam_n_16 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_exam_13 = case_when(
      AP_math_exam_n_13 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_exam_14 = case_when(
      AP_math_exam_n_14 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_exam_15 = case_when(
      AP_math_exam_n_15 >= 1 ~ 1,
      TRUE ~ 0),
    AP_math_exam_16 = case_when(
      AP_math_exam_n_16 >= 1 ~ 1,
      TRUE ~ 0))

dat <- dat %>%
  mutate(
    AP_math_enroll_prior = AP_n_courses_13 + 
      AP_n_courses_15 + 
      AP_n_courses_14,
    AP_math_pass_prior = EXM_APMTHPAS_13 +
      EXM_APMTHPAS_14 + 
      EXM_APMTHPAS_15,
    AP_math_exam_prior = EXM_APMTHATM_13 + 
      EXM_APMTHATM_14 + 
      EXM_APMTHATM_15 + 
      EXM_APMTHATM_16,
    AP_math_exam_pass_ratio_prior = AP_math_pass_prior / AP_math_exam_prior)

# Enroll_15 vs Enroll_16
table(dat$AP_math_enroll_15, dat$AP_math_enroll_16)

dat <- dat %>%
  mutate(
    AP_math_enroll = case_when(
      (AP_n_courses_13 >= 1 | 
         AP_n_courses_15 >= 1 |
         AP_n_courses_14 >= 1 |
         AP_n_courses_16 >= 1) ~ 1,
      TRUE ~ 0),
    AP_math_pass = case_when(
      (EXM_APMTHPAS_13 >= 1 | 
         EXM_APMTHPAS_14 >= 1 |
         EXM_APMTHPAS_15 >= 1 |
         EXM_APMTHPAS_16 >= 1) ~ 1,
      TRUE ~ 0),
    AP_math_exam = case_when(
      (EXM_APMTHATM_13 >= 1 | 
         EXM_APMTHATM_14 >= 1 |
         EXM_APMTHATM_15 >= 1 |
         EXM_APMTHATM_16 >= 1) ~ 1,
      TRUE ~ 0),
    AP_math_pass_course = case_when(
      (AP_n_pass_13 >= 1 | 
         AP_n_pass_15 >= 1 |
         AP_n_pass_14 >= 1 |
         AP_n_pass_16 >= 1) ~ 1,
      TRUE ~ 0))

# Enroll_16 vs Exam_pass_16
table(dat$AP_math_enroll_16, dat$AP_math_pass_exam_16)

#=============== School composition AP variables
dat <- dat %>%
  mutate(
    male_ap_exam = case_when(
      (BIO_male == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0),
    female_ap_exam = case_when(
      (BIO_female == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0),
    black_ap_exam = case_when(
      (BIO_black == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0),
    white_ap_exam = case_when(
      (BIO_white == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0),
    hispanic_ap_exam = case_when(
      (BIO_hispanic == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0),
    asian_ap_exam = case_when(
      (BIO_asian == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0),
    other_ap_exam = case_when(
      (BIO_race_other == 1 & AP_math_exam == 1) ~ 1,
      TRUE ~ 0))

dat <- dat %>%
  mutate(
    male_ap = case_when(
      (BIO_male == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    female_ap = case_when(
      (BIO_female == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    black_ap = case_when(
      (BIO_black == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    white_ap = case_when(
      (BIO_white == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    hispanic_ap = case_when(
      (BIO_hispanic == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    asian_ap = case_when(
      (BIO_asian == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    other_ap = case_when(
      (BIO_race_other == 1 & AP_math_enroll == 1) ~ 1,
      TRUE ~ 0),
    male_ap_pass = case_when(
      (BIO_male == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    female_ap_pass = case_when(
      (BIO_female == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    black_ap_pass = case_when(
      (BIO_black == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    white_ap_pass = case_when(
      (BIO_white == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    hispanic_ap_pass = case_when(
      (BIO_hispanic == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    asian_ap_pass = case_when(
      (BIO_asian == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    other_ap_pass = case_when(
      (BIO_race_other == 1 & AP_math_pass == 1) ~ 1,
      TRUE ~ 0),
    male_ap_pass_course = case_when(
      (BIO_male == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0),
    female_ap_pass_course = case_when(
      (BIO_female == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0),
    black_ap_pass_course = case_when(
      (BIO_black == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0),
    white_ap_pass_course = case_when(
      (BIO_white == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0),
    hispanic_ap_pass_course = case_when(
      (BIO_hispanic == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0),
    asian_ap_pass_course = case_when(
      (BIO_asian == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0),
    other_ap_pass_course = case_when(
      (BIO_race_other == 1 & AP_math_pass_course == 1) ~ 1,
      TRUE ~ 0))

dat <- agg(dat, dat$black_ap, "SCH_n_ap_black", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$white_ap, "SCH_n_ap_white", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$hispanic_ap, "SCH_n_ap_hispanic", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$other_ap, "SCH_n_ap_other", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$asian_ap, "SCH_n_ap_asian", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$male_ap, "SCH_n_ap_male", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$female_ap, "SCH_n_ap_female", dat$DBN_13, "DBN_13")

dat <- agg(dat, dat$black_ap_pass, "SCH_n_ap_pass_black", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$white_ap_pass, "SCH_n_ap_pass_white", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$hispanic_ap_pass, "SCH_n_ap_pass_hispanic", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$other_ap_pass, "SCH_n_ap_pass_other", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$asian_ap_pass, "SCH_n_ap_pass_asian", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$male_ap_pass, "SCH_n_ap_pass_male", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$female_ap_pass, "SCH_n_ap_pass_female", dat$DBN_13, "DBN_13")

dat <- agg(dat, dat$black_ap_exam, "SCH_n_ap_exam_black", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$white_ap_exam, "SCH_n_ap_exam_white", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$hispanic_ap_exam, "SCH_n_ap_exam_hispanic", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$other_ap_exam, "SCH_n_ap_exam_other", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$asian_ap_exam, "SCH_n_ap_exam_asian", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$male_ap_exam, "SCH_n_ap_exam_male", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$female_ap_exam, "SCH_n_ap_exam_female", dat$DBN_13, "DBN_13")

dat <- agg(dat, dat$black_ap_pass_course, "SCH_n_ap_pass_course_black", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$white_ap_pass_course, "SCH_n_ap_pass_course_white", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$hispanic_ap_pass_course, "SCH_n_ap_pass_course_hispanic", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$other_ap_pass_course, "SCH_n_ap_pass_course_other", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$asian_ap_pass_course, "SCH_n_ap_pass_course_asian", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$male_ap_pass_course, "SCH_n_ap_pass_course_male", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$female_ap_pass_course, "SCH_n_ap_pass_course_female", dat$DBN_13, "DBN_13")

#==============================================================
# Save data
#==============================================================
save(dat, file = paste0(DOE_server_wd_data,"/dataset_3_cohort-11-12.Rdata"))
