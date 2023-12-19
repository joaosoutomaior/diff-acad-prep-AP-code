#==============================================================
# File description
#==============================================================
# contents: 
#  filter data at the school level
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

# Directory
setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#==============================================================
# Select variables of interest (2011 cohort)
#==============================================================

# print results to vizualize if needed
#sink("output/school-level-filter.txt")

NYC = paste0(DOE_server_wd_data,"/dataset_5_cohort-11-12.Rdata")
load(NYC)

# Adjust ID variables
dat <- dat %>%
  rename(ID_school = DBN_13,
         ID_student = RANYCSID_13)

# Select variables of interest
dat <- dat %>%
  select(starts_with("BEHAV_"),
         starts_with("BIO_"),
         starts_with("DIST_"),
         starts_with("BEHAV"),
         starts_with("HS_C"),
         starts_with("HS_att"),
         starts_with("HS_math_"),
         starts_with("MS_"),
         starts_with("SCH_"),
         starts_with("ID_"),
         contains("_ap"),
         starts_with("AP_"))

# Make sure numeric variables are correctly classified
numeric_vars <- dat %>%
  select(starts_with("BEHAV_"),
         #starts_with("BIO_"),
         #starts_with("DIST_"),
         starts_with("BEHAV"),
         starts_with("HS_C"),
         starts_with("HS_att"),
         starts_with("MS_"),
         #starts_with("SCH_")
  ) %>%
  names()

dat <- dat %>%
  mutate(across(.cols = all_of(c(numeric_vars)),
                .fns = ~as.numeric(.x)))

# Pct of cohort enrolled in At least one AP math course 
# by the end of their senior year
# (variable could have been created earlier in the code)
dat <- dat %>%
  mutate(ones = 1)
dat <- agg(dat, dat$AP_math_enroll, "SCH_ap_math_enrollment", dat$ID_school, "ID_school")
dat <- agg(dat, dat$ones, "SCH_HS_size_9g", dat$ID_school, "ID_school")
dat <- dat %>%
  mutate(SCH_pct_apmath_enroll = SCH_ap_math_enrollment / SCH_HS_size_9g)

#==============================================================
# Change variable suffixes from year to grade
#==============================================================

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_11.*", "_07", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_12.*", "_08", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_13.*", "_09", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_14.*", "_10", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_15.*", "_11", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_16.*", "_12", colnames(dat)[col])
}

#==============================================================
# Filter schools in the 2011 cohort
#==============================================================

paste("Initial sample", nrow(dat)) 
paste("Initial sample - schools", length(unique(dat$ID_school)))

dat <- dat %>%
  filter(SCH_pct_apmath_enroll> 0)

paste("After AP filter", nrow(dat)) 
paste("After AP filter - schools", length(unique(dat$ID_school)))

dat <- dat %>%
  filter(SCH_n_black_total > 0,
         SCH_n_white_total > 0)

paste("After black white filter", nrow(dat)) 
paste("After black white filter - schools", length(unique(dat$ID_school)))

dat <- dat %>%
  filter(BIO_race_factor != "Not reported")
dat$BIO_race_factor <- droplevels(dat$BIO_race_factor)

paste("After not reported filter", nrow(dat)) 
paste("After not reported filter", length(unique(dat$ID_school)))

dat <- dat %>%
  mutate(AP_math_pass_factor = as.factor(AP_math_pass),
         AP_math_enroll_factor = as.factor(AP_math_enroll),
         AP_math_pass_course_factor = as.factor(AP_math_pass_course))

dat <- dat %>%
  select(-BIO_race_cat,
         -BIO_race_not_reported)

dat_11 <- dat

paste("Total number of students in 2011 cohort sample", nrow(dat_11)) 
paste("Total number of schools in 2011 cohort sample", length(unique(dat_11$ID_school))) 

#==============================================================
# Select variables of interest (2012 cohort)
#==============================================================

rm(dat)
NYC = paste0(DOE_server_wd_data,"/dataset_5_cohort-12-13.Rdata")
load(NYC)

# Adjust ID variables
dat <- dat %>%
  rename(ID_school = DBN_13,
         ID_student = RANYCSID_13)

# Select variables of interest
dat <- dat %>%
  select(starts_with("BEHAV_"),
         starts_with("BIO_"),
         starts_with("DIST_"),
         starts_with("BEHAV"),
         starts_with("HS_math_"),
         starts_with("HS_C"),
         starts_with("HS_att"),
         starts_with("MS_"),
         starts_with("SCH_"),
         starts_with("ID_"),
         contains("_ap"),
         starts_with("AP_"))


# Make sure numeric variables are correctly classified
numeric_vars <- dat %>%
  select(starts_with("BEHAV_"),
         #starts_with("BIO_"),
         #starts_with("DIST_"),
         starts_with("BEHAV"),
         starts_with("HS_C"),
         starts_with("HS_att"),
         starts_with("MS_"),
         #starts_with("SCH_")
  ) %>%
  names()

dat <- dat %>%
  mutate(across(.cols = all_of(c(numeric_vars)),
                .fns = ~as.numeric(.x)))

#==============================================================
# Change variable suffixes from year to grade
#==============================================================

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_11.*", "_07", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_12.*", "_08", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_13.*", "_09", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_14.*", "_10", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_15.*", "_11", colnames(dat)[col])
}

for ( col in 1:ncol(dat)){
  colnames(dat)[col] <-  sub("_16.*", "_12", colnames(dat)[col])
}

# Pct of cohort enrolled in At least one AP math course 
# by the end of their senior year
# (variable could have been created earlier in the code)

dat <- dat %>%
  mutate(ones = 1)
dat <- agg(dat, dat$AP_math_enroll, "SCH_ap_math_enrollment", dat$ID_school, "ID_school")
dat <- agg(dat, dat$ones, "SCH_HS_size_9g", dat$ID_school, "ID_school")
dat <- dat %>%
  mutate(SCH_pct_apmath_enroll = SCH_ap_math_enrollment / SCH_HS_size_9g)

#==============================================================
# Filter schools in the 2012 cohort
#==============================================================

paste("Initial sample", nrow(dat)) 
paste("Initial sample - schools", length(unique(dat$ID_school)))

dat <- dat %>%
  filter(SCH_pct_apmath_enroll> 0)

paste("After AP filter", nrow(dat)) 
paste("After AP filter - schools", length(unique(dat$ID_school)))

dat <- dat %>%
  filter(SCH_n_black_total > 0,
         SCH_n_white_total > 0)

paste("After black white filter", nrow(dat)) 
paste("After black white filter - schools", length(unique(dat$ID_school)))

dat <- dat %>%
  filter(BIO_race_factor != "Not reported")
dat$BIO_race_factor <- droplevels(dat$BIO_race_factor)

paste("After not reported filter", nrow(dat)) 
paste("After not reported filter", length(unique(dat$ID_school)))

dat <- dat %>%
  mutate(AP_math_pass_factor = as.factor(AP_math_pass),
         AP_math_enroll_factor = as.factor(AP_math_enroll),
         AP_math_pass_course_factor = as.factor(AP_math_pass_course))

dat <- dat %>%
  select(-BIO_race_cat,
         -BIO_race_not_reported)

dat_12 <- dat
rm(dat)
paste("Total number of students in 2012 cohort sample", nrow(dat_12)) 
paste("Total number of schools in 2012 cohort sample", length(unique(dat_12$ID_school))) 

#==============================================================
# Final sample of schools
#==============================================================

dat_12 <- dat_12 %>%
  mutate(cohort = 2012)

dat_11 <- dat_11 %>%
  mutate(cohort = 2011)

#================ Only select high schools in the 2 cohorts
school_IDs <- intersect(dat_11$ID_school, dat_12$ID_school)
length(school_IDs)

# check columns which do not match across datasets
x = names(dat_12)
y = names(dat_11)
matched = intersect(y,x)
all = union(x,y)
out = all[!all %in% matched]
out # ---> "BEHAV_SUSINFTOT_08" "BEHAV_SUSINFTOTELM_08"

# adjust accordingly
dat_11 <- dat_11 %>%
  select(-BEHAV_SUSINFTOT_08,
         -BEHAV_SUSINFTOTELM_08)

# append datasets
df <- rbind(dat_12, dat_11)

# Create logical variables
df <- df %>%
  mutate(AP_math_pass_logical = as.logical(AP_math_pass),
         AP_math_enroll_logical = as.logical(AP_math_enroll),
         AP_math_exam_logical = as.logical(AP_math_exam))

df <- df %>%
  filter(ID_school %in% school_IDs)

paste("Final number of schools in the sample", length(unique(df$ID_school))) 

# to save results
#sink()

save(df, file = paste0(DOE_server_wd_data,"/dataset_6_all_schools.Rdata"))
