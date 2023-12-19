#==============================================================
# File description
#==============================================================
# contents: 
#  cleaning of biographical data and enrollment variables
#  selection of school-level variables
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

# ---> year-based subscripts for variables created in this file (e.g., "_13") are copied from the 2012 cohort. 
#      for the correct years, subtract 1.

#==============================================================
# Load school-level data
#==============================================================

prior_data = paste0(DOE_server_wd_data,"/dataset_1_cohort-11-12.Rdata")
load(prior_data)
dat_schools <- dat_schools_12

#==============================================================
# Freshman year
#==============================================================

dat_nyc <- read_sas(DOE_dataset_enr_12, 
                    cols_only = c("RANYCSID", 
                                   "DOEGLVOCT", # student grade oct. e.g: "12"
                                   "DOEGLVJUN",
                                   "ACTIVEOCT",
                                   "DBNOCT", # school ID number
                                   "AGDDCATOCT"))

#"PREVYR", #student discharged in previous year?

# filter
dat_nyc <- dat_nyc %>%
  rename(DBN = DBNOCT) %>%
  filter(DOEGLVOCT == "09",
         AGDDCATOCT == 1)

# Check total number of students before filter
dat_check <- inner_join(dat_schools, dat_nyc, by = c("DBN" = "DBN"))

g = nrow(dat_check)
paste("Total number of students enrolled in 9th grade in the Fall 2012 before filter", nrow(dat_check)) 

# Change variable names
colnames(dat_nyc) <- paste("ENR", colnames(dat_nyc), "13", sep = "_")

dat_nyc <- dat_nyc %>%
  rename(DBN = ENR_DBN_13,
         RANYCSID_13 = ENR_RANYCSID_13) 

# Merge files
x <- intersect(dat_schools$DBN, dat_nyc$DBN)
dat <- inner_join(dat_schools, dat_nyc, by = "DBN")

# Save 2013 file 
paste("Total number of students enrolled in 9th grade in the Fall 2012 after merge", nrow(dat)) 

s = nrow(dat)
a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")
paste("Total number of schools after merge", length(unique(dat$DBN)))

#==============================================================
# BIO data
# (in the Fall of their 9th grade)
#==============================================================

dat_nyc <- read_sas(DOE_dataset_bio_11,
                    cols_only = c("RANYCSID",
                                   "ETHNIC",
                                   "ETHCAT",
                                   "FRLUNCH",
                                   "HLANGCAT", 
                                   "GENDER", 
                                   "DOB",
                                   "GEOCODE"))

#==============================================================
# Merge files
#==============================================================

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID)
dat <- inner_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID"))

paste("Total number of students enrolled in 9th grade in the Fall 2012 after merge", nrow(dat)) 
s = nrow(dat)
a = nrow(dat)
paste("Pct. of grade = ", 100 * round(a/g, 2), "%")
paste("Pct. of sample = ", 100 * round(a/s, 2), "%")
paste("Total number of schools after merge", length(unique(dat$DBN)))

dat <- dat %>%
  mutate(
    DIST_id = GEOCODE,
    BIO_female = case_when(
      GENDER == "F" ~ 1,
      TRUE ~ 0),
    BIO_male = case_when(
      GENDER == "M" ~ 1,
      TRUE ~ 0),
    BIO_black = case_when(
      ETHCAT == 4 ~ 1,
      TRUE ~ 0),
    BIO_white = case_when(
      ETHCAT == 5 ~ 1,
      TRUE ~ 0),
    BIO_asian = case_when(
      ETHCAT == 2 ~ 1,
      TRUE ~ 0),
    BIO_hispanic = case_when(
      ETHCAT == 3 ~ 1,
      TRUE ~ 0),
    BIO_race_other = case_when(
      ETHCAT == 1 | ETHCAT == 7 ~ 1,
      TRUE ~ 0),
    BIO_race_not_reported = case_when(
      ETHCAT == 6 ~ 1,
      TRUE ~ 0),
    BIO_hlang_not_english = case_when(
      HLANGCAT == 10 ~ 0,
      TRUE ~ 1),
    BIO_fr_lunch = case_when(
      FRLUNCH == 1 ~ 1,
      TRUE ~ 0))

# Create a birth year variable
dat <- dat %>%
  mutate(DOB = as.character(DOB))
x <- strsplit(dat$DOB,"-")
y <- data.frame(do.call(rbind, x))
dat <- cbind(dat, y$X1)
dat <- dat %>%
  rename(BIO_birth_year = "y$X1") %>%
  mutate(BIO_birth_year = as.character(BIO_birth_year)) %>%
  mutate(BIO_birth_year = as.numeric(BIO_birth_year))

dat <- dat %>%
  mutate(BIO_race_cat = case_when(
    BIO_black == 1 ~ "Black",
    BIO_white == 1 ~ "White",
    BIO_asian == 1 ~ "Asian",
    BIO_hispanic == 1 ~ "Hispanic",
    BIO_race_other == 1 ~ "Other",
    BIO_race_not_reported == 1 ~ "Not reported")) %>%
  select(-ETHNIC,
         -ETHCAT,
         -HLANGCAT, 
         -GENDER, 
         -FRLUNCH,
         -DOB)

# Adjust variables
dat <- dat %>%
  mutate(BIO_race_factor = as.factor(BIO_race_cat),
         BIO_race_factor = relevel(BIO_race_factor, ref = "White")) %>%
  rename(BIO_age = BIO_birth_year) %>%
  mutate(BIO_age = 2013 - BIO_age)

#==============================================================
# School composition variables
#==============================================================
dat <- dat %>%
  mutate(ones = 1)

dat <- dat %>%
  rename(DBN_13 = DBN)

dat <- agg(dat, dat$ones, "SCH_n_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_race_other, "SCH_n_other_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_asian, "SCH_n_asian_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_hispanic, "SCH_n_hispanic_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_white, "SCH_n_white_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_black, "SCH_n_black_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_male, "SCH_n_male_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_female, "SCH_n_female_total", dat$DBN_13, "DBN_13")
dat <- agg(dat, dat$BIO_fr_lunch, "SCH_n_fr_lunch_total", dat$DBN_13, "DBN_13")

dat <- dat %>%
  mutate(k_asian = ifelse(SCH_n_asian_total > 0, 1, 0),
         k_hispanic = ifelse(SCH_n_hispanic_total > 0, 1, 0),
         k_other = ifelse(SCH_n_other_total > 0, 1, 0),
         k_black = ifelse(SCH_n_black_total > 0, 1, 0),
         k_white = ifelse(SCH_n_white_total > 0, 1, 0),
         sum_f_k_squared =
           (SCH_n_asian_total * SCH_n_asian_total) + 
           (SCH_n_hispanic_total * SCH_n_hispanic_total) + 
           (SCH_n_other_total * SCH_n_other_total) + 
           (SCH_n_black_total * SCH_n_black_total) + 
           (SCH_n_white_total * SCH_n_white_total),
         n_k = k_asian + k_hispanic + k_other + k_black + k_white,
         n_squared = SCH_n_total * SCH_n_total,
         SCH_diversity_index = (n_k * (n_squared - sum_f_k_squared)) / (n_squared * (n_k - 1)))

dat <- dat %>%
  mutate(SCH_diversity_index = replace(SCH_diversity_index,is.na(SCH_diversity_index), 0))
dat <- dat %>%
  mutate(SCH_diversity_index = replace(SCH_diversity_index, 
                                       SCH_diversity_index > 1,
                                       1))
dat <- dat %>%
  mutate(SCH_pct_asian = SCH_n_asian_total / SCH_n_total,
         SCH_pct_hispanic = SCH_n_hispanic_total / SCH_n_total,
         SCH_pct_other = SCH_n_other_total / SCH_n_total, 
         SCH_pct_black = SCH_n_black_total / SCH_n_total,
         SCH_pct_white = SCH_n_white_total / SCH_n_total,
         SCH_pct_black_white = (SCH_n_white_total + SCH_n_black_total) / SCH_n_total,
         SCH_pct_female = SCH_n_female_total / SCH_n_total,
         SCH_pct_fr_lunch = SCH_n_fr_lunch_total / SCH_n_total)

# Save NYC file 
save(dat, file = paste0(DOE_server_wd_data,"/dataset_2_cohort-11-12.Rdata"))

