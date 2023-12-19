#==============================================================
# File description
#==============================================================
# contents: 
#  cleaning of test score variables
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#============================================================
# Header
#============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#============================================================
# School level variables in the 2012-13 academic year
#============================================================

dat_hs <- read_sas(DOE_dataset_enr_13, 
                   cols_only = c("RANYCSID", 
                                  "DOEGLVOCT", # student grade oct. e.g: "12"
                                  "DOEGLVJUN",
                                  "ACTIVEOCT",
                                  "DBNOCT", # school ID number
                                  "AGDDCATOCT"))

# filter all active students in the HS in the 2011-12 academic year
dat_hs <- dat_hs %>%
  rename(DBN = DBNOCT) %>%
  filter(DOEGLVOCT == "09" | 
           DOEGLVOCT == "10" |
           DOEGLVOCT == "11" |
           DOEGLVOCT == "12",
         AGDDCATOCT == 1)

dat_hs <- dat_hs %>%
  mutate(ones = 1)

dat_hs <- agg(dat_hs, dat_hs$ones, "SCH_HS_total_size", dat_hs$DBN, "DBN")

dat_hs <- dat_hs %>%
  select(SCH_HS_total_size,
         RANYCSID,
         DBN)

#============= exams (2013)
dat_nyc <- read_sas(DOE_dataset_exams_13)

# select variables of interest
dat_nyc <- dat_nyc %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

x <- intersect(dat_hs$RANYCSID, dat_nyc$RANYCSID)
length(x)
dat_hs <- left_join(dat_hs, dat_nyc, by = c("RANYCSID" = "RANYCSID"))

dat_hs <- dat_hs %>%
  mutate(
    AP_math_atm = case_when(
      (APMTHATM >= 1) ~ 1,
      TRUE ~ 0),
    AP_math_pass = case_when(
      (APMTHATM >= 1) ~ 1,
      TRUE ~ 0))

dat_hs <- agg(dat_hs, dat_hs$AP_math_atm, "SCH_HS_n_ap_math_atm", dat_hs$DBN, "DBN")
dat_hs <- agg(dat_hs, dat_hs$AP_math_pass, "SCH_HS_n_ap_math_pass", dat_hs$DBN, "DBN")

dat_hs <- dat_hs %>%
  mutate(SCH_HS_pct_ap_math_atm = SCH_HS_n_ap_math_atm / SCH_HS_total_size,
         SCH_HS_pct_ap_math_pass = SCH_HS_n_ap_math_pass / SCH_HS_n_ap_math_atm)

# merge slope data with original data
dat_hs <- dat_hs %>%
  select(DBN, SCH_HS_total_size,
         starts_with("SCH_HS_pct")) %>%
  distinct(DBN, .keep_all = TRUE)

lapply(dat_hs, summary)

#============================================================
# Adjust size of data to students in the cohort of interest
#============================================================

NYC = paste0(DOE_server_wd_data,"/dataset_1_cohort-12-13.Rdata")
load(NYC)

x <- intersect(dat$DBN_13, dat_hs$DBN)
dat <- left_join(dat, dat_hs, by = c("DBN_13" = "DBN"))

#============================================================
# Exams
#============================================================

#============= 2013

colnames(dat_nyc) <- paste("EXM", colnames(dat_nyc), "13", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_13a = EXM_RANYCSID_13)

vars <- dat_nyc %>%
  select(-RANYCSID_13a) %>%
  names()

# Merge files
paste0("Number of students in 2013 exam file: ", length(unique(dat_nyc$RANYCSID_13a)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_13a)
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_13a"))

dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#============= 2014
dat_nyc <- read_sas(DOE_dataset_exams_14)

# select variables of interest
dat_nyc <- dat_nyc %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

# Change variable names
colnames(dat_nyc) <- paste("EXM", colnames(dat_nyc), "14", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_14 = EXM_RANYCSID_14)

vars <- dat_nyc %>%
  select(-RANYCSID_14) %>%
  names()

# Merge files
paste0("Number of students in 2014 exam file: ", length(unique(dat_nyc$RANYCSID_14)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_14) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_14"))

dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#============= 2015
dat_nyc <- read_sas(DOE_dataset_exams_15)

# select variables of interest
dat_nyc <- dat_nyc %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

# Change variable names
colnames(dat_nyc) <- paste("EXM", colnames(dat_nyc), "15", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_15 = EXM_RANYCSID_15)

vars <- dat_nyc %>%
  select(-RANYCSID_15) %>%
  names()

# Merge files
paste0("Number of students in 2015 exam file: ", length(unique(dat_nyc$RANYCSID_15)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_15)
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_15"))

dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

#============= 2016
dat_nyc <- read_sas(DOE_dataset_exams_16)

# select variables of interest
dat_nyc <- dat_nyc %>%
  select(RANYCSID,
         APMTHATM,
         APMTHPAS,
         APCSCATM,
         APCSCPAS)

# Change variable names
colnames(dat_nyc) <- paste("EXM", colnames(dat_nyc), "16", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_16 = EXM_RANYCSID_16)

vars <- dat_nyc %>%
  select(-RANYCSID_16) %>%
  names()

# Merge files
paste0("Number of students in 2016 exam file: ", length(unique(dat_nyc$RANYCSID_16)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_16)
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_16"))

dat <- dat %>%
  mutate(across(.cols = all_of(c(vars)),
                .fns = ~replace(.,is.na(.),0)))

# Save NYC file 
save(dat, file = paste0(DOE_server_wd_data,"/dataset_2_cohort-12-13.Rdata"))
