#==============================================================
# File description
#==============================================================
# contents: 
#  cleaning of attendance variables
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023 

#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#==============================================================
# Load school-level data
#==============================================================

load(paste0(DOE_server_wd_data,"/dataset_4_cohort-12-13.Rdata"))

#==============================================================
# Attendance files
#==============================================================

#============ 2011
dat_nyc <- read_sas(DOE_dataset_att_11, 
                    cols_only = c("RANYCSID",
                                   "ATTPCTROL")) # Annual attendance rate (denominator: days on roll)
# Var names
dat_nyc <- dat_nyc %>%
  rename(MS_att_11 = ATTPCTROL)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID"))

#============ 2012
dat_nyc <- read_sas(DOE_dataset_att_12, 
                    cols_only = c("RANYCSID",
                                   "ATTPCTROL"))
# Var names
dat_nyc <- dat_nyc %>%
  rename(MS_att_12 = ATTPCTROL)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID"))

#============ 2013
dat_nyc <- read_sas(DOE_dataset_att_13, 
                    cols_only = c("RANYCSID",
                                   "ATTPCTROL"))
# Var names
dat_nyc <- dat_nyc %>%
  rename(HS_att_13 = ATTPCTROL)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID"))

#============ 2014
dat_nyc <- read_sas(DOE_dataset_att_14, 
                    cols_only = c("RANYCSID",
                                   "ATTPCTROL"))
# Var names
dat_nyc <- dat_nyc %>%
  rename(HS_att_14 = ATTPCTROL)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID"))

#============ 2015
dat_nyc <- read_sas(DOE_dataset_att_15, 
                    cols_only = c("RANYCSID",
                                   "ATTPCTROL"))
# Var names
dat_nyc <- dat_nyc %>%
  rename(HS_att_15 = ATTPCTROL)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID"))

#==============================================================
# Save NYC file 
#============================================================== 
save(dat, file = paste0(DOE_server_wd_data,"/dataset_5_cohort-12-13.Rdata"))
