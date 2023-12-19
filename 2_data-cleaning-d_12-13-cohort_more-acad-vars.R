#==============================================================
# File description
#==============================================================
# contents: 
#  cleaning of high school credit variables
#  cleaning of middle school credit variables
#  cleaning of middle school state assessment variables
#  cleaning of suspensins variables
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023 
#==============================================================
# Header
#==============================================================

setwd(DOE_server_wd)
source(paste0(DOE_server_wd_code, "/header_server.R"))

#==============================================================
# Previous version of the school data
#==============================================================

load(paste0(DOE_server_wd_data,"/dataset_3_cohort-12-13.Rdata"))

#==============================================================
# Credits files
#==============================================================

#============ 2013
dat_nyc <- read_sas(DOE_dataset_HScredits_13, 
                    cols_only = c("RANYCSID", 
                                  "CRDTOTATM", #TOTAL
                                  "CRSTOTPAS",
                                  "CRSTOTFAL", 
                                  "CRDTOTERN",
                                  "CRSTOTGPAW", 
                                  
                                  "CRDMTHATM", #MATH
                                  "CRDMTHERN",
                                  "CRSMTHFAL",
                                  "CRSMTHPAS",
                                  "CRSMTHGPAW",
                                  
                                  "CRDENGATM", #ENG
                                  "CRDENGERN",
                                  "CRSENGFAL",
                                  "CRSENGPAS",
                                  "CRSENGGPAW",
                                  
                                  "CRDSCIATM", #SCI
                                  "CRDSCIERN",
                                  "CRSSCIFAL",
                                  "CRSSCIPAS",
                                  "CRSSCIGPAW",
                                  
                                  "CRDNACATM", #NAC
                                  "CRDNACERN",
                                  "CRSNACFAL",
                                  "CRSNACPAS",
                                  "CRSNACGPAW",
                                  
                                  "CRDACDATM", #ACAD
                                  "CRDACDERN",
                                  "CRSACDFAL",
                                  "CRSACDPAS",
                                  "CRSACDGPAW"))

# Var names
colnames(dat_nyc) <- paste("HS", colnames(dat_nyc), "13", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_13 = HS_RANYCSID_13)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_13)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_13) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_13"))

#============ 2014
dat_nyc <- read_sas(DOE_dataset_HScredits_14, 
                    cols_only = c("RANYCSID", 
                                  "CRDTOTATM", #TOTAL
                                  "CRSTOTPAS",
                                  "CRSTOTFAL", 
                                  "CRDTOTERN",
                                  "CRSTOTGPAW", 
                                  
                                  "CRDMTHATM", #MATH
                                  "CRDMTHERN",
                                  "CRSMTHFAL",
                                  "CRSMTHPAS",
                                  "CRSMTHGPAW",
                                  
                                  "CRDENGATM", #ENG
                                  "CRDENGERN",
                                  "CRSENGFAL",
                                  "CRSENGPAS",
                                  "CRSENGGPAW",
                                  
                                  "CRDSCIATM", #SCI
                                  "CRDSCIERN",
                                  "CRSSCIFAL",
                                  "CRSSCIPAS",
                                  "CRSSCIGPAW",
                                  
                                  "CRDNACATM", #NAC
                                  "CRDNACERN",
                                  "CRSNACFAL",
                                  "CRSNACPAS",
                                  "CRSNACGPAW",
                                  
                                  "CRDACDATM", #ACAD
                                  "CRDACDERN",
                                  "CRSACDFAL",
                                  "CRSACDPAS",
                                  "CRSACDGPAW"))

# Var names
colnames(dat_nyc) <- paste("HS", colnames(dat_nyc), "14", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_14 = HS_RANYCSID_14)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_14)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_14) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_14"))

#============ 2015
dat_nyc <- read_sas(DOE_dataset_HScredits_15, 
                    cols_only = c("RANYCSID", 
                                  "CRDTOTATM", #TOTAL
                                  "CRSTOTPAS",
                                  "CRSTOTFAL", 
                                  "CRDTOTERN",
                                  "CRSTOTGPAW", 
                                  
                                  "CRDMTHATM", #MATH
                                  "CRDMTHERN",
                                  "CRSMTHFAL",
                                  "CRSMTHPAS",
                                  "CRSMTHGPAW",
                                  
                                  "CRDENGATM", #ENG
                                  "CRDENGERN",
                                  "CRSENGFAL",
                                  "CRSENGPAS",
                                  "CRSENGGPAW",
                                  
                                  "CRDSCIATM", #SCI
                                  "CRDSCIERN",
                                  "CRSSCIFAL",
                                  "CRSSCIPAS",
                                  "CRSSCIGPAW",
                                  
                                  "CRDNACATM", #NAC
                                  "CRDNACERN",
                                  "CRSNACFAL",
                                  "CRSNACPAS",
                                  "CRSNACGPAW",
                                  
                                  "CRDACDATM", #ACAD
                                  "CRDACDERN",
                                  "CRSACDFAL",
                                  "CRSACDPAS",
                                  "CRSACDGPAW"))

# Var names
colnames(dat_nyc) <- paste("HS", colnames(dat_nyc), "15", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_15 = HS_RANYCSID_15)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_15)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_15) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_15"))

#==============================================================
# Middle school credits
#==============================================================

#============ 2012
dat_nyc <- read_sas(DOE_dataset_MScredits_12, 
                    cols_only = c("RANYCSID", 
                                   "CREDATMTOT", #total credits attempted
                                   "CRSCFALTOT", #total credits failed
                                   "CRSCPASTOT",
                                   "CREDERNTOT",
                                   "CRSCGPAWTOT", #weighted gpa for total credits
                                   "CRSCFALACD", #Non-academic courses
                                   "CRSCPASACD",
                                   "CREDATMACD",
                                   "CREDERNACD",
                                   "CRSCGPAWACD"
                    ))

# Var names
colnames(dat_nyc) <- paste("MS", colnames(dat_nyc), "12", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_12 = MS_RANYCSID_12)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_12)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_12) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_12"))

x = as.list(x)
dat$MS_course_data_12 <- as.numeric(dat$RANYCSID_13 %in% x)

#============ 2011
dat_nyc <- read_sas(DOE_dataset_MScredits_11, 
                    cols_only = c("RANYCSID", 
                                   "CREDATMTOT", #total credits attempted
                                   "CRSCFALTOT", #total credits failed
                                   "CRSCPASTOT",
                                   "CREDERNTOT",
                                   "CRSCGPAWTOT", #weighted gpa for total credits
                                   "CRSCFALACD", #Non-academic courses
                                   "CRSCPASACD",
                                   "CREDATMACD",
                                   "CREDERNACD",
                                   "CRSCGPAWACD"
                    ))

# Var names
colnames(dat_nyc) <- paste("MS", colnames(dat_nyc), "11", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_11 = MS_RANYCSID_11)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_11)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_11) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_11"))

x = as.list(x)
dat$MS_course_data_11 <- as.numeric(dat$RANYCSID_13 %in% x)

#==============================================================
# Middle school state exams
#==============================================================

#============ 2012
dat_nyc <- read_sas(DOE_dataset_MSstateexams_12, 
                    cols_only = c("RANYCSID", 
                                  #MTH 
                                  "MTHGRD",# test grade
                                  "MTHPCTCAL", # percentile, calculated score
                                  "MTHPLV", # performance level
                                  "MTHSSC",# scaled score
                                  "MTHTSD",# tested status
                                  "MTHPOV",# poverty category
                                  #ELA
                                  "ELAGRD",# test grade
                                  "ELAPCTCAL",# percentile, calculated score
                                  "ELAPLV", # performance level
                                  "ELASSC",# scaled score
                                  "ELATSD", # tested status
                                  "ELAPOV"))# poverty category
# Var names
colnames(dat_nyc) <- paste("MS", colnames(dat_nyc), "12", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_12 = MS_RANYCSID_12)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_12)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_12) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_12"))

#============ 2011
dat_nyc <- read_sas(DOE_dataset_MSstateexams_11, 
                    cols_only = c("RANYCSID", 
                                  #MTH 
                                  "MTHGRD",# test grade
                                  "MTHPCTCAL", # percentile, calculated score
                                  "MTHPLV", # performance level
                                  "MTHSSC",# scaled score
                                  "MTHTSD",# tested status
                                  "MTHPOV",# poverty category
                                  #ELA
                                  "ELAGRD",# test grade
                                  "ELAPCTCAL",# percentile, calculated score
                                  "ELAPLV", # performance level
                                  "ELASSC",# scaled score
                                  "ELATSD", # tested status
                                  "ELAPOV"))# poverty category
# Var names
colnames(dat_nyc) <- paste("MS", colnames(dat_nyc), "11", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_11 = MS_RANYCSID_11)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_11)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_11) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_11"))

#==============================================================
# Suspensions
#==============================================================

#============ 2011
dat_nyc <- read_sas(DOE_dataset_susp_11, 
                    cols_only = c("RANYCSID", 
                                  "SUSTOT", # total N of suspensions
                                  "SUSTOTDAYS", # total days suspended
                                  "SUSINFTOT", # total suspension type = infractions
                                  "SUSINFTOTELM" # total suspension type = elementary infractions
                    ))

# Var names
colnames(dat_nyc) <- paste("BEHAV", colnames(dat_nyc), "11", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_11 = BEHAV_RANYCSID_11)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_11)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_11) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_11"))

#============ 2012
dat_nyc <- read_sas(DOE_dataset_susp_12,
                    cols_only = c("RANYCSID", 
                                  "SUSALLTOT", 
                                  "SUSALLDAYSTOT")) %>%
  rename(SUSTOT = SUSALLTOT,  # names have changed in the server for this dataset
         SUSTOTDAYS = SUSALLDAYSTOT)

# Var names
colnames(dat_nyc) <- paste("BEHAV", colnames(dat_nyc), "12", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_12 = BEHAV_RANYCSID_12)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_12)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_12) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_12"))

#============ 2013
dat_nyc <- read_sas(DOE_dataset_susp_13,
                    cols_only = c("RANYCSID", 
                                  "SUSALLTOT", 
                                  "SUSALLDAYSTOT")) %>%
  rename(SUSTOT = SUSALLTOT,  # names have changed in the server in the server for this dataset
         SUSTOTDAYS = SUSALLDAYSTOT)

# Var names
colnames(dat_nyc) <- paste("BEHAV", colnames(dat_nyc), "13", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_13 = BEHAV_RANYCSID_13)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_13)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_13) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_13"))

#============ 2014
dat_nyc <- read_sas(DOE_dataset_susp_14,
                    cols_only = c("RANYCSID", 
                                  "SUSALLTOT", 
                                  "SUSALLDAYSTOT")) %>%
  rename(SUSTOT = SUSALLTOT,  # names have changed in the server in the server for this dataset
         SUSTOTDAYS = SUSALLDAYSTOT)

# Var names
colnames(dat_nyc) <- paste("BEHAV", colnames(dat_nyc), "14", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_14 = BEHAV_RANYCSID_14)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_14)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_14) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_14"))

#============ 2015
dat_nyc <- read_sas(DOE_dataset_susp_15,
                    cols_only = c("RANYCSID", 
                                  "SUSALLTOT", 
                                  "SUSALLDAYSTOT")) %>%
  rename(SUSTOT = SUSALLTOT,  # names have changed in the server in the server for this dataset
         SUSTOTDAYS = SUSALLDAYSTOT)

# Var names
colnames(dat_nyc) <- paste("BEHAV", colnames(dat_nyc), "15", sep = "_")
dat_nyc <- dat_nyc %>%
  rename(RANYCSID_15 = BEHAV_RANYCSID_15)

# Merge
paste0("Number of students in transcript file: ", length(unique(dat_nyc$RANYCSID_15)))
paste0("Number of students in the dat file: ", length(unique(dat$RANYCSID_13)))

x <- intersect(dat$RANYCSID_13, dat_nyc$RANYCSID_15) 
length(x)
dat <- left_join(dat, dat_nyc, by = c("RANYCSID_13" = "RANYCSID_15"))

#==============================================================
# Save data
#==============================================================
save(dat, file = paste0(DOE_server_wd_data,"/dataset_4_cohort-12-13.Rdata"))
