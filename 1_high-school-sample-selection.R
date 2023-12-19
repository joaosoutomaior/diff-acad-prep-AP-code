#==============================================================
# File description
#==============================================================
# contents: 
#  selection of high schools of interest
#  selection of school-level variables
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

#==============================================================
# Set up 
#==============================================================

# clear lists
rm(list=ls())

# load stored functions and values
source("header_basic.R")
source("header_plotting-functions.R")

# load additional packages
library(kableExtra)
library(mltools)
library(data.table)

# save outputs
sink("output/high-school-sample-selection.txt")

# load file and variables of interest
schools <- fread(SCHMA, select = c("BEDSCODE",
                                   "GEOID",
                                   "ATTSCHNAM",
                                   "ENRNUMTOT", #total enrollment
                                   "ENRNUMG09",
                                   "ENRNUMG10",
                                   "ENRNUMG11",
                                   "ENRNUMG12",
                                   "DMGPCTELLTOT", # pct ell
                                   "ATTPCTG09",
                                   "CLSAVGG09",
                                   "CLSAVGG10",
                                   "CLSAVGG11",
                                   "CLSAVGG12",
                                   "CBO", # community based org
                                   "CTMSCH", # consortium school
                                   "HPGGEN10CYR1AVG",
                                   "HPGGEN10CYR2AVG",
                                   "HPGGEN10CYR3AVG",
                                   "HPGGENREGREMAVG",
                                   "HPGENVSCORE",
                                   "HPGACAAVG",
                                   "HPGATTAVG",
                                   "HPGCOMAVG",
                                   "HPGENGAVG",
                                   "HPGSAFAVG",
                                   "HPGPERDIP6AVG",
                                   "HPGPERDIP4AVG",
                                   "HPGPEERINDX",
                                   "HPGPRGSCOREW",
                                   "HPGPERSCOREW",
                                   "HPGENVSCOREW",
                                   "HPGSCORE",
                                   "LCGOPNDAT", # date opened
                                   "LCGSCHCAT",
                                   "LCGSCHTYP",
                                   "LCGSTAT", # open/closed
                                   "DISTRICT",
                                   "SPECHS",
                                   #"SCHTYPE",
                                   #"SCHLOGRD",
                                   "SCHGRDCONFIG",
                                   "PPEXPTOTLTY100", # teachers
                                   "PPEXPTOTLSV750", # central adm
                                   "PPEXPPERS", # personel
                                   "PPEXPTOTL", # total
                                   "NCESSCH",
                                   "BOROUGH", 
                                   "YEAR"))

#==============================================================
# Define sample of high schools
#==============================================================

# select high schools that were open and offered general academic content in all the relevant years
# (assigned year values refer to the Spring semester so that it matches the data.)
dat_16 <- schools %>%
  select(YEAR,
         LCGSTAT,
         LCGSCHTYP,
         LCGSCHCAT,
         BEDSCODE) %>%
  filter(YEAR == 2015, # as of June
         LCGSTAT == "OPEN",
         LCGSCHTYP == "GENERAL ACADEMIC",
         LCGSCHCAT == "HIGH SCHOOL" | LCGSCHCAT == "K-12 ALL GRADES")
         
dat_15 <- schools %>%
  select(YEAR,
         LCGSTAT,
         LCGSCHTYP,
         LCGSCHCAT,
         BEDSCODE) %>%
  filter(YEAR == 2014, # as of June
         LCGSTAT == "OPEN",
         LCGSCHTYP == "GENERAL ACADEMIC",
         LCGSCHCAT == "HIGH SCHOOL" | LCGSCHCAT == "K-12 ALL GRADES")

dat_14 <- schools %>%
  select(YEAR,
         LCGSTAT,
         LCGSCHTYP,
         LCGSCHCAT,
         BEDSCODE) %>%
  filter(YEAR == 2013, # as of June
         LCGSTAT == "OPEN",
         LCGSCHTYP == "GENERAL ACADEMIC",
         LCGSCHCAT == "HIGH SCHOOL" | LCGSCHCAT == "K-12 ALL GRADES")

dat_13 <- schools %>%
  select(YEAR,
         LCGSTAT,
         LCGSCHTYP,
         LCGSCHCAT,
         BEDSCODE) %>%
  filter(YEAR == 2012, # as of June
         LCGSTAT == "OPEN",
         LCGSCHTYP == "GENERAL ACADEMIC",
         LCGSCHCAT == "HIGH SCHOOL" | LCGSCHCAT == "K-12 ALL GRADES")

dat_12 <- schools %>%
  select(YEAR,
         LCGSTAT,
         LCGSCHTYP,
         LCGSCHCAT,
         BEDSCODE) %>%
  filter(YEAR == 2011, # as of June
         LCGSTAT == "OPEN",
         LCGSCHTYP == "GENERAL ACADEMIC",
         LCGSCHCAT == "HIGH SCHOOL" | LCGSCHCAT == "K-12 ALL GRADES")

# check number of high schools in each of the relevant years
paste0("number of high schools meeting our criteria in 2015-16 = ", length(unique(dat_16$BEDSCODE)))
paste0("number of high schools meeting our criteria in 2014-15 = ", length(unique(dat_15$BEDSCODE)))
paste0("number of high schools meeting our criteria in 2013-14 = ", length(unique(dat_14$BEDSCODE)))
paste0("number of high schools meeting our criteria in 2012-13 = ", length(unique(dat_13$BEDSCODE)))
paste0("number of high schools meeting our criteria in 2011-12 = ", length(unique(dat_12$BEDSCODE)))


# merge files
ids <- inner_join(dat_12, dat_13, by = "BEDSCODE")
ids <- inner_join(ids, dat_14, by = "BEDSCODE")
ids <- inner_join(ids, dat_15, by = "BEDSCODE")
ids <- inner_join(ids, dat_16, by = "BEDSCODE")
ids <- ids %>%
  select(BEDSCODE)

# number of schools which meet the conditions in all the relevant years.
paste0("number of high schools meeting our criteria across all relevant academic years = ", length(unique(ids$BEDSCODE)))

sink()
#==============================================================
# Select school-level variables (2011 cohort)
#==============================================================

# select relevant year
dat_12 <- schools %>%
  filter(YEAR == 2011)

# check missing data
vars <- dat_12 %>%
  select(ENRNUMG09, ENRNUMG10, ENRNUMG11, ENRNUMG12)
pct_missing_variable(vars) #--->no missing data

# compute HS size
dat_12 <- dat_12 %>%
  mutate(SCH_hs_size = ENRNUMG09 + ENRNUMG10 + ENRNUMG11 + ENRNUMG12)

# rename vars
dat_12 <- dat_12 %>%
  rename(ID_district_name = DISTRICT,        
         ID_geo_district = GEOID,
         ID_NCESSCH = NCESSCH,
         ID_school_name = ATTSCHNAM,
         SCH_total_enrollment = ENRNUMTOT,
         SCH_pct_ell = DMGPCTELLTOT,
         SCH_class_size_09g = CLSAVGG09,
         SCH_class_size_10g = CLSAVGG10,
         SCH_class_size_11g = CLSAVGG11,
         SCH_class_size_12g = CLSAVGG12,
         SCH_community_based_org = CBO,                 
         SCH_consortium = CTMSCH,                  
         SCH_HS_acad_expect_avg = HPGACAAVG,                
         SCH_HS_att_rate_avg = HPGATTAVG,              
         SCH_HS_communication_avg = HPGCOMAVG,                
         SCH_HS_engagement_avg = HPGENGAVG,               
         SCH_HS_safety_avg = HPGSAFAVG,
         SCH_HS_peer_index = HPGPEERINDX,
         SCH_HS_progress_score = HPGPRGSCOREW,
         SCH_HS_performance_score = HPGPERSCOREW,
         SCH_HS_environment_score = HPGENVSCOREW,
         SCH_HS_total_score = HPGSCORE,
         SCH_HS_6year_grad_rate = HPGPERDIP6AVG,   
         SCH_HS_4year_grad_rate = HPGPERDIP4AVG,           
         SCH_spec_HS = SPECHS,         
         SCH_exp_pp_instruction = PPEXPTOTLTY100,          
         SCH_exp_pp_adm = PPEXPTOTLSV750,           
         SCH_exp_pp_personal = PPEXPPERS,               
         SCH_exp_pp_total = PPEXPTOTL)

# keep only new variable names
dat_12 <- dat_12 %>%
  select(starts_with("ID_"),
         starts_with("SCH_"),
         BEDSCODE)

# I will temporarily assign the suffix "2011-cohort" to 2011 variables so that they are differentiated from the 2012 variables when merged
colnames(dat_12) <- paste(colnames(dat_12), "2011-cohort", sep = "_")

#==============================================================
# Select school-level variables (2012 cohort)
#==============================================================

# select relevant year
dat_13 <- schools %>%
  filter(YEAR == 2012)

# check missing data
vars <- dat_13 %>%
  select(ENRNUMG09, ENRNUMG10, ENRNUMG11, ENRNUMG12)
pct_missing_variable(vars) #---> no missing data

# compute high school size
dat_13 <- dat_13 %>%
  mutate(SCH_hs_size = ENRNUMG09 + ENRNUMG10 + ENRNUMG11 + ENRNUMG12)

# rename vars
dat_13 <- dat_13 %>%
  rename(ID_district_name = DISTRICT,        
         ID_geo_district = GEOID,
         ID_NCESSCH = NCESSCH,
         ID_school_name = ATTSCHNAM,
         SCH_total_enrollment = ENRNUMTOT,
         SCH_pct_ell = DMGPCTELLTOT,
         SCH_class_size_09g = CLSAVGG09,
         SCH_class_size_10g = CLSAVGG10,
         SCH_class_size_11g = CLSAVGG11,
         SCH_class_size_12g = CLSAVGG12,
         SCH_community_based_org = CBO,                 
         SCH_consortium = CTMSCH,                  
         SCH_HS_acad_expect_avg = HPGACAAVG,                
         SCH_HS_att_rate_avg = HPGATTAVG,              
         SCH_HS_communication_avg = HPGCOMAVG,                
         SCH_HS_engagement_avg = HPGENGAVG,               
         SCH_HS_safety_avg = HPGSAFAVG,
         SCH_HS_peer_index = HPGPEERINDX,
         SCH_HS_progress_score = HPGPRGSCOREW,
         SCH_HS_performance_score = HPGPERSCOREW,
         SCH_HS_environment_score = HPGENVSCOREW,
         SCH_HS_total_score = HPGSCORE,
         SCH_HS_6year_grad_rate = HPGPERDIP6AVG,   
         SCH_HS_4year_grad_rate = HPGPERDIP4AVG,           
         SCH_spec_HS = SPECHS,         
         SCH_exp_pp_instruction = PPEXPTOTLTY100,          
         SCH_exp_pp_adm = PPEXPTOTLSV750,           
         SCH_exp_pp_personal = PPEXPPERS,               
         SCH_exp_pp_total = PPEXPTOTL)

# keep only new variable names
dat_13 <- dat_13 %>%
  select(starts_with("ID_"),
         starts_with("SCH_"),
         BEDSCODE)

#==============================================================
# Create a single file for both cohorts
#==============================================================

# only select schools which meet our definition of "high school" in all the relevant years
dat_schools <- inner_join(ids, dat_12, by = c("BEDSCODE" = "BEDSCODE_2011-cohort"))
dat_schools <- inner_join(dat_schools, dat_13, by = "BEDSCODE")

# describe missing data
paste0("missing data in the merged dataset")
print(pct_missing_variable(dat_schools), n = Inf)

# remove selected variables (high missing rates)
dat_schools <- dat_schools %>%
  select(-starts_with("SCH_class_size_11"),
         -starts_with("SCH_class_size_12"),
         -starts_with("SCH_HS_6year"))

# delete columns with more than 10% missing
dat_schools <- dat_schools[which(rowMeans(is.na(dat_schools)) < 0.1), ]

# delete columns full of zeros
col_zeros <- apply(dat_schools, 2, function(row) all(row == 0))
dat_schools <- dat_schools %>%
  select(-starts_with("SCH_community"))

# which variables have NAs?
paste0("Variables in the merged data with NAs")
vars <- dat_schools %>%
  select_if(vars_NA) %>%
  names()
vars

# input NAs with the variable means
impute_vars = vars
dat_schools <- as.data.frame(dat_schools)
for(i in impute_vars) {
  dat_schools[is.na(dat_schools[,i]), i] <- mean(dat_schools[,i], na.rm = TRUE)
}
# transform back to tibble
dat_schools <- as_tibble(dat_schools)

#==============================================================
# Merge RANYCS master file with DOE data
#==============================================================

# additional school IDs are needed to match with the DOE data. 
# to see more about NYC DOE's IDs, check https://www.twosigma.com/wp-content/uploads/opendataweek.pdf.

# How to navigate across IDs:
#  Last 5 digits of CCD NCESSCH = last 5 digits CRDC combokey
#  CCD SEASCH = NYC open data's BEDS number
#  NYC open data's ATS SYSTEM CODE = DBN from RANYCS data
#  CCD NCESSCH = SEDA ncessch

# load NYC open data
options(scipen=999)
open_dat <- read.csv(NYC_open_data)

# get BEDS number from the RANYCS file
dat_schools <- dat_schools %>%
  mutate(BEDSCODE = as.numeric(BEDSCODE))

# get ATS SYSTETEM code from the nyc open data. 
# it should be equal to RANYCS DBN
open_dat <- open_dat %>%
  rename(DBN = ATS.SYSTEM.CODE) %>%
  select(BEDS.NUMBER,
         DBN)

# check matching IDs
x <- intersect(open_dat$BEDS.NUMBER, dat_schools$BEDSCODE)
paste0("pct of rows with matching IDs ", 100 * length(x) / nrow(dat_schools)) 

# merge datasets based on the BEDS number
dat_schools <- inner_join(dat_schools, open_dat, by = c("BEDSCODE" = "BEDS.NUMBER"))

# note that the DBN code has an extra space. I am removing it (can be useful later)
dat_schools <- dat_schools %>%
  mutate(DBN = gsub(" ", "", DBN, fixed = TRUE))

# remove the "2011-cohort" suffix from the 2011 variables
dat_schools_12 <- dat_schools %>%
  select(ends_with("2011-cohort"),
         DBN)
for(col in 1:ncol(dat_schools_12)){
  colnames(dat_schools_12)[col] <-  sub("_2011-cohort", "", colnames(dat_schools_12)[col])
}

# remove all the 2011 variables from the 2012 data.
dat_schools_13 <- dat_schools %>%
  select(-ends_with("2011-cohort"),
         DBN) # keep DBN

# save
save(dat_schools_12, file="../data/dataset_1_cohort-11-12.Rdata")
save(dat_schools_13, file="../data/dataset_1_cohort-12-13.Rdata")

#==============================================================
# Summary table for 2011
#==============================================================
# delete character variables when making the table
chr <- dat_schools_12 %>%
  select(where(is.character)) %>%
  names()
vars <- dat_schools %>%
  select(-all_of(c(chr)),
         -starts_with("ID_"))

# make summary table
dat_table <- summary_of_data(vars)
print(dat_table, n = Inf)

# if we want to provide it in latex
#x = kbl(dat_table, "latex", booktabs = T) %>%
#column_spec(1, width = "10em") 

#==============================================================
# Summary table for 2012
#==============================================================
# delete character variables when making the table
chr <- dat_schools_13 %>%
  select(where(is.character)) %>%
  names()
vars <- dat_schools %>%
  select(-all_of(c(chr)),
         -starts_with("ID_"))

# make summary table
dat_table <- summary_of_data(vars)
print(dat_table, n = Inf)

# if we want to provide it in latex
#x = kbl(dat_table, "latex", booktabs = T) %>%
#column_spec(1, width = "10em") 
