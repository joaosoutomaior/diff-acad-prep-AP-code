Code for replication of the paper *Differences in academic preparedness do not fully explain Black-White enrollment disparities in advanced high school coursework* by Joao Souto-Maior and Ravi Shroff.

The file "0_run-all.R" runs the full codebase. 

The "output" folder provides the intermediate data products to construct all figures in the paper.

## Data and code availability statement

The administrative dataset used in this paper was provided to the authors under a Data Use Agreement with the NYC Department of Education (NYC DOE) through the Research Alliance for New York City Schools (RANYCS). Due to the nature of our agreement, we are not at liberty to make the data public. These data can be obtained upon request via the research partnership, pending scientific review and a completed material transfer agreement. Requests for the data should be submitted directly to RANYCS. Their contact information is available at https://steinhardt.nyu.edu/research-alliance/contact-us. The code to reproduce all results in the paper is publicly available at https://github.com/joaosoutomaior/diff-acad-prep-AP-code. The code is organized so that anyone with access to the restricted administrative data could reproduce all results.

## Datasets 

Following our agreement with RANYCS, due to the sensitivity of these data, we are unable to disclose exact server paths. These are loaded from private R scripts not included in this repository. That said, they are described (below) in sufficient detail so that those with access to the server can identify exact paths. If you have access to the server and have questions about the exact paths, please reach out to the corresponding author.

Enrollment data from step 2 of data cleaning for all years between 2010 and 2017.

* DOE_dataset_enr_10
* DOE_dataset_enr_11
* DOE_dataset_enr_12
* DOE_dataset_enr_13 
* DOE_dataset_enr_14 
* DOE_dataset_enr_15
* DOE_dataset_enr_16 
* DOE_dataset_enr_17

Biographical data from step 1 of data cleaning for 2011 and 2012.

* DOE_dataset_bio_11
* DOE_dataset_bio_12 

Exam data from step 2 of data cleaning for all years between 2012 and 2016.

* DOE_dataset_exams_12 
* DOE_dataset_exams_13 
* DOE_dataset_exams_14 
* DOE_dataset_exams_15 
* DOE_dataset_exams_16 

Course data from raw files for all years between 2012 and 2016.

* DOE_dataset_courses_12 
* DOE_dataset_courses_13 
* DOE_dataset_courses_14 
* DOE_dataset_courses_15 
* DOE_dataset_courses_16 

High school credits data from step 2 of data cleaning for all years between 2012 and 2016.

* DOE_dataset_HScredits_12 
* DOE_dataset_HScredits_13 
* DOE_dataset_HScredits_14 
* DOE_dataset_HScredits_15 
* DOE_dataset_HScredits_16 

Middle school credits data from step 2 of data cleaning for all years between 2010 and 2012

* DOE_dataset_MScredits_10
* DOE_dataset_MScredits_11 
* DOE_dataset_MScredits_12 

Middle school state exams data from step 2 of data cleaning for all years between 2010 and 2012.

* DOE_dataset_MSstateexams_10 
* DOE_dataset_MSstateexams_11
* DOE_dataset_MSstateexams_12 

Suspensions data from step 2 of data cleaning for all years between 2010 and 2015.

* DOE_dataset_susp_10 
* DOE_dataset_susp_11 
* DOE_dataset_susp_12 
* DOE_dataset_susp_13 
* DOE_dataset_susp_14 
* DOE_dataset_susp_15 

Attendance data from step 2 of data cleaning for all years between 2010 and 2016.

* DOE_dataset_att_10
* DOE_dataset_att_11
* DOE_dataset_att_12 
* DOE_dataset_att_13
* DOE_dataset_att_14 
* DOE_dataset_att_15
* DOE_dataset_att_16

In addition to the DOE administrative datasets, the paper uses two additional datasets. 

First, the RANYCS school-level master file (SCHMA). A data usage agreement needs to be signed in order to access it. Instructions for how to gain access are available at https://steinhardt.nyu.edu/research-alliance/research/school-level-master-file. 

Second, a publicly available dataset of NYC schools to walk across IDs, titled "2015 - 2016 School Locations". This file can be downloaded from https://data.cityofnewyork.us/Education/2015-2016-School-Locations/i4ni-6qin.

## Intermediate data products for figures

* Figure 1.A --> output/enrollment-patterns_full-sample.txt
* Figure 1.B ---> unable to provide (small cells). See file 4_sample-description.R
* Figure 2 ---> output/acad-prep-dist.txt
* Figure 3 ---> output/disparate-impact-models.txt (without bootstrap standard errors) + output\bootstrap-standard-erros.txt
* Figure 4 ---> output/sensitivity-estimates.txt
* Figure S.B.1 (online supplement) --> output/racial-composition_full-sample.txt
* Figure S.C.1 (online supplement) --> unable to provide (small cells). See file 5_courses-exams-distributions.R
* Figure S.C.2 (online supplement) --> unable to provide (small cells). See file 5_courses-exams-distributions.R
* Figure S.E.1 (online supplement) --> output/model-checks.txt
* Figure S.E.2 (online supplement) --> output/model-checks.txt

## Intermediate data products for tables

* Table 1 --> output/train-test-samples.txt
* Table S.B.1 (online supplement) --> output/student-level-filter_cohort-12-13.txt
* Table S.B.2 (online supplement) --> output/assess-sample-restrictions(...).txt for all grades and years. Same years/grades are omitted due to small cells.
* Table S.E.3 (online supplement) --> output/assess-sample-restrictions(...).txt for all grades and years. Same years/grades are omitted due to small cells.
* Table S.E.1 (online supplement) --> output/covariates.txt
* Table S.E.2 (online supplement) --> output/disparate-impact-models.txt (without bootstrap standard errors) + output\bootstrap-standard-erros.txt



