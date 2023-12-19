############################################################
#      Installing packages to the 3.6.0 R version 
############################################################
# Joao Souto-Maior, Spring 2021

###### local library
.libPaths()[1]
options(repos = "http://cran.r-project.org")

####### basic packages
install.packages("crayon",
                 lib = lib_path)
library("crayon", lib.loc = lib_path)

install.packages("dplyr",
                 lib = lib_path)
library("dplyr", lib.loc = lib_path)

install.packages("tidyr",
                 lib = lib_path)
library("tidyr", lib.loc = lib_path)

####### Haven package to load sas data (did not load automatically)
url = "https://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_1.0.1.tar.gz"
install.packages(url,
                 lib = lib_path,
                 repos = NULL, 
                 type = "source",
                 dependencies = TRUE)
library("Rcpp", lib.loc = lib_path)

url = "https://cran.r-project.org/src/contrib/Archive/BH/BH_1.69.0-1.tar.gz"
install.packages(url,
                 lib = lib_path,
                 repos = NULL, 
                 type = "source",
                 dependencies = TRUE)
library("BH", lib.loc = lib_path)

url = "https://cran.r-project.org/src/contrib/Archive/readr/readr_1.3.1.tar.gz"
install.packages(url,
                 lib = lib_path,
                 repos = NULL, 
                 type = "source",
                 dependencies = TRUE)
library("readr", lib.loc = lib_path)

url = "https://cran.r-project.org/src/contrib/Archive/haven/haven_2.1.0.tar.gz"
install.packages(url,
                 lib = lib_path,
                 repos = NULL, 
                 type = "source",
                 dependencies = TRUE)