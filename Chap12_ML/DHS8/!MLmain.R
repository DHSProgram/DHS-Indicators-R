# /*******************************************************************************************************************************
# Program: 			MLmain.R - DHS8 update
# Purpose: 			Main file for Malaria Chapter.  
#               The main file will call other R files that will produce the ML indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Shireen Assaf - translated to R by Mahmoud Elkasabi 
# Date last modified:	August 02, 2024 by Courtney Allen
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.

library(expss)      # for creating tables with Haven labeled data
library(haven)      # used for Haven labeled DHS variables
library(here)       # to get R project path
library(labelled)   # used for Haven labeled variable creation
library(naniar)     # to use replace_with_na function
library(openxlsx)   # for exporting to excel
library(survey)     # survey weight data to find median ages
library(tidyverse)  # most variable creation here uses tidyverse library(haven)
library(stringr)    # for the rename (str_replace_all) function

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap12_ML/DHS8/"

#####################################################################################
## Define datasets

# HR Files
HRdatafile <- "GHHR8AFL.dta"

# PR Files
PRdatafile <- "GHPR8AFL.dta"

# IR Files
NRdatafile <- "GHNR8AFL.dta"

# KR Files
KRdatafile <- "GHKR8AFL.dta"

# HR file variables ############################################################

# open HR dataset
HRdata <-  read_dta(here(chap,HRdatafile))

# Purpose: Code household net indicators
source(here(paste0(chap,"/ML_NETS_HH.R")))

# Purpose: will produce the tables for ML_NETS_HH.do file indicators
source(here(paste0(chap,"/ML_tables_HR.R")))

# Purpose: code source of mosquito net
source(here(paste0(chap,"/ML_NETS_source.R"))) 

# PR file variables ############################################################

# open PR dataset
PRdata <-  read_dta(here(chap,PRdatafile))

# Purpose: code population access to ITN - tables created in script
# merge HR and PR dataset
source(here(paste0(chap,"/ML_NETS_access.R")))

# Purpose: Code net use in population
source(here(paste0(chap,"/ML_NETS_use.R")))

# Purpose: Code anemia and malaria testing prevalence in children under 5
source(here(paste0(chap,"/ML_BIOMARKERS.R")))

# Purpose: produce the tables for indicators produced from the above two do files.
source(here(paste0(chap,"/ML_tables_PR.R")))



# IR file variables ############################################################

# open IR dataset
NRdata <-  read_dta(here(chap,NRdatafile))

# Purpose: Code malaria IPTP indicators
source(here(paste0(chap,"/ML_IPTP.R")))

# Purpose: produce the tables for indicators produced from the above do file
source(here(paste0(chap,"/ML_tables_NR.R")))

# KR file variables ############################################################

# open KR dataset
KRdata <-  read_dta(here(chap,KRdatafile))

# Purpose: Code indicators on fever, fever care-seeking, and antimalarial drugs
source(here(paste0(chap,"/ML_FEVER.R")))

# Purpose: produce the tables for indicators produced from the above do file
source(here(paste0(chap,"/ML_tables_KR.R")))

