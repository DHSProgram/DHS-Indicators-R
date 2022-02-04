# /*******************************************************************************************************************************
# Program: 			MLmain.R
# Purpose: 			Main file for Malaria Chapter.  
#               The main file will call other R files that will produce the ML indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Shireen Assaf - translated to R by Mahmoud Elkasabi 
# Date last modified:	January 12, 2022 by Mahmoud Elkasabi
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(haven)
library(naniar) #to replace values with NA
library(dplyr)
library(sjlabelled)
library(expss)
library(xlsx)
library(here)       # to get R project path
library(stringr)   # for the rename (str_replace_all) function

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap12_ML"

#####################################################################################
## Define datasets

# HR Files
HRdatafile <- "MWHR7IFL.dta"

# PR Files
PRdatafile <- "MWPR7IFL.dta"

# IR Files
IRdatafile <- "MWIR7IFL.DTA"

# KR Files
KRdatafile <- "MWKR7IFL.dta"

# HR file variables ############################################################

# open HR dataset
HRdata <-  read_dta(here(chap,HRdatafile))

# Purpose: Code household net indicators
source(here(paste0(chap,"/ML_NETS_HH.R")))

# Purpose: will produce the tables for ML_NETS_HH.do file indicators
source(here(paste0(chap,"/ML_tables_HR.R")))

# Purpose: Code indicators for Use of existing ITNs
source(here(paste0(chap,"/ML_EXISTING_ITN.R"))) 

# Purpose: code source of mosquito net
source(here(paste0(chap,"/ML_NETS_source.R"))) 

# PR file variables ############################################################

# open PR dataset
PRdata <-  read_dta(here(chap,PRdatafile))

# Purpose: Code net use in population
source(here(paste0(chap,"/ML_NETS_use.R")))

# Purpose: Code anemia and malaria testing prevalence in children under 5
source(here(paste0(chap,"/ML_BIOMARKERS.R")))

# Purpose: produce the tables for indicators produced from the above two do files.
source(here(paste0(chap,"/ML_tables_PR.R")))

# Purpose: code population access to ITN
# open HR dataset
HRdata <-  read_dta(here(chap,HRdatafile))
source(here(paste0(chap,"/ML_NETS_access.R")))


# IR file variables ############################################################

# open IR dataset
IRdata <-  read_dta(here(chap,IRdatafile))

# Purpose: Code malaria IPTP indicators
source(here(paste0(chap,"/ML_IPTP.R")))

# Purpose: produce the tables for indicators produced from the above do file
source(here(paste0(chap,"/ML_tables_IR.R")))

# KR file variables ############################################################

# open KR dataset
KRdata <-  read_dta(here(chap,KRdatafile))

# Purpose: Code indicators on fever, fever care-seeking, and antimalarial drugs
source(here(paste0(chap,"/ML_FEVER.R")))

# Purpose: produce the tables for indicators produced from the above do file
source(here(paste0(chap,"/ML_tables_KR.R")))
