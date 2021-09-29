# *******************************************************************************************************************************
# Program: 				!RHmain.R
# Purpose: 				Main file for the Reproductive Health Chapter. 
#						      The main file will call other do files that will produce the RH indicators and produce tables.
# Data outputs:		coded variables and table output in excel tables.
# Author: 				Shireen Assaf 
# Date last modified:		September 29, 2021 by Shireen Assaf
# Notes:					
# *******************************************************************************************************************************

rm(list = ls(all = TRUE))

# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)    # for creating tables with Haven labeled data
library(xlsx)     # for exporting to excel
library(naniar)   # to use replace_with_na function
library(here)       # to get R project path

#path for R project
here()
 
# path for this chapter. This is also where the data is stored
chap <- "Chap09_RH"

# select your survey

# IR Files
IRdatafile <-  "GMIR81FL.dta"
#AFIR71FL.dta GMIR81FL.dta
# BR Files
BRdatafile <- "GMBR81FL.dta"

# ****************************

# IR file variables

# open dataset
IRdata <-  read_dta(here(chap,IRdatafile))

# do separate R scripts for each subtopic
source(here(paste0(chap,"/RH_ANC.R")))
# Purpose: 	Code ANC indicators

source(here(paste0(chap,"/RH_PNC.R")))
# Purpose: 	Code PNC indicators for mother and newborn

source(here(paste0(chap,"/RH_Probs.R")))
# Purpose: 	Code indicators for problems accessing health care 

# ****************************

# BR file variables
 
# open dataset
BRdata <-  read_dta(here(chap,BRdatafile))

source(here(paste0(chap,"/RH_DEL.R")))
# Purpose: Code delivery indicators

# ****************************

source(here(paste0(chap,"/RH_tables.R")))
# Purpose: 	Produce tables for indicators computed from above do files (both from IR and BR data)

# ******************************************************************************************************************************
