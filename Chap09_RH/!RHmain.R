# *******************************************************************************************************************************
# Program: 				!RHmain.R
# Purpose: 				Main file for the Reproductive Health Chapter. 
#						      The main file will call other do files that will produce the RH indicators and produce tables.
# Data outputs:		coded variables and table output on screen and in excel tables.
# Author: 				Shireen Assaf 
# Date last modified:		September 14, 2021 by Shireen Assaf
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
library(here)

#path for R project
here()
 
# path for this chapter
chap <- "Chap09_RH"

# select your survey

# IR Files
IRdatafile <-  "AFIR71FL.dta"
#AFIR71FL.dta GMIR81FL.dta
# BR Files
BRdatafile <- "AFBR71FL.dta"

# ****************************

# IR file variables

# open dataset
IRdata <-  read_dta(here(chap,IRdatafile))

# do separate R scripts for each subtopic

source(here::here("Chap09_RH/RH_ANC.R"))
# Purpose: 	Code ANC indicators

source(here::here("Chap09_RH/RH_PNC.R"))
# Purpose: 	Code PNC indicators for mother and newborn

source(here::here("Chap09_RH/RH_Probs.R"))
# Purpose: 	Code indicators for problems accessing health care 

#source(here::here("Chap09_RH/RH_tables.R"))
# Purpose: 	Produce tables for indicators computed from above do files. This will output 3 excel files for these indicators.
# Note:		This will drop any women not in 15-49 age range. You can change this selection. Please check the notes in the do file.

#******************************************************************************************************************************
# 
#*******************************************************************************************************************************

# BR file variables
 
# open dataset
BRdata <-  read_dta(here(chap,BRdatafile))

source(here::here("Chap09_RH/RH_DEL.R"))
# source(file.path(chap, "RH_DEL.R"))
 
#source(here::here("Chap09_RH/RH_tables.R"))
# Purpose: 	Produce tables for indicators computed from above do files. This will output 2 excel files for these indicators.
# ******************************************************************************************************************************
