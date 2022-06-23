# ******************************************************************************
# Program: 			FPmain.R
# Purpose: 			Main file for the Family Planning Chapter. 
#               The main file will call other R scripts that will produce the FP indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Courtney Allen 
# Date last modified:	Sept 29, 2021 by Shireen Assaf
# 
# Notes:				Indicators for men only cover knowledge of contraceptive methods and exposure to family planning messages.
# Indicators are coded for all women/all men unless indicated otherwise. 
# In the tables do file you can select other populations of interest (ex: among those currently in a union)
# ******************************************************************************

# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labelled DHS variables
library(labelled)   # used for Haven labelled variable creation
library(expss)    # for creating tables with Haven labelled data
library(xlsx)     # for exporting to excel
library(here)       # to get R project path

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap07_FP"

# *******************************************************************************************************************************

## select your survey

# IR Files
IRdatafile <- "UGIR7BFL.dta"
# example of file names: MMIR71FL TJIR70FL GHIR72FL UGIR7BFL

# MR Files
MRdatafile <- "UGMR7BFL.dta"
# example of file names: MMR71FL TJMR70FL GHMR72FL UGMR7BFL
# *******************************************************************************************************************************

## IR file variables

# open database
IRdata <-  read_dta(here(chap,IRdatafile))
# create indicator of file type (e.g. "IR", "MR", etc)
file <- "IR" 

## do separate R scripts for each subtopic

# script for contraceptive knowledge variables
source(here(paste0(chap,"/FP_KNOW.R")))

# script for contraceptive use variables (ever use and current use)
source(here(paste0(chap,"/FP_USE.R")))

# script for contraceptive unmet need, met need, demand satisfied, intention to use
source(here(paste0(chap,"/FP_NEED.R")))

# script for communication related indicators: exposure to FP messages, decision on use/nonuse, discussions. 
source(here(paste0(chap,"/FP_COMM.R")))

# script to produce tables for indicators computed from above R scripts.
source(here(paste0(chap,"/FP_tables.R")))

# script to create an event file where the episode of contraceptive use is the unit of analysis.
source(here(paste0(chap,"/FP_EVENTS.R")))

# script for discontinuation variables (discontinuation rates and reasons for discontinuation) and create discontinuation tables
source(here(paste0(chap,"/FP_DISCONT.R")))

# note: This script will create the discontinuation results table Tables_Discont_12m.xlsx and an .RDS file, eventsfile.rds for the survey. 

# *******************************************************************************************************************************
## MR file variables

# open dataset
MRdata <-   read_dta(here(chap,MRdatafile))
# create indicator of file type (e.g. "IR", "MR", etc)
file <- "MR" 

# script for contraceptive knowledge variables
source(here(paste0(chap,"/FP_KNOW.R")))

# script for communication related indicators: exposure to FP messages indicators only for men. 
source(here(paste0(chap,"/FP_COMM.R")))

# script to produce tables for indicators computed from above R scripts. 
source(here(paste0(chap,"/FP_tables.R")))

# *******************************************************************************************************************************
