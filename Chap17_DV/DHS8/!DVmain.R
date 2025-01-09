# /*****************************************************************************
# Program: 				!DVmain.R - DHS8 update
# Purpose: 				Main file for the Domestic Violence Chapter. 
# 						    The main file will call other do files that will produce the DV indicators and produce tables.
# Data outputs:		Coded variables and table output in excel tables.  
# Author: 				Shireen Assaf
# Date last modified:		September 14 2021 by Shireen Assaf
# *****************************************************************************/

rm(list = ls(all = TRUE))

# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)      # for creating tables with Haven labeled data
library(xlsx)       # for exporting to excel
library(naniar)     # to use replace_with_na function
library(here)       # to get R project path

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap17_DV/DHS8"

# select your survey

# IR Files
IRdatafile <-  "GHIR8AFL.dta"
#AFIR71FL.dta GMIR81FL.dta
 
# *****************************************************************************
# *****************************************************************************

# IR file variables

# open dataset
IRdata <-  read_dta(here(chap,IRdatafile))

# do separate R scripts for each subtopic

source(here(paste0(chap,"/DV_VIOL.R")))
# Purpose: 	Calculate violence indicators, such as age at first violence, ever experienced violence

source(here(paste0(chap,"/DV_PRTNR.R")))
# Purpose: 	Calculate violence indicators that have to do with spousal/partner violence and seeking help

source(here(paste0(chap,"/DV_CNTRL.R")))
# Purpose: 	Calculate violence indicators that have to do with spousal/partner violence and seeking help
 
source(here("Chap17_DV/DV_tables.R"))
# Purpose: 	Produce tables for indicators computed from the above do files.

# ******************************************************************************
# ******************************************************************************
