# /*******************************************************************************************************************************
# Program: 				!FSmain.R
# Purpose: 				Main file for the Fistula Chapter. 
# 						    The main file will call other do files that will produce the FS indicators and produce tables.
# Data outputs:		Coded variables and table output on screen and in excel tables.  
# Author: 				Shireen Assaf
# Date last modified:		September 22, 2022 by Shireen Assaf
# Note:				This Chapter is a module and not part of the core questionnaire. 
# 						Please check if the survey you are interested in has included this module in the survey. 
# 						
# 						IMPORTANT!
# 						The variables for this chapter are not standardized and you would need to search for the variable names used in the survey of interest. 
# 						The Afghanistan 2015 survey was used in the code. 
# 						The FS_FIST.do file contains notes on how to find the correct variables according the variable labels. The same code can then be used for the survey. 
# 						
# *******************************************************************************************************************************/

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
library(sjlabelled) # to set variables label
library(survey)     # to calculate weighted ratio for GAR

#path for R project
here()

# path for this chapter. This is also where the data is stored
chap <- "Chap20_FS"

# select your survey

# IR File
IRdatafile <- "AFIR71FL.dta"

# *******************************************************************************************************************************
# *******************************************************************************************************************************
# IR file variables

# open dataset
IRdata <-  read_dta(here(chap,IRdatafile))

source(here(paste0(chap,"/FS_FIST.R"))) 
# *Purpose: 	Calculate fistula indicators among women

source(here(paste0(chap,"/FS_tables.R"))) 
# Purpose: 	Produce tables for indicators computed from the above do files.
 
# *******************************************************************************************************************************
# *******************************************************************************************************************************
