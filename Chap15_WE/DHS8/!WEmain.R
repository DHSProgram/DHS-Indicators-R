# /*******************************************************************************************************************************
# Program: 				WEmain.R
# Purpose: 				Main file for the Women's Empowerment Chapter. 
# 						    The main file will call other do files that will produce the WE indicators and produce tables.
# Data outputs:		Coded variables and table output on screen and in excel tables.  
# Author: 				Shireen Assaf
# Date last modified:		November 23, 2021 by Shireen Assaf
# *******************************************************************************************************************************/
 
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
chap <- "Chap15_WE"

# select your survey

# IR Files
IRdatafile <- "RWIR81FL.dta"

# MR Files
MRdatafile <- "RWMR81FL.dta"

# ****************************

# * open datasets
IRdata <-  read_dta(here(chap,IRdatafile))
MRdata <-  read_dta(here(chap,MRdatafile))

source(here(paste0(chap,"/WE_ASSETS.R")))
# *Purpose: Code employment, earnings, and asset ownership for men and women

source(here(paste0(chap,"/WE_EMPW.R")))
# *Purpose: 	Code decision making and justification of violence among men and women

source(here(paste0(chap,"/WE_tables.R")))
# *Purpose: 	Produce tables for indicators computed from the above do files.
# * Note:		This select for women and men in 15-49 age range. You can change this selection. Please check the notes in the do file.
