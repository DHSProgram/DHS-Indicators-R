# /*******************************************************************************************************************************
# Program: 			FFmain.R
# Purpose: 			Main file for the Fertility Preferences Chapter.  
#               The main file will call other R files that will produce the FF indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	Sept 29, 2021 by Shireen Assaf
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

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap06_FF"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "UGIR7BFL.DTA"

# MR Files
MRdatafile <- "UGMR7BFL.dta"

#####################################################################################
## Do separate R scripts for each subtopic

# Fertility Preferences for women
IRdata <-  read_dta(here(chap,IRdatafile))

# create analytic variables
source(here(paste0(chap,"/FF_PREF_WM.R")))

# run the chapter tables
source(here(paste0(chap,"/FF_tables_WM.R")))

# run the Fertility planning status table
source(here(paste0(chap,"/FF_PLAN.R")))

# run the wanted fertility rates table
source(here(paste0(chap,"/FF_WANT_TFR.R")))

# Fertility Preferences for men
MRdata <-  read_dta(here(chap,MRdatafile))

# create analytic variables
source(here(paste0(chap,"/FF_PREF_MN.R")))

# run the chapter tables
source(here(paste0(chap,"/FF_tables_MN.R")))
