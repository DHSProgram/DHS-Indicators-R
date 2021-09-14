# /*******************************************************************************************************************************
# Program: 			FFmain.R
# Purpose: 			Main file for the Fertility Preferences Chapter.  
#               The main file will call other R files that will produce the FF indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	September 07, 2021 by Mahmoud Elkasabi
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(haven)
library(naniar) #to replace values with NA
library(dplyr)
library(sjlabelled)
library(expss)
library(xlsx)

# Set the current file location as the default working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

# path where data files are stored 
datapath <- "./Data/DHSdata"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "UGIR7BFL.DTA"

# MR Files
MRdatafile <- "UGMR7BFL.dta"

#####################################################################################
## Do separate R scripts for each subtopic

# Fertility Preferences for women
IRdata <-  read_dta(file.path(datapath,IRdatafile))

# create analytic variables
source("FF_PREF_WM.R", echo=TRUE)
# run the chapter tables
source("FF_tables_WM.R", echo=TRUE)
# run the Fertility planning status table
source("FF_PLAN.R", echo=TRUE)
# run the wanted fertility rates table
source("FF_WANT_TFR.R", echo=TRUE)


# Fertility Preferences for men
MRdata <-  read_dta(file.path(datapath,MRdatafile))

# create analytic variables
source("FF_PREF_MN.R", echo=TRUE)
# run the chapter tables
source("FF_tables_MN.R", echo=TRUE)
