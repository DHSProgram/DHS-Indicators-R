# /*******************************************************************************************************************************
# Program: 			RCmain.R
# Purpose: 			Main file for the Respondents' Characteristics Chapter. 
#               The main file will call other R files that will produce the RC indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	March 30, 2021 by Mahmoud Elkasabi
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(haven)
library(naniar) #to replace values with NA
library(dplyr)
library(sjlabelled)
library(matrixStats) # for weightedMedian function
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

# Respondent characteristic indicators for women
IRdata <-  read_dta(file.path(datapath,IRdatafile))

# create analytic variables
source("RC_CHAR_WM.R", echo=TRUE)
# run the chapter tables
source("RC_tables_WM.R", echo=TRUE)

# Respondent characteristic indicators for men
MRdata <-  read_dta(file.path(datapath,MRdatafile))

# create analytic variables
source("RC_CHAR_MN.R", echo=TRUE)
# run the chapter tables
source("RC_tables_MN.R", echo=TRUE)
