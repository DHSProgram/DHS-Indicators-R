# /*******************************************************************************************************************************
# Program: 			!RCmain.R
# Purpose: 			Main file for the Respondents' Characteristics Chapter. 
#               The main file will call other R files that will produce the RC indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi
# Date last modified:	August 2024 by Courtney Allen
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(tidyverse)  # most variable creation here uses tidyverse 
library(haven)      # used for Haven labeled DHS variables
library(naniar)     # to replace values with NA
library(sjlabelled) # used for Haven labeled variable creation
library(matrixStats) # for weightedMedian function
library(expss)      # for creating tables with Haven labeled data
library(openxlsx)   # for exporting to excel
library(here)       # to get R project path


#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap03_RC/DHS8"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "GHIR8AFL.DTA"

# MR Files
MRdatafile <- "GHMR8AFL.dta"

#####################################################################################
## Do separate R scripts for each subtopic

# Respondent characteristic indicators for women
IRdata <-  read_dta(here(chap,IRdatafile))

# create analytic variables
source(here(paste0(chap,"/RC_CHAR_WM.R")))
# run the chapter tables
source(here(paste0(chap,"/RC_tables_WM.R")))

#####################################################################################

# Respondent characteristic indicators for men
MRdata <-  read_dta(here(chap,MRdatafile))

# create analytic variables
source(here(paste0(chap,"/RC_CHAR_MN.R")))
# run the chapter tables
source(here(paste0(chap,"/RC_tables_MN.R")))

#####################################################################################
