# /*******************************************************************************************************************************
# Program: 			CMmain.R
# Purpose: 			Main file for the Child Mortality Chapter.  
#               The main file will call other R files that will produce the CM indicators and produce tables.
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
library(DHS.rates)
library(stringr)
library(here)       # to get R project path

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap08_CM"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "UGIR7BFL.DTA"

# BR Files
BRdatafile <- "UGBR7BFL.dta"

# KR Files
KRdatafile <- "UGKR7BFL.dta"

#####################################################################################
BRdata <-  read_dta(here(chap,BRdatafile))
IRdata <-  read_dta(here(chap,IRdatafile))
KRdata <-  read_dta(here(chap,KRdatafile))

# Code child mortality indicators
#Purpose: 	Code child mortality indicators
source(here(paste0(chap,"/CM_CHILD.R")))

#Purpose: 	Code stillbirths, early neonatal deaths, and perinatal mortality rate
#This do file uses IR and BR file
source(here(paste0(chap,"/CM_PMR.R")))

#Purpose: 	Produce tables for indicators computed from above do files. 
source(here(paste0(chap,"/CM_tables1.R")))

#Purpose: 	Code high risk fertility indicators among women
#WARNING: This do file will drop women that are not currently married
source(here(paste0(chap,"/CM_RISK_wm.R")))

#Purpose: 	Code high risk birth indicators and risk ratios
source(here(paste0(chap,"/CM_RISK_births.R")))

#Purpose: 	Produce tables for high risk births.
source(here(paste0(chap,"/CM_tables2.R")))
