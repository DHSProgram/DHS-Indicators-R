# /*******************************************************************************************************************************
# Program: 			CMmain.R
# Purpose: 			Main file for the Child Mortality Chapter.  
#               The main file will call other R files that will produce the CM indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	September 24, 2021 by Mahmoud Elkasabi
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

# Set the current file location as the default working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

# path where data files are stored 
datapath <- "./Data/DHSdata"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "UGIR7BFL.DTA"

# BR Files
BRdatafile <- "UGBR7BFL.dta"

# KR Files
KRdatafile <- "UGKR7BFL.dta"

#####################################################################################
BRdata <-  read_dta(file.path(datapath,BRdatafile))
IRdata <-  read_dta(file.path(datapath,IRdatafile))
KRdata <-  read_dta(file.path(datapath,KRdatafile))

# Code child mortality indicators
#Purpose: 	Code child mortality indicators
source("CM_CHILD.R", echo=TRUE)

#Purpose: 	Code stillbirths, early neonatal deaths, and perinatal mortality rate
#This do file uses IR and BR file
source("CM_PMR.R", echo=TRUE)

#Purpose: 	Produce tables for indicators computed from above do files. 
source("CM_tables1.R", echo=TRUE)

#Purpose: 	Code high risk fertility indicators among women
#WARNING: This do file will drop women that are not currently married
source("CM_RISK_wm.R", echo=TRUE)

#Purpose: 	Code high risk birth indicators and risk ratios
source("CM_RISK_births.R", echo=TRUE)

#Purpose: 	Produce tables for high risk births.
source("CM_tables2.R", echo=TRUE)
