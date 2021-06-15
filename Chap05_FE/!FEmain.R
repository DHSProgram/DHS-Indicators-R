# /*******************************************************************************************************************************
# Program: 			FEmain.R
# Purpose: 			Main file for the Fertility Chapter. 
#               The main file will call other R files that will produce the FE indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	March 11, 2021 by Mahmoud Elkasabi
# *******************************************************************************************************************************/

rm(list = ls(all = TRUE))

library(haven)
library(data.table)
library(dplyr)
library(xlsx)
library(survey)
library(sjlabelled)
library(expss)
library(matrixStats)
library(DHS.rates)

# Set the current file location as the default working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

## select your survey ================= define datasets =============================

# IR Files
IRdatafile <- "UGIR7BFL.DTA"

# PR Files
PRdatafile <- "UGPR7BFL.dta"

# BR Files
BRdatafile <- "UGBR7BFL.DTA"

# KR Files
KRdatafile <- "UGKR7BFL.dta"

# open required datasets
IRdata <-  read_dta(IRdatafile)
PRdata <-  read_dta(PRdatafile)
BRdata <-  read_dta(BRdatafile)
KRdata <-  read_dta(KRdatafile)


################################################################################################

## do separate R scripts for each subtopic

# Fertility rates
source("FE_TFR.R", echo=TRUE)

# Current Fertility indicators
source("FE_FERT.R", echo=TRUE)


# Crude Birth Rates
source("FE_CBR.R", echo=TRUE)


# Birth intervals indicators
source("FE_INT.R", echo=TRUE)

# Postpartum amenorrhoea, abstinence, and insusceptibility
DHS_PHASE <- 7
source("FE_MEDIANS.R", echo=TRUE)
