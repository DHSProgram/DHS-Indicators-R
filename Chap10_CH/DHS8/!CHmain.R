# /*******************************************************************************************************************************
# Program: 				  !CHmain.R
# Purpose: 				  Main file for the Child Health Chapter. 
# 						      The main file will call other syntax files that will produce the CH indicators and produce tables.
# Data outputs:			Coded variables and table output in excel tables.  
# Author: 				  Shireen Assaf	
# Date last modified:		August 17, 2022 by Shireen Assaf
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
chap <- "Chap10_CH"

# select your survey

# KR Files
KRdatafile <- "UGKR7BFL.dta"

# IR Files
IRdatafile <- "UGIR7BFL.dta"

# ****************************

# KR file variables

#open dataset
KRdata <-  read_dta(here(chap,KRdatafile))

source(here(paste0(chap,"/CH_SIZE.R")))
# Purpose: 	Code child size indicators

source(here(paste0(chap,"/CH_ARI_FV.R")))
# Purpose:	Code ARI indicators

source(here(paste0(chap,"/CH_DIAR.R")))
# Purpose:	Code diarrhea indicators

source(here(paste0(chap,"/CH_VAC.R")))
# Purpose:	Code vaccination indicators
# Note:     This code will create a data subset KRvac for children in the vaccination age group

source(here(paste0(chap,"/CH_STOOL.R")))
# Purpose:	Safe disposal of stool
# Note:     This code will create a data subset KRstool to selected for youngest child under age 2 living with the mother

# do CH_tables.do
# *Purpose: 	Produce tables for indicators computed from above do files. 


# *******************************************************************************************************************************
# *******************************************************************************************************************************
 
# IR file variables
#open dataset
IRdata <-  read_dta(here(chap,IRdatafile))

source(here(paste0(chap,"/CH_KNOW_ORS.R")))
# Purpose: 	Code knowledge of ORS
# 
# *******************************************************************************************************************************

source(here(paste0(chap,"/CH_tables.R")))
# Purpose: 	Produce tables for indicators computed from above files. 


