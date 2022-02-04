# /*******************************************************************************************************************************
# Program: 				NTmain.R
# Purpose: 				Main file for the Nutrition Chapter. 
# 						    The main file will call other do files that will produce the NT indicators and produce tables.
# Data outputs:		coded variables and table output in excel tables.  
# Author: 				Shireen Assaf 
# Date last modified:		February 4, 2021
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

#path for R project
here()

# path for this chapter. This is also where the data is stored
chap <- "Chap11_NT"

# select your survey

# KR Files
KRdatafile <- "RWKR81FL.dta"

# PR Files
PRdatafile <- "RWPR81FL.dta"

# IR Files
IRdatafile <- "RWIR81FL.dta"

# MR Files
MRdatafile <- "RWMR81FL.dta"

# HR Files
HRdatafile <- "RWHR81FL.dta"

# ****************************

# KR file variables

# Note: For the NT_MICRO do file, you need to merge the KR with the HR file. 
# The merge will be performed before running any of the do files below. 
#open HR file
HRdata <-  read_dta(here(chap,HRdatafile))
# rename IDs
HRdata <- HRdata %>%
  mutate(v001=hv001) %>%
  mutate(v002=hv002) 
# keep relevant vars
HRtemp =subset(HRdata, select=c(v001, v002, hv234a))
# * open IR dataset
KRdata <-  read_dta(here(chap,KRdatafile))
#perform merge
KRdata <- merge(KRdata,HRtemp,by=c("v001", "v002"))
rm(HRtemp)

# Calculate age of child. If b19 is not available in the data use v008 - b3
if ("TRUE" %in% (!("b19" %in% names(KRdata))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  KRdata <- KRdata %>%
    mutate(age = b19)
} else {
  KRdata <- KRdata %>%
    mutate(age = v008 - b3)
}


source(here(paste0(chap,"/NT_CH_MICRO.R")))
# Purpose: 	Code micronutrient indicators
 
source(here(paste0(chap,"/NT_BF_INIT.R")))
# Purpose:   Code initial breastfeeding indicators

#source(here(paste0(chap,"/NT_BF_MED.do"))) 
# Purpose: 	Code breastfeeding indicators

#############
# Note: The following files select for the youngest child under 2 years living with the mother. 
# Therefore a subset of the KR file (KRiycf)  will be produced to select for these children. 
# Open KR file - if not open from code above
# KRdata <-  read_dta(here(chap,KRdatafile))


#create subset of KRfile to select for children for IYCF indicators
KRiycf <- KRdata %>%
  subset(age < 24 & b9==0) %>% # children under 24 months living at home
  arrange(caseid, bidx) %>% # make sure the data is sorted
  subset(is.na(lag(caseid)) | caseid!=lag(caseid)) # select just the youngest

source(here(paste0(chap,"/NT_IYCF.R")))
# Purpose: 			Code to compute infant and child feeding indicators
# 
source(here(paste0(chap,"/NT_tables_KR.R")))
# Purpose: 	Produce tables for indicators computed from KR file. This includes the KRiycf data. 

# *******************************************************************************************************************************
# *******************************************************************************************************************************
# PR file variables

# open dataset
PRdata <-  read_dta(here(chap,PRdatafile))
 
source(here(paste0(chap,"/NT_CH_NUT.R")))
# Purpose: 	Code child's anthropometry indicators

source(here(paste0(chap,"/NT_tables_PR.R")))
# Purpose: 	Produce tables for indicators computed from the PR file 

# *******************************************************************************************************************************
# *******************************************************************************************************************************
# HR file variables
 
# open dataset
HRdata <-  read_dta(here(chap,HRdatafile))

source(here(paste0(chap,"/NT_SALT.R")))
# Purpose: 	Code salt indicators

source(here(paste0(chap,"/NT_tables_HR.R")))
# Purpose: 	Produce tables for indicators computed from HR file. 
# *******************************************************************************************************************************
# *******************************************************************************************************************************
# IR file variables

# A merge with the HR file is required to compute one of the indicators. 
# open HR file
HRdata <-  read_dta(here(chap,HRdatafile))
# rename IDs
HRdata <- HRdata %>%
  mutate(v001=hv001) %>%
  mutate(v002=hv002) 
# keep relevant vars
HRtemp =subset(HRdata, select=c(v001, v002, hv234a))
# * open IR dataset
IRdata <-  read_dta(here(chap,IRdatafile))
#perform merge
IRdata <- merge(IRdata,HRtemp,by=c("v001", "v002"))
rm(HRtemp)

source(here(paste0(chap,"/NT_WM_NUT.R")))
# Purpose: 	Code women's anthropometric indicators

source(here(paste0(chap,"/NT_tables_adults.R")))
# Purpose: 	Produce tables for indicators computed from IR file. 
# Note:		  The indicators are filtered for age 15-49. This can be changed if required for all women/men. 

# *******************************************************************************************************************************
# *******************************************************************************************************************************
# MR file variables

# A merge with the PR file is required to compute the indicators below.
# open PR file
PRdata <-  read_dta(here(chap,PRdatafile))
# rename IDs
PRdata <- PRdata %>%
  mutate(mv001=hv001) %>%
  mutate(mv002=hv002) %>%
  mutate(mv003=hvidx) 
# keep relevant vars
PRtemp =subset(PRdata, select=c(mv001, mv002, mv003, hv042, hb55, hb56, hb57, hb40, hv103))

#open MR dataset
MRdata <-  read_dta(here(chap,MRdatafile))
#perform merge
MRdata <- merge(MRdata,PRtemp,by=c("mv001", "mv002", "mv003"))
rm(PRtemp)

source(here(paste0(chap,"/NT_MN_NUT.R")))
# Purpose: 	Code men's anthropometric indicators

source(here(paste0(chap,"/NT_tables_adults.R")))
# Purpose: 	Produce tables for indicators computed from MR file. 
# Note:	 The indicators are filtered for age 15-49. This can be changed if required for all women/men. 

# *******************************************************************************************************************************
# *******************************************************************************************************************************