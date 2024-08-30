# /*******************************************************************************************************************************
# Program: 				MSmain.R - DHS8 update
# Purpose: 				Main file for the Marriage and Sexual Activity Chapter. 
# The main file will call other source files that will produce the MS indicators and produce tables.
# Data outputs:		coded variables and table output on screen and in excel tables.  
# Author: 			        Courtney Allen
# Translated to R:      Courtney Allen and Ali Roghani 
# Date last modified:   August 2024 by Courtney Allen		

# *******************************************************************************************************************************/
  
rm(list = ls(all = TRUE))


library(tidyverse)  # most variable creation here uses tidyverse 
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)      # for creating tables with Haven labeled data
library(openxlsx)   # for exporting to excel
library(naniar)     # to use replace_with_na function
library(here)       # to get R project path
library(survey)     # survey weight data to find median ages

#path for R project
here()

# path for this chapter. This is also where the data is stored
chap <- "Chap04_MS/DHS8"

# select your survey

# IR Files
IRdatafile <- "GHIR8AFL.dta"


# MR Files
MRdatafile <- "GHMR8AFL.dta"

# ****************************


# open dataset (women's)
IRdata <-  read_dta(here(chap,IRdatafile))

# open dataset (men's)
MRdata <-  read_dta(here(chap,MRdatafile))



# do separate R scripts for each subtopic
source(here(paste0(chap,"/MS_MAR.R")))
#Purpose: 	Code marital status variables for men and women

source(here(paste0(chap,"/MS_SEX.R")))
#Purpose: 	Code sexual activity variables for men and women

source(here(paste0(chap,"/MS_tables_WM.R")))
#Purpose: 	Produce tables for indicators for women computed above. 

source(here(paste0(chap,"/MS_tables_MN.R")))
#Purpose: 	Produce tables for indicators for men computed above. 


