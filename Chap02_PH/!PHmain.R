# /*****************************************************************************
# Program: 				!PHmain.R
# Purpose: 				Main file for the Population and Housing Chapter. 
# 						    The main file will call other do files that will produce the PH indicators and produce tables.
# Data outputs:		Coded variables and table output on screen and in excel tables.  
# Author: 				Shireen Assaf
# Date last modified:		July 25, 2023 by Shireen Assaf to add srvyr library needed for PH.SCHOL file
# ******************************************************************************
rm(list = ls(all = TRUE))

# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)    # for creating tables with Haven labeled data
library(xlsx)     # for exporting to excel
library(naniar)   # to use replace_with_na function
library(here)       # to get R project path
library(sjlabelled) # to set variables label
library(survey)  # to calculate weighted ratio for GAR
library(srvyr)

#path for R project
here()

# path for this chapter. This is also where the data is stored
chap <- "Chap02_PH"

# select your survey

# HR Files
HRdatafile <- "RWHR81FL.dta"

# PR Files
PRdatafile <- "RWPR81FL.dta"

# BR Files
BRdatafile <- "RWBR81FL.dta"

# ****************************

# open datasets
HRdata <-  read_dta(here(chap,HRdatafile))
PRdata <-  read_dta(here(chap,PRdatafile))
BRdata <-  read_dta(here(chap,BRdatafile))
# ****************************

# HR file variables (use for indicators where households is the unit of measurement)
WASHdata <- HRdata #same code can be used for PR or HR files, but must be specified here

source(here(paste0(chap,"/PH_SANI.R")))
# Purpose: 	Code Sanitation indicators

source(here(paste0(chap,"/PH_WATER.R")))
# Purpose: 	Code Water Source indicators
 
HRWASHdata <- WASHdata # tables.R will refer to this dataset for tables on household characteristics

source(here(paste0(chap,"/PH_HOUS.R")))
# Purpose:	Code housing indicators such as house material, assets, cooking fuel and place, and smoking in the home

# **********************************
# PR file variables (use for indicators where the population is the unit of measurement)

WASHdata <- PRdata #same code can be used for PR or HR files, but must be specified here

source(here(paste0(chap,"/PH_SANI.R")))
# Purpose: 	Code Sanitation indicators

source(here(paste0(chap,"/PH_WATER.R")))
# Purpose: 	Code Water Source indicators

PRWASHdata <- WASHdata # tables.R will refer to this dataset for tables on household characteristics

source(here(paste0(chap,"/PH_HNDWSH.R")))
# Purpose:	Code hand-washing indicators



# For the PH_SCHOL.R below you need to update the following inputs since they are country-specific
# To produce the net attendance ratios you need to provide country specific information on the year 
# and month of the school calendar and the age range for school attendance. See lines 63-73. 
# You can obtain this information for each country from the UNESCO webiste: http://data.uis.unesco.org/. 
# This would be under "Education" and then "Other policy relevant indicators".
# Scroll to the bottom of the list to obtain the school ages from "Offical entrance age to each ISCED level of education" and the school calendar from "Start and end of the academic year".

# To calculate the child's age at the start of the school year we have to specify the month and year of the start of the school year referred to in the survey. 
# For example, for Zimbabwe 2015 survey this was January 2015
school_start_yr = 2015
school_start_mo = 1
# also we need the age ranges for primary and secondary
# for example, for Zimbabwe 2015, the age range is 6-12 for primary school and 13-18 for secondary school
age_prim_min = 6
age_prim_max = 12
age_sec_min = 13
age_sec_max = 18

source(here(paste0(chap,"/PH_SCHOL.R")))
# Purpose:	Code education and schooling indicators. 
# Note: This code will merge BR and PR files and drop some cases. It will also produce the excel file Tables_schol 
# 
source(here(paste0(chap,"/PH_POP.R")))
# Purpose: 	Code to compute population characteristics, birth registration, education levels, household composition, orphanhood, and living arrangments
# Warning: This do file will collapse the data and therefore some indicators produced will be lost. However, they are saved in the file PR_temp_children.dta and this data file will be used to produce the tables for these indicators in the PH_table code. This do file will produce the Tables_hh_comps for household composition (usually Table 2.8 or 2.9 in the Final Report). 
# Note: The code will also produce the table Tables_PH.xlsx
 
source(here(paste0(chap,"/PH_GINI.R")))
# Purpose:	Code to produce Gini index table. 
# Note: This code will collapse the data and produce the table Table_gini.xls



source(here(paste0(chap,"/PH_tables.R")))
# Purpose: 	Produce tables for indicators computed from the above do files 