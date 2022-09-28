# /*******************************************************************************************************************************
# Program: 				FGmain.R
# Purpose: 				Main file for the Female Genital Cutting Chapter. 
# 						    The main file will call other do files that will produce the FG indicators and produce tables.
# Data outputs:		Coded variables and table output on screen and in excel tables.  
# Author: 				Shireen Assaf
# Date last modified:		September 26, 2022 by Shireen Assaf
# Note:					This Chapter is a module and not part of the core questionnaire. 
# 						  Please check if the survey you are interested in has included this module in the survey. 
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
chap <- "Chap18_FG"

# select your survey
 
# IR Files
 IRdatafile <- "ETIR71FL.dta"

# BR Files
 BRdatafile <- "ETBR71FL.dta"

#  MR Files
 MRdatafile <- "ETMR71FL.dta"

# ****************************
# 
# IR and MR file variables
# 
# open dataset
 IRdata <-  read_dta(here(chap,IRdatafile))
 MRdata <-  read_dta(here(chap,MRdatafile))
 
 source(here(paste0(chap,"/FG_CIRCUM.R"))) 
# *Purpose: 	Calculate female circumcision indicators among women and men

 source(here(paste0(chap,"/FG_tables.R"))) 
# *Purpose: 	Produce tables for indicators computed from the above do file.
# *Note:		  Tables are produced among women and men age 15-49. 
# If tables are needed for all men (for some surveys this can be over age 49) the filter can be changed in the code. 

# *******************************************************************************************************************************
# *******************************************************************************************************************************
# 
# * BR file variables
# 
# * open dataset
 BRdata <-  read_dta(here(chap,BRdatafile))
 
 ## Note: Choose which file to run below. Check the notes ##

 source(here(paste0(chap,"/FG_GIRLS.R")))
# *Purpose: 	Calculate female circumcision indicators among girls age 0-14
# *	This do file will also create the tables for these indicators
# * This code only uses the BR file. Older surveys may not have information about the daughter's circumcision in the BR file. 
# * The information may instead be in the IR file. In that case please use the FG_GIRLS_merge.do file. 

# source(here(paste0(chap,"/FG_GIRLS_merge.R"))) 
# *Purpose: 	Calculate female circumcision indicators among girls age 0-14
# *	This do file will also create the tables for these indicators
# * Use this do file if information about the daughter's circumcision status is not found in the BR file. 
# * To compute female circumcision among girls 0-14, we need to merge the IR and BR files
# * This code will reshape the IR file and merge with the BR file so we create a file for daughters. 
# * The information on female circumcision of daughter is reported by the mother in the IR file
# *******************************************************************************************************************************
# *******************************************************************************************************************************
