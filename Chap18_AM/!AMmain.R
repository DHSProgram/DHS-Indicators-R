# /*******************************************************************************************************************************
# Program: 			AMmain.R
# Purpose: 			Main file for the Adults Mortality Chapter.  
#               The main file will call other R file that will produce the AM indicators and produce tables.
# Data outputs:	Table output on in excel tables.  
# Author: 			Mahmoud Elkasabi 
# Date last modified:	December 09, 2021 by Mahmoud Elkasabi
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(haven)
library(xlsx)
library(DHS.rates)
library(here)

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap17_AM"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "ZWIR72FL.DTA"

#####################################################################################

IRdata <-  read_dta(here(chap,IRdatafile))

# Code and tables for adult mortality rates 
source(here(paste0(chap,"/AM_rates.R")))