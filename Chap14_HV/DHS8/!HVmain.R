# /*******************************************************************************************************************************
# Program: 			HVmain.R
# Purpose: 			Main file for HIV Chapter.  
#               The main file will call other R files that will produce the HIV indicators and produce tables.
# Data outputs:	coded variables and table output on screen and in excel tables.  
# Author: 			Shireen Assaf - translated to R by Mahmoud Elkasabi 
# Date last modified:	December 06, 2021 by Mahmoud Elkasabi
# *******************************************************************************************************************************/
rm(list = ls(all = TRUE))

# install the following library if needed.
library(haven)
library(naniar) #to replace values with NA
library(dplyr)
library(sjlabelled)
library(expss)
library(xlsx)
library(here)       # to get R project path
library(stringr)   # for the rename (str_replace_all) function

#path for R project
here()

# Path for this chapter. This is also where the data is stored
chap <- "Chap14_HV"

#####################################################################################
## Define datasets

# IR Files
IRdatafile <- "ZWIR72FL.DTA"

# MR Files
MRdatafile <- "ZWMR72FL.dta"

# CR Files
CRdatafile <- "ZWCR72FL.dta"

# PR Files
PRdatafile <- "ZWPR72FL.dta"

# AR Files - files with HIV test results
ARdatafile <- "ZWAR71FL.dta"


# PR file variables ############################################################

# merge AR file to PR file
ARdata <-  read_dta(here(chap,ARdatafile))
temp <- ARdata

temp[["hv001"]] <- temp[["hivclust"]]
temp[["hv002"]] <- temp[["hivnumb"]]
temp[["hvidx"]] <- temp[["hivline"]]

# open PR dataset
PRdata <-  read_dta(here(chap,PRdatafile))

AR_PMRdata <- merge(PRdata, temp, by = c("hv001", "hv002", "hvidx"), all.y = TRUE, all.x = TRUE)


# add age from IR & MR
# open IR dataset
IRdata <-  read_dta(here(chap,IRdatafile))
w_temp <- (IRdata[, c("v001", "v002","v003", "v012")])
w_temp[["hv001"]] <- w_temp[["v001"]]
w_temp[["hv002"]] <- w_temp[["v002"]]
w_temp[["hvidx"]] <- w_temp[["v003"]]
# open MR dataset
MRdata <-  read_dta(here(chap,MRdatafile))
m_temp <- (MRdata[, c("mv001", "mv002","mv003", "mv012")])
m_temp[["hv001"]] <- m_temp[["mv001"]]
m_temp[["hv002"]] <- m_temp[["mv002"]]
m_temp[["hvidx"]] <- m_temp[["mv003"]]
m_temp[["v012"]] <- m_temp[["mv012"]]

wm_temp <- bind_rows(w_temp,m_temp)

AR_PMRdata <- merge(AR_PMRdata, wm_temp, by = c("hv001", "hv002", "hvidx"), all.y = TRUE, all.x = TRUE)

# use age from IR/MR if exists and from PR otherwise
AR_PMRdata[["age"]] <- ifelse(is.na(AR_PMRdata[["v012"]]), AR_PMRdata[["hv105"]], AR_PMRdata[["v012"]])


# Code to compute HIV testing status
source(here(paste0(chap,"/HV_TEST_COVG.R")))

# limiting to de-facto population only
AR_PMRdata <- AR_PMRdata %>%
  filter(hv103==1)

# limiting to age 15-49, you can comment this out if you want all men
AR_PMRdata <- AR_PMRdata %>%
  filter(age>=15 & age<=49)

# Produce tables for indicators computed from the above R file
source(here(paste0(chap,"/HV_tables_AR.R")))

# IR, MR, AR file ############################################################
# A merge of the IR and MR files with the AR file is needed to produce the Total HIV prevalence and present them by background variables present in the IR and MR files
# The following merge sequence will produce an IRMRARmerge file for the survey of interest

# merge AR file to IR file
ARdata <-  read_dta(here(chap,ARdatafile))
temp <- ARdata

temp[["v001"]] <- temp[["hivclust"]]
temp[["v002"]] <- temp[["hivnumb"]]
temp[["v003"]] <- temp[["hivline"]]

# open IR dataset
IRdata <-  read_dta(here(chap,IRdatafile))
# merge
IRARtemp <- merge(IRdata, temp, by = c("v001", "v002", "v003"), all = FALSE)
IRARtemp <- IRARtemp %>% mutate(sex = 2)

# merge AR file to MR file
ARdata <-  read_dta(here(chap,ARdatafile))
temp <- ARdata

temp[["mv001"]] <- temp[["hivclust"]]
temp[["mv002"]] <- temp[["hivnumb"]]
temp[["mv003"]] <- temp[["hivline"]]

# open MR dataset
MRdata <-  read_dta(here(chap,MRdatafile))
# merge
MRARtemp <- merge(MRdata, temp, by = c("mv001", "mv002", "mv003"), all = FALSE)
MRARtemp <- MRARtemp %>% mutate(sex = 1)

# append IRARtemp and MRARtemp

# IMPORTANT! we are renaming all mv* variables to v* variables. 
names(MRARtemp) <- stringr::str_replace_all(names(MRARtemp),"mv","v")

IRMRARmerge <- bind_rows(IRARtemp,MRARtemp)

# limiting to age 15-49, you can comment this out if you want all men
IRMRARmerge <- IRMRARmerge %>%
  filter(!v012>49)

# Code for HIV prevalence
source(here(paste0(chap,"/HV_PREV.R")))

# Code for HIV prevalence by circumcision indicators
source(here(paste0(chap,"/HV_CIRCUM.R")))

# Code the background variables needed for the HV_tables
source(here(paste0(chap,"/HV_backgroundvars.R")))

# Produce tables for indicators computed from the above R file
source(here(paste0(chap,"/HV_tables_MR.R")))

# CR file ############################################################

# merge CR and AR files
# open AR dataset
ARdata <-  read_dta(here(chap,ARdatafile))
# women
temp <- ARdata

temp[["v001"]] <- temp[["hivclust"]]
temp[["v002"]] <- temp[["hivnumb"]]
temp[["v003"]] <- temp[["hivline"]]
temp[["w_hiv03"]] <- temp[["hiv03"]]

w_temp <- (temp[, c("v001", "v002","v003", "w_hiv03")])
w_temp <- w_temp[order(w_temp$v001, w_temp$v002, w_temp$v003),]

#men
temp <- ARdata

temp[["mv001"]] <- temp[["hivclust"]]
temp[["mv002"]] <- temp[["hivnumb"]]
temp[["mv003"]] <- temp[["hivline"]]
temp[["m_hiv03"]] <- temp[["hiv03"]]

m_temp <- (temp[, c("mv001", "mv002","mv003", "m_hiv03", "hiv05")])
m_temp <- m_temp[order(m_temp$mv001, m_temp$mv002, m_temp$mv003),]

# open CR dataset
CRdata <-  read_dta(here(chap,CRdatafile))
CRdata <- CRdata[order(CRdata$v001, CRdata$v002, CRdata$v003),]
# merge
CRARtemp <- merge(CRdata, w_temp, by = c("v001", "v002", "v003"), all = FALSE)
CRARtemp <- CRARtemp[order(CRARtemp$mv001, CRARtemp$mv002, CRARtemp$mv003),]
CRARmerge <- merge(CRARtemp, m_temp, by = c("mv001", "mv002", "mv003"), all = FALSE)

# Code for HIV prevalence among couples
source(here(paste0(chap,"/HV_PREV_CR.R")))

# Produce tables for indicators computed from the above R file
source(here(paste0(chap,"/HV_tables_CR.R")))

