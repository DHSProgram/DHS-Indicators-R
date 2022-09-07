# ******************************************************************************
# Program: 			  HK_CIRCUM.R
# Purpose: 			  Code to compute indicators on Sexually Transmitted Infections (STI)
# Data inputs: 		MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# Note:				The indicators are computed for all men. No age selection is made here. 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_circum				"Circumcised"
# hk_circum_status_prov	"Circumcision status and provider"


# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)


# CIRCUMCISION AMONG MEN -------------------------------------------------------


# //Circumcised
MRdata <- MRdata %>% mutate(hk_circum = case_when(
  mv483 == 1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_circum = yesno) %>%
  set_variable_labels(hk_circum = "Circumcised")

# //Circumcision status and provider
MRdata <- MRdata %>% mutate(hk_circum_status_prov = case_when(
  mv483 == 0 ~ 0,
  mv483b > 2 ~ 3,
  mv483 %in% 8:9 ~ 9,
  TRUE ~ as.numeric(mv483b))) %>%
  set_value_labels(hk_circum_status_prov = c("Not circumcised" = 0,
                   "Traditional practitioner, family, or friend" =1,
                   "Health worker, health professional" = 2,
                   "Other/dont know/missing" = 3,
                   "Dont know/missing circumcision status" = 9)) %>%
  set_variable_labels(hk_circum_status_prov = "Circumcision status and provider")






