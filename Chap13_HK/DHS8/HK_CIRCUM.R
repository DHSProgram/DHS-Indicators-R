# ******************************************************************************
# Program: 			  HK_CIRCUM.R - DHS8 update
# Purpose: 			  Code to compute indicators on Sexually Transmitted Infections (STI)
# Data inputs: 		MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: August 30, 2024 by Courtney Allen 
# Note:				The indicators are computed for all men. No age selection is made here. 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_circum				"Circumcised"
# hk_circum_type	"Type of circumcision" - NEW indicator in DHS8

# NOTES ------------------------------------------------------------------------
# The indicators are computed for men. No age selection is made here. 
# One ndicator has been discontiued in DHS8. Please check the excel indicator list for these indicators.

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

# //Type of circumcision - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_circum_type= case_when(
  mv483d==1 & mv483e==0 ~ 1,
  mv483d==0 & mv483e==1 ~ 2,
  mv483d==1 & mv483e==1 ~ 3,
  mv483==1 & (mv483d!=1 | mv483e!=1) ~ 4,
  mv483!=1 ~ 0,
  TRUE ~ 0)) %>%
  set_value_labels(hk_circum_type = c("Not circumcised or don't know"=0, 
                                      "Traditional only"=1, 
                                      "Medical only"=2,
                                      "Both traditionally and medically"=3,
                                      "Circumcised but don't know type"=4)) %>%
  set_variable_labels(hk_circum_type = "Type of circumcision")






