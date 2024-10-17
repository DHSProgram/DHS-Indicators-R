# ******************************************************************************
# Program: 			  HK_STI.R - DHS8 update
# Purpose: 			  Code to compute indicators on Sexually Transmitted Infections (STI)
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2024 by Courtney Allen 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_sti				"Had STI in the past 12 months"
# hk_gent_disch		"Had abnormal (or bad-smelling) genital discharge in the past 12 months"
# hk_gent_sore		"Had genital sore or ulcer in the past 12 months"
# hk_sti_symp			"Had STI or STI symptoms in the past 12 months"

# NOTES ------------------------------------------------------------------------
# The indicators below can be computed for men and women. No age selection is made here.
# Several indicators have also been discontiued in DHS8. Please check the excel indicator list for these indicators.

# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)


# SELF REPORT STIS (WOMEN) -----------------------------------------------------
# STI in the past 12 months
IRdata <- IRdata %>%  mutate(hk_sti = case_when(
  v763a==1 ~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti = yesno) %>%
  set_variable_labels(hk_sti = "Had STI in the past 12 months")

# Discharge in the past 12 months
IRdata <- IRdata %>%  mutate(hk_gent_disch = case_when(
  v763c==1 ~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_disch = yesno) %>%
  set_variable_labels(hk_gent_disch= "Had abnormalgenital discharge in past 12 mnths")

# Genital sore in past 12 months
IRdata <- IRdata %>%  mutate(hk_gent_sore = case_when(
  v763b==1 ~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_sore = yesno) %>%
  set_variable_labels(hk_gent_sore = "Had genital sore or ulcer in past 12 mnths")

# STI or STI symptoms in the past 12 months
IRdata <- IRdata %>%  mutate(hk_sti_symp = case_when(
  v763a==1 | v763b==1 | v763c==1~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti_symp = yesno) %>%
  set_variable_labels(hk_sti_symp = "Had STI or STI symptoms in past 12 mnths")


# SELF REPORT STIS (MEN) -------------------------------------------------------



MRdata <- MRdata %>%  mutate(hk_sti = case_when(
  mv763a==1 ~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti = yesno) %>%
  set_variable_labels(hk_sti = "Had STI in the past 12 months")

# Discharge in the past 12 months
MRdata <- MRdata %>%  mutate(hk_gent_disch = case_when(
  mv763c==1 ~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_disch = yesno) %>%
  set_variable_labels(hk_gent_disch= "Had abnormalgenital discharge in past 12 mnths")

# Genital sore in past 12 months
MRdata <- MRdata %>%  mutate(hk_gent_sore = case_when(
  mv763b==1 ~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_sore = yesno) %>%
  set_variable_labels(hk_gent_sore = "Had genital sore or ulcer in past 12 mnths")

# STI or STI symptoms in the past 12 months
MRdata <- MRdata %>%  mutate(hk_sti_symp = case_when(
  mv763a==1 | mv763b==1 | mv763c==1~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti_symp = yesno) %>%
  set_variable_labels(hk_sti_symp = "Had STI or STI symptoms in past 12 mnths")



