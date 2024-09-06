# ******************************************************************************
# Program: 		ML_NETS_HH.R - No changes in DHS8
# Purpose: 		Code for household ITN ownership  
# Data inputs: 		HR survey list
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: July 26, 2024 by Courtney Allen
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_mosquitonet		"Households with at least one mosquito net"
# ml_itnhh				  "Households with at least one ITN"
# ml_avgmosnethh		"Average number of mosquito nets per household"
# ml_avgitnhh				"Average number of ITNs per household"
# ml_mosnethhaccess	"Households with >1 mosquito net per 2 household members"
# ml_hhaccess				"Households with >1 ITN per 2 household members"
# -----------------------------------------------------------------------------#

# Household mosquito net ownership
HRdata <- HRdata %>%
  mutate(ml_mosquitonet = case_when(
    hv227==1   ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_mosquitonet = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_mosquitonet = "Household owns at least one mosquito net")

# Household ITN ownership
HRdata <- HRdata %>%
  mutate(ml_itnhh = case_when(
    hml10_1==1 | hml10_2==1 | hml10_3==1 | hml10_4==1 | hml10_5==1 | hml10_6==1 | hml10_7 ==1   ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_itnhh = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_itnhh = "Household owns at least one ITN")

# Number of mosquito nets per household
HRdata <- HRdata %>%
  mutate(ml_numnets = hml1) %>%
  set_variable_labels(ml_numnets = "Number of mosquito nets per household")

# Number of ITNs per household
HRdata <- HRdata %>%
  mutate(itnhh_01 = case_when(hml10_1==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_02 = case_when(hml10_2==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_03 = case_when(hml10_3==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_04 = case_when(hml10_4==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_05 = case_when(hml10_5==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_06 = case_when(hml10_6==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_07 = case_when(hml10_7==1 ~ 1,TRUE ~ 0)) %>%
  mutate(ml_numitnhh = itnhh_01 + itnhh_02 + itnhh_03 + itnhh_04 + itnhh_05 + itnhh_06 + itnhh_07) %>%
  set_variable_labels(ml_numitnhh = "Number of ITNs per household")

# Average number of mosquito nets per household
ml_avgmosnethh <- HRdata %>%
  summarise(ml_avgmosnethh = weighted.mean(ml_numnets, hv005/1000000))

# Average number of ITNs per household
ml_avgitnhh <- HRdata %>%
  summarise(ml_avgitnhh = weighted.mean(ml_numitnhh, hv005/1000000))

# Households with > 1 mosquito net per 2 members
HRdata <- HRdata %>%
  mutate(ml_mosnetpotuse = ml_numnets*2) %>%
  set_variable_labels(ml_mosnetpotuse= "Potential mosquito net users in household")

HRdata <- HRdata %>%
  mutate(ml_mosnethhaccess0 =ml_mosnetpotuse/hv013) %>%
  mutate(ml_mosnethhaccess = case_when(
    hv013==0 ~ 99,
    ml_mosnethhaccess0 >= 1   ~ 1,
    TRUE   ~ 0)) %>%
  set_value_labels(ml_mosnethhaccess = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_mosnethhaccess= "Households with >1 mosquito net per 2 household members") %>%
  replace_with_na(replace = list(ml_mosnethhaccess = c(99)))

# Households with > 1 ITN per 2 members
HRdata <- HRdata %>%
  mutate(ml_potuse = ml_numitnhh*2) %>%
  set_variable_labels(ml_potuse= "Potential ITN users in household")

HRdata <- HRdata %>%
  mutate(ml_hhaccess0 =ml_potuse/hv013) %>%
  mutate(ml_hhaccess = case_when(
    hv013==0 ~ 99,
    ml_hhaccess0 >= 1   ~ 1,
    TRUE   ~ 0)) %>%
  set_value_labels(ml_hhaccess = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_hhaccess= "Households with >1 ITN per 2 household members") %>%
  replace_with_na(replace = list(ml_hhaccess = c(99)))

