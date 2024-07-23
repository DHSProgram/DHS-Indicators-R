# ******************************************************************************
# Program: 		ML_NETS_HH.R
# Purpose: 		Code for household ITN ownership  
# Data inputs: 		HR survey list
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: January 12, 2022 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_mosquitonet			"Households with at least one mosquito net"
# ml_itnhh				"Households with at least one ITN"
# ml_avgmosnethh			"Average number of mosquito nets per household"
# ml_avgitnhh				"Average number of ITNs per household"
# ml_mosnethhaccess		"Households with >1 mosquito net per 2 household members"
# ml_hhaccess				"Households with >1 ITN per 2 household members"
# -----------------------------------------------------------------------------#

# Household mosquito net ownership
HRdata <- HRdata %>%
  mutate(ml_mosquitonet = case_when(
    hv227==1   ~ 1,
    TRUE ~ 0),
    ml_mosquitonet = add_labels(ml_mosquitonet, labels = c("No"=0, "Yes"=1)),
    ml_mosquitonet = set_label(ml_mosquitonet, label = "Household owns at least one mosquito net"))

# Household ITN ownership
HRdata <- HRdata %>%
  mutate(ml_itnhh = case_when(
    hml10_1==1 | hml10_2==1 | hml10_3==1 | hml10_4==1 | hml10_5==1 | hml10_6==1 | hml10_7 ==1   ~ 1,
    TRUE ~ 0),
    ml_itnhh = add_labels(ml_itnhh, labels = c("No"=0, "Yes"=1)),
    ml_itnhh = set_label(ml_itnhh, label = "Household owns at least one ITN"))

# Number of mosquito nets per household
HRdata <- HRdata %>%
  mutate(ml_numnets = hml1,
    ml_numnets = set_label(ml_numnets, label = "Household owns at least one mosquito net"))

# Number of ITNs per household
HRdata <- HRdata %>%
  mutate(itnhh_01 = case_when(hml10_1==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_02 = case_when(hml10_2==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_03 = case_when(hml10_3==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_04 = case_when(hml10_4==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_05 = case_when(hml10_5==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_06 = case_when(hml10_6==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_07 = case_when(hml10_7==1 ~ 1,TRUE ~ 0)) %>%
  mutate(ml_numitnhh = itnhh_01 + itnhh_02 + itnhh_03 + itnhh_04 + itnhh_05 + itnhh_06 + itnhh_07,
    ml_numitnhh = set_label(ml_numitnhh, label = "Number of ITNs per household"))

# Average number of mosquito nets per household
ml_avgmosnethh <- HRdata %>%
  summarise(ml_avgmosnethh = weighted.mean(ml_numnets, hv005/1000000))

# Average number of ITNs per household
ml_avgitnhh <- HRdata %>%
  summarise(ml_avgitnhh = weighted.mean(ml_numitnhh, hv005/1000000))

# Households with > 1 mosquito net per 2 members
HRdata <- HRdata %>%
  mutate(ml_mosnetpotuse = ml_numnets*2,
         ml_mosnetpotuse = set_label(ml_mosnetpotuse, label = "Potential mosquito net users in household"))

HRdata <- HRdata %>%
  mutate(ml_mosnethhaccess0 =ml_mosnetpotuse/hv013) %>%
  mutate(ml_mosnethhaccess = case_when(
    hv013==0 ~ 99,
    ml_mosnethhaccess0 >= 1   ~ 1,
    TRUE   ~ 0),
    ml_mosnethhaccess = set_label(ml_mosnethhaccess, label = "Households with >1 mosquito net per 2 household members"))%>%
  replace_with_na(replace = list(ml_mosnethhaccess = c(99)))

# Households with > 1 ITN per 2 members
HRdata <- HRdata %>%
  mutate(ml_potuse = ml_numitnhh*2,
         ml_potuse = set_label(ml_potuse, label = "Potential ITN users in household"))

HRdata <- HRdata %>%
  mutate(ml_hhaccess0 =ml_potuse/hv013) %>%
  mutate(ml_hhaccess = case_when(
    hv013==0 ~ 99,
    ml_hhaccess0 >= 1   ~ 1,
    TRUE   ~ 0),
    ml_hhaccess = set_label(ml_hhaccess, label = "Households with >1 ITN per 2 household members"))%>%
  replace_with_na(replace = list(ml_hhaccess = c(99)))
