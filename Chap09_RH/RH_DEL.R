# /*****************************************************************************************************
# Program: 			RH_DEL.R
# Purpose: 			Code Delivery Care indicators
# Data inputs: 	BR dataset
# Data outputs:	coded variables
# Author:			  Shireen Assaf 
# Date last modified: Sept 10, 2021 by Shireen Assaf 		
# *****************************************************************************************************/
#
# /*----------------------------------------------------------------------------//
# Variables created in this file:
# rh_del_place		"Live births by place of delivery"
# rh_del_pltype		"Live births by type of place"
# rh_del_pv			  "Person providing assistance during birth"
# rh_del_pvskill	"Skilled provider providing assistance during birth"
# rh_del_ces			"Live births delivered by cesarean"
# rh_del_cestime	"Timing of decision to have Cesarean"
# rh_del_stay			"Duration of stay following recent birth"
# /----------------------------------------------------------------------------*/
# 
BRdata <- BRdata %>%
  mutate(wt = v005/1000000)

# period and age of child
# choose reference period, last 2 years (24 months) or last 5 years (60 months)
# Using a period of the last 2 years will not match final report but would provide more recent information.
BRdata <- BRdata %>%
  mutate(period = 60)

# age of child. If b19 is not available in the data use v008 - b3
if ("TRUE" %in% (!("b19" %in% names(BRdata))))
  BRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(BRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  BRdata <- BRdata %>%
    mutate(age = b19)
} else {
  BRdata <- BRdata %>%
    mutate(age = v008 - b3)
}


# //Place of delivery
# Note: please check the categories of m15 especially for older surveys. The category values may differ. 
BRdata <- BRdata %>%
  mutate(rh_del_place =
           case_when(
             m15 >=20 & m15<40   ~ 1 ,
             m15 >=10 & m15<20   ~ 2,
             m15 >=40 & m15<99   ~ 3 ,
             m15 == 99 ~ 9 ,
             age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_place = c(99))) %>%
  set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
  set_variable_labels(rh_del_place = "Live births by place of delivery")
# 
# //Place of delivery - by place type
BRdata <- BRdata %>%
  mutate(rh_del_pltype =
           case_when(
             m15 >=20 & m15<30   ~ 1 ,
             m15 >=30 & m15<40   ~ 2 ,
             m15 >=10 & m15<20   ~ 3,
             m15 >=40 & m15<99   ~ 4 ,
             m15 == 99 ~ 9 ,
             age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_pltype = c(99))) %>%
  set_value_labels(rh_del_pltype = c("Health facility - public" = 1, "Health facility - private" = 2, "Home"=3, "Other"=4, "Missing"=9  )) %>%
  set_variable_labels(rh_del_pltype = "Live births by type of health facility")

# //Assistance during delivery
# **Note: Assistance during delivery and skilled provider indicators are both country specific indicators. 
# **The table for these indicators in the final report would need to be checked to confirm the code below.
BRdata <- BRdata %>%
  mutate(rh_del_pv =
           case_when(
             m3a == 1   ~ 1 ,
             m3b == 1 ~ 2,
             m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1~ 3 ,
             m3g == 1 ~ 4 ,
             m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
             m3n ==1 ~ 6,
             m3a ==8 | m3a==9 ~ 9 ,
             age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_pv = c(99))) %>%
  set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
  set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

# //Skilled provider during delivery
# ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
BRdata <- BRdata %>%
  mutate(rh_del_pvskill =
           case_when(
             rh_del_pv %in% c(1,2)   ~ 1 ,
             rh_del_pv %in% c(3,4,5) ~ 2,
             rh_del_pv ==6 ~ 3 ,
             rh_del_pv==9 ~ 9 ,
             age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
  set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
  set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")
 	
# //Caesarean delivery
BRdata <- BRdata %>%
  mutate(rh_del_ces =
           case_when(
             m17==1   ~ 1 ,
             age>=period ~ 99,
             TRUE ~ 0 )) %>%
  replace_with_na(replace = list(rh_del_ces = c(99))) %>%
  set_value_labels(rh_del_ces = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(rh_del_ces = "Live births delivered by Caesarean")

# //Timing of decision for caesarean
#  some surveys did not ask this question, confirm m17a exists
BRdata <- BRdata %>%
  mutate(rh_del_cestime =
           case_when(
             m17a==1 ~ 1 ,
             m17a==2 ~ 2 , 
             age>=period ~ 99,
             TRUE ~ 0 )) %>%
  replace_with_na(replace = list(rh_del_cestime = c(99))) %>%
  set_value_labels(rh_del_cestime = c("Vaginal birth"=0, "before labor started"=1, "after labor started"=2  )) %>%
  set_variable_labels(rh_del_cestime = "Timing of decision to have Caesarean")

# //Duration of stay following recent birth
BRdata <- BRdata %>%
  mutate(rh_del_stay =
           case_when(
             m61<106   ~ 1 ,
             m61>=106 & m61<112 ~ 2,
             m61>=112 & m61<124 ~ 3 ,
             (m61>=124 & m61<172) | m61==201 | m61==202 ~ 4 ,
             (m61>=172 & m61<200) | (m61>=203 & m61<400) ~ 5 ,
             m61==998 | m61==999 ~ 9 ,
             rh_del_place!=1 | bidx!=1 | age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_stay = c(99))) %>%
  set_value_labels(rh_del_stay = c("<6 hours" = 1, "6-11 hours"=2, "12-23 hours"=3, "1-2 days"=4, "3+ days"=5, "Don't know/missing"=9  )) %>%
  set_variable_labels(rh_del_stay = "Duration of stay following recent birth")
