# /*****************************************************************************************************
# Program: 			NT_CH_MICRO.R
# Purpose: 			Code to compute micronutrient indicators in children
# Data inputs: 	KR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Dec 3, 2021 by Shireen Assaf 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# nt_ch_micro_mp		"Children age 6-23 mos given multiple micronutrient powder"
# nt_ch_micro_iron	"Children age 6-59 mos given iron supplements"
# nt_ch_micro_vas		"Children age 6-59 mos given Vit. A supplements"
# nt_ch_micro_dwm		"Children age 6-59 mos given deworming medication"
# nt_ch_micro_iod		"Children age 6-59 mos live in hh with iodized salt"
# nt_ch_food_ther		"Children age 6-35 mos given therapeutic food"
# nt_ch_food_supp		"Children age 6-35 mos given supplemental food"
# 
# ----------------------------------------------------------------------------*/

# age of child. If b19 is not available in the data use v008 - b3
if ("TRUE" %in% (!("b19" %in% names(KRdata))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  KRdata <- KRdata %>%
    mutate(age = b19)
} else {
  KRdata <- KRdata %>%
    mutate(age = v008 - b3)
}


KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# //Received multiple micronutrient powder
KRdata <- KRdata %>%
  mutate(nt_ch_micro_mp =
           case_when(
             age<6 | age>23 | b5==0 ~ 99, 
             h80a!=1 ~ 0 ,
             h80a==1 ~ 1 )) %>%
  replace_with_na(replace = list(nt_ch_micro_mp = c(99))) %>%
  set_value_labels(nt_ch_micro_mp = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_mp = "Children age 6-23 mos given multiple micronutrient powder")

# //Received iron supplements
KRdata <- KRdata %>%
  mutate(nt_ch_micro_iron =
           case_when(
             age<6 | age>59 | b5==0 ~ 99, 
             h42!=1 ~ 0 ,
             h42==1 ~ 1 )) %>%
  replace_with_na(replace = list(nt_ch_micro_iron = c(99))) %>%
  set_value_labels(nt_ch_micro_iron = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_iron = "Children age 6-59 mos given iron supplements")

# //Received Vit. A supplements
KRdata <- KRdata %>%
  mutate(h33m2= h33m) %>%
  replace_with_na(replace = list(h33m2 = c(98))) %>%
  mutate(h33d2= if_else(h33d==98, 15, as.numeric(h33d))) %>%
  mutate(h33y2= h33y) %>%
  replace_with_na(replace = list(h33y2 = c(9998))) %>%
  mutate(Date = as.Date(with(KRdata,paste(h33y2,h33m2,h33d2,sep="-")),"%Y-%m-%d")) %>%
  mutate(mdyc = as.integer((v008a - (difftime(Date, "1960-01-01", units = "days") + 21916)) /30.4375)) %>%  
  mutate(nt_ch_micro_vas =
           case_when(
             ((age >= 6 & age <= 59)) & (h34==1 | mdyc <= 6)  ~ 1,
             !(age >= 6 & age <= 59) | b5==0 ~ 99,
             TRUE~ 0,)) %>%
  replace_with_na(replace = list(nt_ch_micro_vas = c(99))) %>%
  set_value_labels(nt_ch_micro_vas = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_vas = "Children age 6-59 mos given Vit. A supplements")

# //Received deworming medication
KRdata <- KRdata %>%
  mutate(nt_ch_micro_dwm =
           case_when(
             age<6 | age>59 | b5==0 ~ 99, 
             h43!=1 ~ 0 ,
             h43==1 ~ 1 )) %>%
  replace_with_na(replace = list(nt_ch_micro_dwm = c(99))) %>%
  set_value_labels(nt_ch_micro_dwm = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_dwm = "Children age 6-59 mos given deworming medication")

# //Child living in household with iodized salt 
KRdata <- KRdata %>%
  mutate(nt_ch_micro_iod =
           case_when(
             age<6 | age>59 | b5==0 | hv234a>1  ~ 99,
             hv234a==0   ~ 0, 
             hv234a==1  ~ 1)) %>%
  replace_with_na(replace = list(nt_ch_micro_iod = c(99))) %>%
  set_value_labels(nt_ch_micro_iod = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_iod = "Children age 6-59 mos live in hh with iodized salt")

# //Received therapeutic food
KRdata <- KRdata %>%
  mutate(nt_ch_food_ther =
           case_when(
             age<6 | age>35 | b5==0  ~ 99,
             h80b!=1   ~ 0, 
             h80b==1  ~ 1)) %>%
  replace_with_na(replace = list(nt_ch_food_ther = c(99))) %>%
  set_value_labels(nt_ch_food_ther = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_food_ther = "Children age 6-35 mos given therapeutic food")

# //Received supplemental food
KRdata <- KRdata %>%
  mutate(nt_ch_food_supp =
           case_when(
             age<6 | age>35 | b5==0  ~ 99,
             h80c!=1   ~ 0, 
             h80c==1  ~ 1)) %>%
  replace_with_na(replace = list(nt_ch_food_supp = c(99))) %>%
  set_value_labels(nt_ch_food_supp = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_food_supp = "Children age 6-35 mos given supplemental food")