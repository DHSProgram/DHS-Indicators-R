# /*****************************************************************************************************
# Program: 			NT_WM_NUT.R
# Purpose: 			Code to compute anthropometry and anemia indicators in women
# Data inputs: 	IR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: December 6, 2021 by Shireen Assaf 
# Note:				For ever-married sample surveys please use the PR file instead of the IR file for anemia and anthropometry indicators.
# 					  Please check the guide to DHS statistics for calculations: 
# 					  For anemia using the PR file, use the variables ha57, hv103, and ha55 to produce the estimates
# 					  For anthropometry using PR file, use the variables ha3, ha40, and hv103.
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# nt_wm_any_anem		"Any anemia - women"
# nt_wm_mild_anem		"Mild anemia - women"
# nt_wm_mod_anem		"Moderate anemia - women"
# nt_wm_sev_anem		"Severe anemia - women"
# 
# nt_wm_ht			    "Height under 145cm - women"	
# 
# nt_wm_bmi_mean		"Mean BMI  - women"
# nt_wm_norm			  "Normal BMI - women"
# nt_wm_thin			  "Thin BMI - women"
# nt_wm_mthin			  "Mildly thin BMI  - women"
# nt_wm_modsevthin	"Moderately and severely thin BMI - women"
# nt_wm_ovobese		  "Overweight or obese BMI  - women"
# nt_wm_ovwt			  "Overweight BMI  - women"
# nt_wm_obese			  "Obese BMI  - women"
# 
# nt_wm_micro_iron	"Number of days women took iron supplements during last pregnancy"
# nt_wm_micro_dwm		"Women who took deworming medication during last pregnancy"
# nt_wm_micro_iod		"Women living in hh with iodized salt"
# 
# ----------------------------------------------------------------------------*/

IRdata <- IRdata %>%
  mutate(wt = v005/1000000)
# 
# *** Anemia indicators ***
# 
# //Any anemia
IRdata <- IRdata %>%
  mutate(nt_wm_any_anem =
           case_when(
             v042==1 & v457<4 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_any_anem = "Any anemia - women")

# //Mild anemia
IRdata <- IRdata %>%
  mutate(nt_wm_mild_anem =
           case_when(
             v042==1 & v457==3 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_mild_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_mild_anem = "Mild anemia - women")

# //Moderate anemia
IRdata <- IRdata %>%
  mutate(nt_wm_mod_anem =
           case_when(
             v042==1 & v457==2 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_mod_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_mod_anem = "Moderate anemia - women")

# //Severe anemia
IRdata <- IRdata %>%
  mutate(nt_wm_sev_anem =
           case_when(
             v042==1 & v457==1 ~ 1 ,
             v042==1 &  v455==0 ~ 0)) %>%
  set_value_labels(nt_wm_sev_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_sev_anem = "Severe anemia - women")

# *** Anthropometry indicators ***

# * age of most recent child
# age of child. If b19_01 is not available in the data use v008 - b3_01
if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
  IRdata [[paste("b19_01")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $b19_01)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  IRdata <- IRdata %>%
    mutate(age = b19_01)
} else {
  IRdata <- IRdata %>%
    mutate(age = v008 - b3_01)
}

# //Height less than 145cm
IRdata <- IRdata %>%
  mutate(nt_wm_ht =
           case_when(
             v438<1450 & v438>=1300 & v438<=2200 ~ 1 ,
             v438>=1450 & v438>=1300 & v438<=2200  ~ 0)) %>%
  set_value_labels(nt_wm_ht = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_ht = "Height under 145cm - women")

# //Mean BMI
IRdata <- IRdata %>%
  mutate(bmi = case_when(v445>=1200 & v445<=6000 & v213==0 & (v208==0 | age>=2) ~ v445/100)) 
IRdata$nt_wm_bmi_mean <- matrixStats::weightedMean(IRdata$bmi, IRdata$wt, idxs = NULL, na.rm = TRUE) 
 

# //Normal weight
IRdata <- IRdata %>%
  mutate(nt_wm_norm =
           case_when(
              v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
              v445< 1850 | v445> 2499  ~ 0, 
              v445>=1850 & v445<=2499  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_norm = c(99))) %>%
  set_value_labels(nt_wm_norm = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_norm = "Normal BMI - women")

# //Thin
IRdata <- IRdata %>%
  mutate(nt_wm_thin =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445>= 1850   ~ 0, 
             v445>=1200 & v445<1850  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_thin = c(99))) %>%
  set_value_labels(nt_wm_thin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_thin = "Thin BMI - women")

# //Mildly thin
IRdata <- IRdata %>%
  mutate(nt_wm_mthin =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445<1700 | v445>= 1850   ~ 0, 
             v445>=1700 & v445<1850  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_mthin = c(99))) %>%
  set_value_labels(nt_wm_mthin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_mthin = "Mildly thin BMI - women")

# //Moderately and severely thin
IRdata <- IRdata %>%
  mutate(nt_wm_modsevthin =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445>= 1700   ~ 0, 
             v445>=1200 & v445<1700  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_modsevthin = c(99))) %>%
  set_value_labels(nt_wm_modsevthin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_modsevthin = "Moderately and severely thin BMI - women")

# //Overweight or obese
IRdata <- IRdata %>%
  mutate(nt_wm_ovobese =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 2500   ~ 0, 
             v445>=2500 & v445<=6000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_ovobese = c(99))) %>%
  set_value_labels(nt_wm_ovobese = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_ovobese = "Overweight or obese BMI - women")

# //Overweight
IRdata <- IRdata %>%
  mutate(nt_wm_ovwt =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 2500  | v445>=3000 ~ 0, 
             v445>=2500 & v445<3000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_ovwt = c(99))) %>%
  set_value_labels(nt_wm_ovwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_ovwt = "Overweight BMI - women")

# //Obese
IRdata <- IRdata %>%
  mutate(nt_wm_obese =
           case_when(
             v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
             v445< 3000   ~ 0, 
             v445>=3000 & v445<=6000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_wm_obese = c(99))) %>%
  set_value_labels(nt_wm_obese = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_obese = "Obese BMI - women")

# //Took iron supplements during last pregnancy
IRdata <- IRdata %>%
  mutate(nt_wm_micro_iron =
           case_when(
             v208==0 ~ 99,
             m45_1==0   ~ 0, 
             m46_1<60  ~ 1, 
             m46_1>=60 & m46_1<90 ~ 2,
             m46_1>=90 & m46_1<=300 ~ 3,
             m46_1>=998 | m45_1>=8 ~ 4)) %>%
  replace_with_na(replace = list(nt_wm_micro_iron = c(99))) %>%
  set_value_labels(nt_wm_micro_iron = c("None"=0, "<60"=1, "60-89"=2, "90+"=3, "Don't know/missing"=4)) %>%
  set_variable_labels(nt_wm_micro_iron = "Number of days women took iron supplements during last pregnancy")

# //Took deworming medication during last pregnancy
IRdata <- IRdata %>%
  mutate(nt_wm_micro_dwm =
           case_when(
             v208==0 ~ 99,
             m60_1!=1   ~ 0, 
             m60_1==1  ~ 1)) %>%
  replace_with_na(replace = list(nt_wm_micro_dwm = c(99))) %>%
  set_value_labels(nt_wm_micro_dwm = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_micro_dwm = "Women who took deworming medication during last pregnancy")

# //Woman living in household with iodized salt 
IRdata <- IRdata %>%
  mutate(nt_wm_micro_iod =
           case_when(
             v208==0 | hv234a>1  ~ 99,
             hv234a==0   ~ 0, 
             hv234a==1  ~ 1)) %>%
  replace_with_na(replace = list(nt_wm_micro_iod = c(99))) %>%
  set_value_labels(nt_wm_micro_iod = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_wm_micro_iod = "Women living in hh with iodized salt")