# /*****************************************************************************************************
# Program: 			NT_MN_NUT.R
# Purpose: 			Code to compute anthropometry and anemia indicators in men
# Data inputs: 	MR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Dec 6, 2021 by Shireen Assaf 
# Note:				
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# nt_mn_any_anem		  "Any anemia - men"
# 
# nt_mn_bmi_mean		  "Mean BMI  - men 15-49"
# nt_mn_bmi_mean_all 	"Mean BMI  - all men"
# nt_mn_norm			    "Normal BMI - men"
# nt_mn_thin			    "Thin BMI - men"
# nt_mn_mthin			    "Mildly thin BMI  - men"
# nt_mn_modsevthin	  "Moderately and severely thin BMI - men"
# nt_mn_ovobese		    "Overweight or obese BMI  - men"
# nt_mn_ovwt			    "Overweight BMI  - men"
# nt_mn_obese			    "Obese BMI  - men"
# 
# ----------------------------------------------------------------------------*/

MRdata <- MRdata %>%
  mutate(wt = mv005/1000000)
# 
# *** Anemia indicators ***
# 
# //Any anemia
MRdata <- MRdata %>%
  mutate(nt_mn_any_anem =
           case_when(
              hv103==0 | hv042!=1 | hb55!=0  ~ 99,
              hb56<130 ~ 1 ,
              hb56>=130 ~ 0)) %>%
  replace_with_na(replace = list(nt_mn_any_anem = c(99))) %>%
  set_value_labels(nt_mn_any_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_any_anem = "Any anemia - men")


# *** Anthropometry indicators ***
# 
# //Mean BMI - 15-49
MRdata <- MRdata %>%
  mutate(bmi = case_when(hb40>=1200 & hb40<=6000 & mv013<8 ~ hb40/100)) 
MRdata$nt_mn_bmi_mean <- matrixStats::weightedMean(MRdata$bmi, MRdata$wt, idxs = NULL, na.rm = TRUE) 

# //Mean BMI - all men
MRdata <- MRdata %>%
  mutate(bmi = case_when(hb40>=1200 & hb40<=6000 ~ hb40/100)) 
MRdata$nt_mn_bmi_mean_all <- matrixStats::weightedMean(MRdata$bmi, MRdata$wt, idxs = NULL, na.rm = TRUE) 

# //Normal weight
MRdata <- MRdata %>%
  mutate(nt_mn_norm =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40< 1850 | hb40> 2499  ~ 0, 
             hb40>=1850 & hb40<=2499  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_norm = c(99))) %>%
  set_value_labels(nt_mn_norm = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_norm = "Normal BMI - men")

# //Thin
MRdata <- MRdata %>%
  mutate(nt_mn_thin =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40>= 1850   ~ 0, 
             hb40>=1200 & hb40<1850  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_thin = c(99))) %>%
  set_value_labels(nt_mn_thin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_thin = "Thin BMI - men")

# //Mildly thin
MRdata <- MRdata %>%
  mutate(nt_mn_mthin =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40<1700 | hb40>= 1850   ~ 0, 
             hb40>=1700 & hb40<1850  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_mthin = c(99))) %>%
  set_value_labels(nt_mn_mthin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_mthin = "Mildly thin BMI - men")

# //Moderately and severely thin
MRdata <- MRdata %>%
  mutate(nt_mn_modsevthin =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40>= 1700   ~ 0, 
             hb40>=1200 & hb40<1700  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_modsevthin = c(99))) %>%
  set_value_labels(nt_mn_modsevthin = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_modsevthin = "Moderately and severely thin BMI - men")
 
# //Overweight or obese
MRdata <- MRdata %>%
  mutate(nt_mn_ovobese =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40< 2500   ~ 0, 
             hb40>=2500 & hb40<=6000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_ovobese = c(99))) %>%
  set_value_labels(nt_mn_ovobese = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_ovobese = "Overweight or obese BMI - men")

# //Overweight
MRdata <- MRdata %>%
  mutate(nt_mn_ovwt =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40< 2500  | hb40>=3000 ~ 0, 
             hb40>=2500 & hb40<3000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_ovwt = c(99))) %>%
  set_value_labels(nt_mn_ovwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_ovwt = "Overweight BMI - men")

# //Obese
MRdata <- MRdata %>%
  mutate(nt_mn_obese =
           case_when(
             hb40<1200 | hb40>6000  ~ 99,
             hb40< 3000   ~ 0, 
             hb40>=3000 & hb40<=6000  ~ 1 )) %>%
  replace_with_na(replace = list(nt_mn_obese = c(99))) %>%
  set_value_labels(nt_mn_obese = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mn_obese = "Obese BMI - men")
