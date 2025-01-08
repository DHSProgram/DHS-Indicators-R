# /*****************************************************************************************************
# Program: 			  CH_VAC.R
# Purpose: 			  Code vaccination variables.
# Data inputs: 		KR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: August 16, 2022 by Shireen Assaf 
# Notes:				Estimates can be created for two age groups (12-23) and (24-35). 
# 					
# 					!! Please choose the age group of interest in line 100. Default is age group 12-23
# 					This code will create a subset of the KR data file KRvac that selects for children in the age group of interest 

# 					Vaccination indicators are country specific. However, most common vaccines are coded below and the same logic can be applied to others.
# 					When the vaccine is a single dose, the logic for single dose vaccines can be used (ex: bcg).
# 					When the vaccine has 3 doses, the logic for multiple dose vaccines can be used (ex: dpt)
# *****************************************************************************************************/
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_bcg_card			"BCG vaccination according to card"
# ch_bcg_moth			"BCG vaccination according to mother"
# ch_bcg_either		"BCG vaccination according to either source"
# 
# ch_pent1_card		"Pentavalent 1st dose vaccination according to card"
# ch_pent1_moth		"Pentavalent 1st dose vaccination according to mother"
# ch_pent1_either	"Pentavalent 1st dose vaccination according to either source"
# ch_pent2_card		"Pentavalent 2nd dose vaccination according to card"
# ch_pent2_moth		"Pentavalent 2nd dose vaccination according to mother"
# ch_pent2_either	"Pentavalent 2nd dose vaccination according to either source"
# ch_pent3_card		"Pentavalent 3rd dose vaccination according to card"
# ch_pent3_moth		"Pentavalent 3rd dose vaccination according to mother"
# ch_pent3_either	"Pentavalent 3rd dose vaccination according to either source"
# 
# ch_polio0_card		"Polio at birth vaccination according to card"
# ch_polio0_moth		"Polio at birth vaccination according to mother"
# ch_polio0_either	"Polio at birth vaccination according to either source"
# ch_polio1_card		"Polio 1st dose vaccination according to card"
# ch_polio1_moth		"Polio 1st dose vaccination according to mother"
# ch_polio1_either	"Polio 1st dose vaccination according to either source"
# ch_polio2_card		"Polio 2nd dose vaccination according to card"
# ch_polio2_moth		"Polio 2nd dose vaccination according to mother"
# ch_polio2_either	"Polio 2nd dose vaccination according to either source"
# ch_polio3_card		"Polio 3rd dose vaccination according to card"
# ch_polio3_moth		"Polio 3rd dose vaccination according to mother"
# ch_polio3_either	"Polio 3rd dose vaccination according to either source"
# 
# ch_pneumo1_card		"Pneumococcal 1st dose vaccination according to card"
# ch_pneumo1_moth		"Pneumococcal 1st dose vaccination according to mother"
# ch_pneumo1_either	"Pneumococcal 1st dose vaccination according to either source"
# ch_pneumo2_card		"Pneumococcal 2nd dose vaccination according to card"
# ch_pneumo2_moth		"Pneumococcal 2nd dose vaccination according to mother"
# ch_pneumo2_either	"Pneumococcal 2nd dose vaccination according to either source"
# ch_pneumo3_card		"Pneumococcal 3rd dose vaccination according to card"
# ch_pneumo3_moth		"Pneumococcal 3rd dose vaccination according to mother"
# ch_pneumo3_either	"Pneumococcal 3rd dose vaccination according to either source"
# 
# ch_rotav1_card		"Rotavirus 1st dose vaccination according to card"
# ch_rotav1_moth		"Rotavirus 1st dose vaccination according to mother"
# ch_rotav1_either	"Rotavirus 1st dose vaccination according to either source"
# ch_rotav2_card		"Rotavirus 2nd dose vaccination according to card"
# ch_rotav2_moth		"Rotavirus 2nd dose vaccination according to mother"
# ch_rotav2_either	"Rotavirus 2nd dose vaccination according to either source"
# ch_rotav3_card		"Rotavirus 3rd dose vaccination according to card"
# ch_rotav3_moth		"Rotavirus 3rd dose vaccination according to mother"
# ch_rotav3_either	"Rotavirus 3rd dose vaccination according to either source"
# 
# ch_meas_card		  "Measles vaccination according to card"
# ch_meas_moth		  "Measles vaccination according to mother"
# ch_meas_either		"Measles vaccination according to either source"
# 
# ch_allvac_card		"All basic vaccinations according to card"
# ch_allvac_moth		"All basic vaccinations according to mother"
# ch_allvac_either	"All basic vaccinations according to either source"
# 
# ch_novac_card		  "No vaccinations according to card"
# ch_novac_moth		  "No vaccinations according to mother"
# ch_novac_either		"No vaccinations according to either source"
# 
# ch_card_ever_had	"Ever had a vaccination card"
# ch_card_seen		  "Vaccination card seen"
# ----------------------------------------------------------------------------*/

# weight variable 
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

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
  
# *** Two age groups used for reporting. 
KRdata <- KRdata %>%
  mutate(agegroup = 
           case_when(
             age>=12 & age<=23 ~ 1,
             age>=24 & age<=35 ~ 2  )) %>%
  set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
  set_variable_labels(agegroup = "age group of child for vaccination")

# Selecting children
# Create subset of KRfile to select for children for VAC indicators
# Select agegroup 1 or agegroup 2
KRvac <- KRdata %>%
  subset(agegroup==1 & b5==1) # select age group and live children 
  
# *******************************************************************************

# Source of vaccination information. We need this variable to code vaccination indicators by source.
KRvac <- KRvac %>%
  mutate(source = 
           case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
  set_value_labels(source = c("card" = 1, "mother"=2)) %>%
  set_variable_labels(source = "source of vaccination information")

# *** BCG ***
# //BCG either source
KRvac <- KRvac %>%
  mutate(ch_bcg_either = 
           case_when(h2%in%c(1,2,3) ~ 1, h2%in%c(0,8)   ~ 0  )) %>%
  set_value_labels(ch_bcg_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_bcg_either = "BCG vaccination according to either source")

# //BCG mother's report
KRvac <- KRvac %>%
  mutate(ch_bcg_moth = 
           case_when(h2%in%c(1,2,3) & source==2 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_bcg_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_bcg_moth = "BCG vaccination according to mother")

# //BCG by card
KRvac <- KRvac %>%
  mutate(ch_bcg_card = 
           case_when(h2%in%c(1,2,3) & source==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_bcg_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_bcg_card = "BCG vaccination according to card")
 
# *** Pentavalent ***
# //DPT 1, 2, 3 either source
KRvac <- KRvac %>%
  mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
  mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
  mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
  mutate(dptsum = dpt1 + dpt2 + dpt3)
# This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history. 
# See DHS guide to statistics for further explanation
KRvac <- KRvac %>%
  mutate(ch_pent1_either = case_when(dptsum >=1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent1_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent1_either = "Pentavalent 1st dose vaccination according to either source") %>%
  mutate(ch_pent2_either = case_when(dptsum >=2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent2_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent2_either = "Pentavalent 2nd dose vaccination according to either source") %>%
  mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent3_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent3_either = "Pentavalent 3rd dose vaccination according to either source") 

# //DPT 1, 2, 3 mother's report
KRvac <- KRvac %>%
  mutate(ch_pent1_moth = case_when(dptsum >=1 & source==2~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent1_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent1_moth = "Pentavalent 1st dose vaccination according to mother") %>%
  mutate(ch_pent2_moth = case_when(dptsum >=2 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent2_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent2_moth = "Pentavalent 2nd dose vaccination according to mother") %>%
  mutate(ch_pent3_moth = case_when(dptsum >=3 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent3_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent3_moth = "Pentavalent 3rd dose vaccination according to mother") 

# //DPT 1, 2, 3 by card
KRvac <- KRvac %>%
  mutate(ch_pent1_card = case_when(dptsum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent1_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent1_card = "Pentavalent 1st dose vaccination according to card") %>%
  mutate(ch_pent2_card = case_when(dptsum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent2_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent2_card = "Pentavalent 2nd dose vaccination according to card") %>%
  mutate(ch_pent3_card = case_when(dptsum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent3_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent3_card = "Pentavalent 3rd dose vaccination according to card") 

# *** Polio ***
# //polio 0, 1, 2, 3 either source
KRvac <- KRvac %>%
  mutate(polio1 = case_when(h4%in%c(1,2,3) ~ 1, h4%in%c(0,8) ~ 0  )) %>%
  mutate(polio2 = case_when(h6%in%c(1,2,3) ~ 1, h6%in%c(0,8) ~ 0  )) %>%
  mutate(polio3 = case_when(h8%in%c(1,2,3) ~ 1, h8%in%c(0,8) ~ 0  )) %>%
  mutate(poliosum=polio1 + polio2 + polio3)
# This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history. 
# See DHS guide to statistics for further explanation
KRvac <- KRvac %>%
  mutate(ch_polio0_either = case_when(h0%in%c(1,2,3) ~ 1, h0%in%c(0,8) ~ 0 )) %>%
  set_value_labels(ch_polio0_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio0_either = "Polio at birth vaccination according to either source") %>%
  mutate(ch_polio1_either = case_when(poliosum >=1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio1_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio1_either = "Polio 1st dose vaccination according to either source") %>%
  mutate(ch_polio2_either = case_when(poliosum >=2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio2_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio2_either = "Polio 2nd dose vaccination according to either source") %>%
  mutate(ch_polio3_either = case_when(poliosum >=3 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio3_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio3_either = "Polio 3rd dose vaccination according to either source") 
 
# //polio 0, 1, 2, 3 mother's report
KRvac <- KRvac %>%
  mutate(ch_polio0_moth = case_when(h0%in%c(1,2,3) & source==2 ~ 1, TRUE ~ 0 )) %>%
  set_value_labels(ch_polio0_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio0_moth = "Polio at birth vaccination according to mother") %>%
  mutate(ch_polio1_moth = case_when(poliosum >=1 & source==2~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio1_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio1_moth = "Polio 1st dose vaccination according to mother") %>%
  mutate(ch_polio2_moth = case_when(poliosum >=2 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio2_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio2_moth = "Polio 2nd dose vaccination according to mother") %>%
  mutate(ch_polio3_moth = case_when(poliosum >=3 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio3_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio3_moth = "Polio 3rd dose vaccination according to mother") 

# //polio 0, 1, 2, 3 by card
KRvac <- KRvac %>%
  mutate(ch_polio0_card = case_when(h0%in%c(1,2,3) & source==1 ~ 1, TRUE ~ 0 )) %>%
  set_value_labels(ch_polio0_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio0_card = "Polio at birth vaccination according to card") %>%
  mutate(ch_polio1_card = case_when(poliosum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio1_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio1_card = "Polio 1st dose vaccination according to card") %>%
  mutate(ch_polio2_card = case_when(poliosum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio2_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio2_card = "Polio 2nd dose vaccination according to card") %>%
  mutate(ch_polio3_card = case_when(poliosum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio3_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio3_card = "Polio 3rd dose vaccination according to card") 

# *** Pneumococcal  ***
# //Pneumococcal 1, 2, 3 either source
# Some surveys that do not have information on this vaccine.
KRvac <- KRvac %>%
  mutate(Pneumo1 = case_when(h54%in%c(1,2,3) ~ 1, h54%in%c(0,8) ~ 0  )) %>%
  mutate(Pneumo2 = case_when(h55%in%c(1,2,3) ~ 1, h55%in%c(0,8) ~ 0  )) %>%
  mutate(Pneumo3 = case_when(h56%in%c(1,2,3) ~ 1, h56%in%c(0,8) ~ 0  )) %>%
  mutate(Pneumosum= Pneumo1+Pneumo2+Pneumo3)
# This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history. 
# See DHS guide to statistics for further explanation
KRvac <- KRvac %>%
  mutate(ch_pneumo1_either = case_when(Pneumosum >=1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo1_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo1_either = "Pneumococcal 1st dose vaccination according to either source") %>%
  mutate(ch_pneumo2_either = case_when(Pneumosum >=2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo2_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo2_either = "Pneumococcal 2nd dose vaccination according to either source") %>%
  mutate(ch_pneumo3_either = case_when(Pneumosum >=3 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo3_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo3_either = "Pneumococcal 3rd dose vaccination according to either source") 

# //Pneumococcal 1, 2, 3 mother's report
KRvac <- KRvac %>%
  mutate(ch_pneumo1_moth = case_when(Pneumosum >=1 & source==2~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo1_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo1_moth = "Pneumococcal 1st dose vaccination according to mother") %>%
  mutate(ch_pneumo2_moth = case_when(Pneumosum >=2 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo2_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo2_moth = "Pneumococcal 2nd dose vaccination according to mother") %>%
  mutate(ch_pneumo3_moth = case_when(Pneumosum >=3 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo3_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo3_moth = "Pneumococcal 3rd dose vaccination according to mother") 

# //Pneumococcal 1, 2, 3 by card
KRvac <- KRvac %>%
  mutate(ch_pneumo1_card = case_when(Pneumosum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo1_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo1_card = "Pneumococcal 1st dose vaccination according to card") %>%
  mutate(ch_pneumo2_card = case_when(Pneumosum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo2_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo2_card = "Pneumococcal 2nd dose vaccination according to card") %>%
  mutate(ch_pneumo3_card = case_when(Pneumosum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pneumo3_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pneumo3_card = "Pneumococcal 3rd dose vaccination according to card") 
 
# *** Rotavirus  ****
# //Rotavirus 1, 2, 3 either source
# Some surveys that do not have information on this vaccine.
KRvac <- KRvac %>%
  mutate(rotav1 = case_when(h57%in%c(1,2,3) ~ 1, h57%in%c(0,8) ~ 0  )) %>%
  mutate(rotav2 = case_when(h58%in%c(1,2,3) ~ 1, h58%in%c(0,8) ~ 0  )) %>%
  mutate(rotav3 = case_when(h59%in%c(1,2,3) ~ 1, h59%in%c(0,8) ~ 0  )) %>%
  mutate(rotavsum= rotav1+rotav2+rotav3)
# This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history. 
# See DHS guide to statistics for further explanation
KRvac <- KRvac %>%
  mutate(ch_rotav1_either = case_when(rotavsum >=1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav1_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav1_either = "Rotavirus 1st dose vaccination according to either source") %>%
  mutate(ch_rotav2_either = case_when(rotavsum >=2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav2_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav2_either = "Rotavirus 2nd dose vaccination according to either source") %>%
  mutate(ch_rotav3_either = case_when(rotavsum >=3 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav3_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav3_either = "Rotavirus 3rd dose vaccination according to either source") 

# //Rotavirus 1, 2, 3 mother's report
KRvac <- KRvac %>%
  mutate(ch_rotav1_moth = case_when(rotavsum >=1 & source==2~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav1_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav1_moth = "Rotavirus 1st dose vaccination according to mother") %>%
  mutate(ch_rotav2_moth = case_when(rotavsum >=2 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav2_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav2_moth = "Rotavirus 2nd dose vaccination according to mother") %>%
  mutate(ch_rotav3_moth = case_when(rotavsum >=3 & source==2 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav3_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav3_moth = "Rotavirus 3rd dose vaccination according to mother") 

# //Rotavirus 1, 2, 3 by card
KRvac <- KRvac %>%
  mutate(ch_rotav1_card = case_when(rotavsum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav1_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav1_card = "Rotavirus 1st dose vaccination according to card") %>%
  mutate(ch_rotav2_card = case_when(rotavsum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav2_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav2_card = "Rotavirus 2nd dose vaccination according to card") %>%
  mutate(ch_rotav3_card = case_when(rotavsum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_rotav3_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_rotav3_card = "Rotavirus 3rd dose vaccination according to card") 
 
# *** Measles ***
# //Measles either source
KRvac <- KRvac %>%
  mutate(ch_meas_either = 
           case_when(h9%in%c(1,2,3) ~ 1, h9%in%c(0,8)   ~ 0  )) %>%
  set_value_labels(ch_meas_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_meas_either = "Measles vaccination according to either source")

# //Measles mother's report
KRvac <- KRvac %>%
  mutate(ch_meas_moth = 
           case_when(h9%in%c(1,2,3) & source==2 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_meas_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_meas_moth = "Measles vaccination according to mother")

# //Measles by card
KRvac <- KRvac %>%
  mutate(ch_meas_card = 
           case_when(h9%in%c(1,2,3) & source==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_meas_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_meas_card = "Measles vaccination according to card")

# *** All vaccinations ***
KRvac <- KRvac %>%
  mutate(ch_allvac_either = 
           case_when(ch_bcg_either==1&ch_pent3_either==1&ch_polio3_either==1&ch_meas_either==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_allvac_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_allvac_either = "All basic vaccinations according to either source")

KRvac <- KRvac %>%
  mutate(ch_allvac_moth = 
           case_when(ch_bcg_either==1&ch_pent3_either==1&ch_polio3_either==1&ch_meas_either==1 & source==2 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_allvac_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_allvac_moth = "All basic vaccinations according to mother")

KRvac <- KRvac %>%
  mutate(ch_allvac_card = 
           case_when(ch_bcg_either==1&ch_pent3_either==1&ch_polio3_either==1&ch_meas_either==1 & source==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ch_allvac_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_allvac_card = "All basic vaccinations according to card")

# *** No vaccinations ***
KRvac <- KRvac %>%
  mutate(ch_novac_either = 
           case_when(ch_bcg_either==0&ch_pent1_either==0&ch_pent2_either==0&ch_pent3_either==0& 
                     ch_polio0_either==0&ch_polio1_either==0&ch_polio2_either==0&ch_polio3_either==0&
                     ch_meas_either==0 ~ 1, 
                     TRUE ~ 0)) %>%
  set_value_labels(ch_novac_either = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_novac_either = "No vaccinations according to either source")

KRvac <- KRvac %>%
  mutate(ch_novac_moth = 
           case_when(ch_bcg_either==0&ch_pent1_either==0&ch_pent2_either==0&ch_pent3_either==0& 
                       ch_polio0_either==0&ch_polio1_either==0&ch_polio2_either==0&ch_polio3_either==0&
                       ch_meas_either==0& source==2 ~ 1, 
                       TRUE ~ 0)) %>%  
  set_value_labels(ch_novac_moth = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_novac_moth = "No vaccinations according to mother")

KRvac <- KRvac %>%
  mutate(ch_novac_card = 
           case_when(ch_bcg_either==0&ch_pent1_either==0&ch_pent2_either==0&ch_pent3_either==0& 
                       ch_polio0_either==0&ch_polio1_either==0&ch_polio2_either==0&ch_polio3_either==0&
                       ch_meas_either==0& source==1 ~ 1, 
                       TRUE ~ 0)) %>% 
  set_value_labels(ch_novac_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_novac_card = "No vaccinations according to card")
# 
# *** vaccination card possession ***
KRvac <- KRvac %>%
  mutate(ch_card_ever_had = 
           case_when(h1%in%c(1,2,3) ~ 1, TRUE  ~ 0  )) %>%
  set_value_labels(ch_card_ever_had = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_card_ever_had = "Ever had a vaccination card")

KRvac <- KRvac %>%
  mutate(ch_card_seen = 
           case_when(h1==1 ~ 1, TRUE  ~ 0  )) %>%
  set_value_labels(ch_card_seen = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_card_seen = "Vaccination card seen")
