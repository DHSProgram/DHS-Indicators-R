# ******************************************************************************
# Program: 		ML_FEVER.R - DHS8 update
# Purpose: 		Code indicators on fever, fever care-seeking, and antimalarial drugs  
# Data inputs: 		KR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: August 16, 2024 by Courtney Allen for DHS8 update
#
# Notes:	There are similarities between the fever code in this do file and the ARI/Fever code for Chapter 10. 
# Several indicators (on care and specific antimalarial drugs) are country specific. Please see notes in the code.
# ******************************************************************************
# -----------------------------------------------------------------------------#

# # Variables created in this file:
# ml_fever			  "Fever symptoms in the 2 weeks before the survey"
# ml_fev_care			"Advice or treatment sought for fever symptoms"
# ml_fev_care_day	"Advice or treatment sought for fever symptoms on the same or next day"
# ml_stick			  "Child received heel or finger stick"
# ml_told_malaria	"Diagnosed with malaria by a healthcare provider" - NEW Indicator in DHS8

# ml_fev_govh			  "Fever treatment sought from government hospital among children with fever"
# ml_fev_govh_trt		"Fever treatment sought from government hospital among children with fever that sought treatment"
# ml_fev_govcent		"Fever treatment sought from government health center among children with fever"
# ml_fev_govcent_trt	"Fever treatment sought from government health center among children with fever that sought treatment"
# ml_fev_phosp	  	"Fever treatment sought from private hospital/clinic among children with fever"
# ml_fev_phosp_trt	"Fever treatment sought from private hospital/clinic  among children with fever that sought treatment"
# ml_fev_pdoc			  "Fever treatment sought from private doctor among children with fever"
# ml_fev_pdoc_trt		"Fever treatment sought from private doctor among children with fever that sought treatment"
# ml_fev_pharm		  "Fever treatment sought from a pharmacy among children with fever"
# ml_fev_pharm_trt	"Fever treatment sought from a pharmacy among children with fever that sought treatment"

# ml_antimal		"Child took any antimalarial"
# ml_act				"Child took an ACT"
# ml_sp_fan			"Child took SP/Fansider"
# ml_chloro			"Child took Chloroquine"
# ml_amodia			"Child took Amodiaquine"
# ml_quin_pill	"Child took Quinine pills"
# ml_quin_inj		"Child took Quinine injection or IV"
# ml_artes_rec	"Child took Artesunate rectal"
# ml_artes_inj	"Child took Artesunate injection or intravenous"
# ml_antimal_other	"Child took other antimalarial"
# -----------------------------------------------------------------------------#

# Fever and care-seeking -------
# Fever
KRdata <- KRdata %>%
  mutate(ml_fever = case_when(
    h22==1  ~ 1,
    h22!=1  ~ 0,
    b5==0  ~ 99)) %>%
  set_value_labels(ml_fever = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_fever = "Fever symptoms in the 2 weeks before the surveys") %>%
  replace_with_na(replace = list(ml_fever = c(99)))

# this is country specific and the footnote for the final table needs to be checked to see what sources are included. 
# the code below only excludes traditional practitioner (h32t). Some surveys also exclude pharmacies (h32k), shop (h32s) or other sources.
# In some surveys traditional practitioner is h32w. Please check the data file using h32*

KRdata <- KRdata %>%
  mutate(ml_fev_care = case_when(
    ml_fever==1 & (h32a==1| h32b==1| h32c==1| h32d==1| h32e==1| h32f==1| h32g==1| 
                   h32h==1| h32i==1| h32j==1| h32k==1| h32l==1| h32m==1| h32n==1| 
                     h32na==1| h32nb==1 | h32nc==1 | h32nd==1 | h32ne==1 | 
                     h32o==1| h32p==1| h32q==1| h32r==1| h32s==1| h32u==1| 
                     h32v==1| h32w==1| h32x==1) ~ 1,
    ml_fever==1 ~ 0,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_care = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_care =  "Advice or treatment sought for fever symptoms") %>%
  replace_with_na(replace = list(ml_fev_care = c(99)))

# Fever care-seeking same or next day
KRdata <- KRdata %>%
  mutate(ml_fev_care_day = case_when(
    ml_fever==1 & ml_fev_care==1 & h46b<2 ~ 1,
    ml_fever==1 & ml_fev_care==1 & h46b>=2 ~ 0,
    ml_fever==1 & !(ml_fev_care==1) ~ 0,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_care_day = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_care_day = "Advice or treatment sought for fever symptoms on the same or next day") %>%
  replace_with_na(replace = list(ml_fev_care_day = c(99)))

# Child with fever received heel or finger stick 
KRdata <- KRdata %>%
  mutate(ml_stick = case_when(
    h47!=1 & ml_fever==1 ~ 0,
    h47==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_stick = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_stick =  "Child received heel or finger stick") %>%
  replace_with_na(replace = list(ml_stick = c(99)))

# Diagnosed with malaria by a healthcare provider - NEW Indicator in DHS8
KRdata <- KRdata %>%
  mutate(ml_told_malaria = case_when(
    h71==1 ~ 1,
    ml_fever==1 ~ 0,
    b5== 0 ~ NA))  %>%
      set_value_labels(ml_told_malaria = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_told_malaria =  "Diagnosed with malaria by a healthcare provider")


### Fever treatment by source  ------
# Two population bases: 1. among children with fever symptoms, 2. among children with fever symptoms that sought treatment
# This is country specific and needs to be checked to produce the specific source of interest. 
# Some sources are coded below and the same logic can be used to code other sources. h32a-z indicates the source.

## PUBLIC SECTOR

# Fever treatment in government hospital
KRdata <- KRdata %>%
  mutate(ml_fev_govh = case_when(
    h32a!=1 & ml_fever==1 ~ 0,
    h32a==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_govh = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_govh = "Fever treatment sought from govt hospital among children with fever") %>%
  replace_with_na(replace = list(ml_fev_govh = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_govh_trt = case_when(
    h32a==1 & ml_fever==1 ~ 1,
    ml_fever==1 & h32y==0 ~ 0,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_govh_trt = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_govh_trt =  "Fever treatment sought from govt hospital among children with fever that sought treatment") %>%
  replace_with_na(replace = list(ml_fev_govh_trt = c(99)))

# Fever treatment in government health center
KRdata <- KRdata %>%
  mutate(ml_fev_govcent = case_when(
    h32b!=1 & ml_fever==1 ~ 0,
    h32b==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_govcent = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_govcent =  "Fever treatment sought from govt health center among children with fever") %>%
  replace_with_na(replace = list(ml_fev_govcent = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_govcent_trt = case_when(
    h32b==1 & ml_fever==1 ~ 1,
    ml_fever==1 & h32y==0 ~ 0,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_govcent_trt = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_govcent_trt =  "Fever treatment sought from govt health center among children with fever who sought treatment") %>%
  replace_with_na(replace = list(ml_fev_govcent_trt = c(99)))

## PRIVATE SECTOR
    
# Fever treatment from a private hospital
KRdata <- KRdata %>%
  mutate(ml_fev_phosp = case_when(
    h32j!=1 & ml_fever==1 ~ 0,
    h32j==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_phosp = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_phosp =  "Fever treatment sought from private hospital among children with fever") %>%
  replace_with_na(replace = list(ml_fev_phosp = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_phosp_trt = case_when(
    h32j==1 & ml_fever==1 ~ 1,
    ml_fever==1 & h32y==0 ~ 0,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_phosp_trt = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_fev_phosp_trt = "Fever treatment sought from private hospital among children with fever that sought treatment") %>%
  replace_with_na(replace = list(ml_fev_phosp_trt = c(99)))

# Fever treatment from a pharmacy
KRdata <- KRdata %>%
  mutate(ml_fev_pharm = case_when(
    h32k!=1 & ml_fever==1 ~ 0,
    h32k==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_pharm = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_pharm =  "Fever treatment sought from a pharmacy among children with fever") %>%
  replace_with_na(replace = list(ml_fev_pharm = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_pharm_trt = case_when(
    h32k==1 & ml_fever==1 ~ 1,
    ml_fever==1 & h32y==0 ~ 0,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_pharm_trt = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_pharm_trt =  "Fever treatment sought from a pharmacy among children with fever that sought treatment") %>%
  replace_with_na(replace = list(ml_fev_pharm_trt = c(99)))

# Fever treatment from a private doctor
KRdata <- KRdata %>%
  mutate(ml_fev_pdoc = case_when(
    h32l!=1 & ml_fever==1 ~ 0,
    h32l==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_fev_pdoc = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_fev_pdoc =  "Fever treatment sought from private doctor among children with fever") %>%
  replace_with_na(replace = list(ml_fev_pdoc = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_pdoc_trt = case_when(
    h32l==1 & ml_fever==1 ~ 1,
    ml_fever==1 & h32y==0 ~ 0,
    b5==0  ~ 99)) %>%
  set_value_labels(ml_fev_pdoc_trt = c("Yes"= 1, "No" = 0))  %>%
  set_variable_labels(ml_fev_pdoc_trt =  "Fever treatment sought from private doctor among children with fever that sought treatment") %>%
  replace_with_na(replace = list(ml_fev_pdoc_trt = c(99)))


### Antimalarial drugs -----
# Child with fever in past 2 weeks took any antimalarial
# This may need to be updated according to country specifications. 
# There may be additional survey-specific antimalarials in h37f and h37g or "s" variables. For instance in Ghana 2016 MIS there is s412f, s412g, s412h. Please search the date file.
# Also some drugs may be grouped. Please check final report and data files and adjust the code accordingly.

KRdata <- KRdata %>%
  mutate(ml_antimal = case_when(
    ml_fever==1 & !(h37a==1| h37ab==1|h37b==1|h37c==1|h37d==1|h37da==1|h37e==1|h37f==1|h37g==1|h37h==1) ~ 0,
    ml_fever==1 & (h37a==1|h37aa==1|h37ab==1|h37b==1|h37c==1|h37d==1|h37da==1|h37e==1|h37f==1|h37g==1|h37h==1) ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_antimal = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_antimal =  "Child took any antimalarial") %>%
  replace_with_na(replace = list(ml_antimal = c(99)))

# The antimalarial drug indicators below are among children with fever symptoms in the 2 weeks preceding the survey who took any antimalarial medication.	
# Child with fever in past 2 weeks took an ACT
KRdata <- KRdata %>%
  mutate(ml_act = case_when(
    ml_fever==1 & ml_antimal==1 & h37e!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37e==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_act = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_act =  "Child took an ACT") %>%
  replace_with_na(replace = list(ml_act = c(99)))

# Child with fever in past 2 weeks took SP/Fansider
KRdata <- KRdata %>%
  mutate(ml_sp_fan = case_when(
    ml_fever==1 & ml_antimal==1 & h37a!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37a==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_sp_fan = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_sp_fan =  "Child took SP/Fansider") %>%
  replace_with_na(replace = list(ml_sp_fan = c(99)))

# Child with fever in past 2 weeks took Chloroquine
KRdata <- KRdata %>%
  mutate(ml_chloro = case_when(
    ml_fever==1 & ml_antimal==1 & h37b!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37b==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_chloro = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_chloro =  "Child took Chloroquine") %>%
  replace_with_na(replace = list(ml_chloro = c(99)))

# Child with fever in past 2 weeks took Amodiaquine
KRdata <- KRdata %>%
  mutate(ml_amodia = case_when(
    ml_fever==1 & ml_antimal==1 & h37c!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37c==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_amodia = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_amodia =  "Child took Amodiaquine") %>%
  replace_with_na(replace = list(ml_amodia = c(99)))

# Child with fever in past 2 weeks took Quinine pills
KRdata <- KRdata %>%
  mutate(ml_quin_pill = case_when(
    ml_fever==1 & ml_antimal==1 & h37d!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37d==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_quin_pill = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_quin_pill =  "Child took Quinine pills") %>%
  replace_with_na(replace = list(ml_quin_pill = c(99)))

# Child with fever in past 2 weeks took Quinine injection or intravenous (IV)  
KRdata <- KRdata %>%
  mutate(ml_quin_inj = case_when(
    ml_fever==1 & ml_antimal==1 & h37da!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37da==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_quin_inj = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_quin_inj =  "Child took Quinine injection or IV") %>%
  replace_with_na(replace = list(ml_quin_inj = c(99)))

# Child with fever in past 2 weeks took Artesunate rectal
KRdata <- KRdata %>%
  mutate(ml_artes_rec = case_when(
    ml_fever==1 & ml_antimal==1 & h37aa!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37aa==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_artes_rec = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_artes_rec =  "Child took Artesunate rectal") %>%
  replace_with_na(replace = list(ml_artes_rec = c(99)))

# Child with fever in past 2 weeks took Artesunate injection or intravenous
KRdata <- KRdata %>%
  mutate(ml_artes_inj = case_when(
    ml_fever==1 & ml_antimal==1 & h37ab!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37ab==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_artes_inj = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_artes_inj =  "Child took Artesunate injection or intravenous") %>%
  replace_with_na(replace = list(ml_artes_inj = c(99)))

# Child with fever in past 2 weeks took other antimalarial
KRdata <- KRdata %>%
  mutate(ml_antimal_other = case_when(
    ml_fever==1 & ml_antimal==1 & h37h!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & h37h==1 ~ 1,
    b5==0  ~ 99)) %>%
      set_value_labels(ml_antimal_other = c("Yes"= 1, "No" = 0)) %>%
      set_variable_labels(ml_antimal_other =  "Child took other antimalarial") %>%
  replace_with_na(replace = list(ml_antimal_other = c(99)))

