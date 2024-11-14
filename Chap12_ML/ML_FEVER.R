# ******************************************************************************
# Program: 		ML_FEVER.R
# Purpose: 		Code indicators on fever, fever care-seeking, and antimalarial drugs  
# Data inputs: 		KR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: November 14, 2024 by Mahmoud Elkasabi
# Notes:	There are similarities between the fever code in this do file and the ARI/Fever code for Chapter 10. 
# Several indicators (on care and specific antimalarial drugs) are country specific. Please see notes in the code.
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_fever			"Fever symptoms in the 2 weeks before the survey"
# ml_fev_care			"Advice or treatment sought for fever symptoms"
# ml_fev_care_day		"Advice or treatment sought for fever symptoms on the same or next day"
# ml_stick			"Child received heel or finger stick"

# ml_fev_govh			"Fever treatment sought from government hospital among children with fever"
# ml_fev_govh_trt		"Fever treatment sought from government hospital among children with fever that sought treatment"
# ml_fev_govcent		"Fever treatment sought from government health center among children with fever"
# ml_fev_govcent_trt	"Fever treatment sought from government health center among children with fever that sought treatment"
# ml_fev_pclinc		"Fever treatment sought from private hospital/clinic among children with fever"
# ml_fev_pclinc_trt	"Fever treatment sought from private hospital/clinic  among children with fever that sought treatment"
# ml_fev_pdoc			"Fever treatment sought from private doctor among children with fever"
# ml_fev_pdoc_trt		"Fever treatment sought from private doctor among children with fever that sought treatment"
# ml_fev_pharm		"Fever treatment sought from a pharmacy among children with fever"
# ml_fev_pharm_trt	"Fever treatment sought from a pharmacy among children with fever that sought treatment"

# ml_antimal			"Child took any antimalarial"
# ml_act				"Child took an ACT"
# ml_sp_fan			"Child took SP/Fansider"
# ml_chloro			"Child took Chloroquine"
# ml_amodia			"Child took Amodiaquine"
# ml_quin_pill		"Child took Quinine pills"
# ml_quin_inj			"Child took Quinine injection or IV"
# ml_artes_rec		"Child took Artesunate rectal"
# ml_artes_inj		"Child took Artesunate injection or intravenous"
# ml_antimal_other	"Child took other antimalarial"
# -----------------------------------------------------------------------------#

# Fever and care-seeking 
# Fever
KRdata <- KRdata %>%
  mutate(ml_fever = case_when(
    h22==1  ~ 1,
    h22!=1  ~ 0,
    b5==0  ~ 99),
    ml_fever = set_label(ml_fever, label = "Fever symptoms in the 2 weeks before the surveyvs"))%>%
  replace_with_na(replace = list(ml_fever = c(99)))

# this is country specific and the footnote for the final table needs to be checked to see what sources are included. 
# the code below only excludes traditional practitioner (h32t). Some surveys also exclude pharmacies (h32k), shop (h32s) or other sources.
# In some surveys traditional practitioner is h32w. Please check the data file using h32*
KRdata <- KRdata %>%
  mutate(ml_fev_care = case_when(
    (ml_fever==1 &  b5==1) & (h32a == 1 | h32b == 1 | h32c == 1 | h32d == 1 | h32e == 1 | h32f == 1 |
                                h32g == 1 | h32h == 1 | h32i == 1 | h32j == 1 | h32k == 1 | h32l == 1 |
                                h32m == 1 | h32n == 1 | h32o == 1 | h32p == 1 | h32q == 1 | h32r == 1 |
                                h32s == 1 |             h32u == 1 | h32v == 1 | h32w == 1 | h32x == 1 )  ~ 1 ,
    b5==1 & ml_fever==1 ~ 0)) %>%
  set_value_labels(ml_fev_care = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ml_fev_care = "Advice or treatment sought for fever symptoms")

# Fever care-seeking same or next day
KRdata <- KRdata %>%
  mutate(ml_fev_care_day = case_when(
    ml_fever==1 & !(ml_fev_care==1 & h46b<2) ~ 0,
    ml_fever==1 & ml_fev_care==1 & h46b<2 ~ 1,
    b5==0  ~ 99),
    ml_fev_care_day = set_label(ml_fev_care_day, label = "Advice or treatment sought for fever symptoms on the same or next day"))%>%
  replace_with_na(replace = list(ml_fev_care_day = c(99)))

# Child with fever received heel or finger stick 
KRdata <- KRdata %>%
  mutate(ml_stick = case_when(
    h47!=1 & ml_fever==1 ~ 0,
    h47==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99),
    ml_stick = set_label(ml_stick, label = "Child received heel or finger stick"))%>%
  replace_with_na(replace = list(ml_stick = c(99)))

### Fever treatment by source 
# Two population bases: 1. among children with fever symptoms, 2. among children with fever symptoms that sought treatment
# This is country specific and needs to be checked to produce the specific source of interest. 
# Some sources are coded below and the same logic can be used to code other sources. h32a-z indicates the source.

# Fever treatment in government hospital
KRdata <- KRdata %>%
  mutate(ml_fev_govh = case_when(
    h32a!=1 & ml_fever==1 ~ 0,
    h32a==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_govh = set_label(ml_fev_govh, label = "Fever treatment sought from government hospital among children with fever"))%>%
  replace_with_na(replace = list(ml_fev_govh = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_govh_trt = case_when(
    h32a!=1 & ml_fev_care==1 ~ 0,
    h32a==1 & ml_fev_care==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_govh_trt = set_label(ml_fev_govh_trt, label = "Fever treatment sought from government hospital among children with fever that sought treatment"))%>%
  replace_with_na(replace = list(ml_fev_govh_trt = c(99)))

# Fever treatment in government health center
KRdata <- KRdata %>%
  mutate(ml_fev_govcent = case_when(
    h32b!=1 & ml_fever==1 ~ 0,
    h32b==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_govcent = set_label(ml_fev_govcent, label = "Fever treatment sought from government health center among children with fever"))%>%
  replace_with_na(replace = list(ml_fev_govcent = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_govcent_trt = case_when(
    h32b!=1 & ml_fev_care==1 ~ 0,
    h32b==1 & ml_fev_care==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_govcent_trt = set_label(ml_fev_govcent_trt, label = "Fever treatment sought from government health center among children with fever that sought treatment"))%>%
  replace_with_na(replace = list(ml_fev_govcent_trt = c(99)))

# Fever treatment from a private hospital/clinic
KRdata <- KRdata %>%
  mutate(ml_fev_pclinc = case_when(
    h32j!=1 & ml_fever==1 ~ 0,
    h32j==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_pclinc = set_label(ml_fev_pclinc, label = "Fever treatment sought from private hospital/clinic among children with fever"))%>%
  replace_with_na(replace = list(ml_fev_pclinc = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_pclinc_trt = case_when(
    h32j!=1 & ml_fev_care==1 ~ 0,
    h32j==1 & ml_fev_care==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_pclinc_trt = set_label(ml_fev_pclinc_trt, label = "Fever treatment sought from private hospital/clinic  among children with fever that sought treatment"))%>%
  replace_with_na(replace = list(ml_fev_pclinc_trt = c(99)))

# Fever treatment from a private doctor
KRdata <- KRdata %>%
  mutate(ml_fev_pdoc = case_when(
    h32l!=1 & ml_fever==1 ~ 0,
    h32l==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_pdoc = set_label(ml_fev_pdoc, label = "Fever treatment sought from private doctor among children with fever"))%>%
  replace_with_na(replace = list(ml_fev_pdoc = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_pdoc_trt = case_when(
    h32l!=1 & ml_fev_care==1 ~ 0,
    h32l==1 & ml_fev_care==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_pdoc_trt = set_label(ml_fev_pdoc_trt, label = "Fever treatment sought from private doctor among children with fever that sought treatment"))%>%
  replace_with_na(replace = list(ml_fev_pdoc_trt = c(99)))

# Fever treatment from a pharmacy
KRdata <- KRdata %>%
  mutate(ml_fev_pharm = case_when(
    h32k!=1 & ml_fever==1 ~ 0,
    h32k==1 & ml_fever==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_pharm = set_label(ml_fev_pharm, label = "Fever treatment sought from a pharmacy among children with fever"))%>%
  replace_with_na(replace = list(ml_fev_pharm = c(99)))

KRdata <- KRdata %>%
  mutate(ml_fev_pharm_trt = case_when(
    h32k!=1 & ml_fev_care==1 ~ 0,
    h32k==1 & ml_fev_care==1 ~ 1,
    b5==0  ~ 99),
    ml_fev_pharm_trt = set_label(ml_fev_pharm_trt, label = "Fever treatment sought from a pharmacy among children with fever that sought treatment"))%>%
  replace_with_na(replace = list(ml_fev_pharm_trt = c(99)))


### Antimalarial drugs
# Child with fever in past 2 weeks took any antimalarial
# This may need to be updated according to country specifications. 
# There may be additional survey-specific antimalarials in ml13f and ml13g or "s" variables. For instance in Ghana 2016 MIS there is s412f, s412g, s412h. Please search the date file.
# Also some drugs may be grouped. Please check final report and data files and adjust the code accordingly.

KRdata <- KRdata %>%
  mutate(ml_antimal = case_when(
    ml_fever==1 & !(ml13a==1|ml13aa==1|ml13ab==1|ml13b==1|ml13c==1|ml13d==1|ml13da==1|ml13e==1|ml13f==1|ml13g==1|ml13h==1) ~ 0,
    ml_fever==1 & (ml13a==1|ml13aa==1|ml13ab==1|ml13b==1|ml13c==1|ml13d==1|ml13da==1|ml13e==1|ml13f==1|ml13g==1|ml13h==1) ~ 1,
    b5==0  ~ 99),
    ml_antimal = set_label(ml_antimal, label = "Child took any antimalarial"))%>%
  replace_with_na(replace = list(ml_antimal = c(99)))

# The antimalarial durg indicators below are among children with fever symptoms in the 2 weeks preceding the survey who took any antimalarial medication.	
# Child with fever in past 2 weeks took an ACT
KRdata <- KRdata %>%
  mutate(ml_act = case_when(
    ml_fever==1 & ml_antimal==1 & ml13e!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13e==1 ~ 1,
    b5==0  ~ 99),
    ml_act = set_label(ml_act, label = "Child took an ACT"))%>%
  replace_with_na(replace = list(ml_act = c(99)))

# Child with fever in past 2 weeks took SP/Fansider
KRdata <- KRdata %>%
  mutate(ml_sp_fan = case_when(
    ml_fever==1 & ml_antimal==1 & ml13a!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13a==1 ~ 1,
    b5==0  ~ 99),
    ml_sp_fan = set_label(ml_sp_fan, label = "Child took SP/Fansider"))%>%
  replace_with_na(replace = list(ml_sp_fan = c(99)))

# Child with fever in past 2 weeks took Chloroquine
KRdata <- KRdata %>%
  mutate(ml_chloro = case_when(
    ml_fever==1 & ml_antimal==1 & ml13b!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13b==1 ~ 1,
    b5==0  ~ 99),
    ml_chloro = set_label(ml_chloro, label = "Child took Chloroquine"))%>%
  replace_with_na(replace = list(ml_chloro = c(99)))

# Child with fever in past 2 weeks took Amodiaquine
KRdata <- KRdata %>%
  mutate(ml_amodia = case_when(
    ml_fever==1 & ml_antimal==1 & ml13c!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13c==1 ~ 1,
    b5==0  ~ 99),
    ml_amodia = set_label(ml_amodia, label = "Child took Amodiaquine"))%>%
  replace_with_na(replace = list(ml_amodia = c(99)))

# Child with fever in past 2 weeks took Quinine pills
KRdata <- KRdata %>%
  mutate(ml_quin_pill = case_when(
    ml_fever==1 & ml_antimal==1 & ml13d!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13d==1 ~ 1,
    b5==0  ~ 99),
    ml_quin_pill = set_label(ml_quin_pill, label = "Child took Quinine pills"))%>%
  replace_with_na(replace = list(ml_quin_pill = c(99)))

# Child with fever in past 2 weeks took Quinine injection or intravenous (IV)  
KRdata <- KRdata %>%
  mutate(ml_quin_inj = case_when(
    ml_fever==1 & ml_antimal==1 & ml13da!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13da==1 ~ 1,
    b5==0  ~ 99),
    ml_quin_inj = set_label(ml_quin_inj, label = "Child took Quinine injection or IV"))%>%
  replace_with_na(replace = list(ml_quin_inj = c(99)))

# Child with fever in past 2 weeks took Artesunate rectal
KRdata <- KRdata %>%
  mutate(ml_artes_rec = case_when(
    ml_fever==1 & ml_antimal==1 & ml13aa!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13aa==1 ~ 1,
    b5==0  ~ 99),
    ml_artes_rec = set_label(ml_artes_rec, label = "Child took Artesunate rectal"))%>%
  replace_with_na(replace = list(ml_artes_rec = c(99)))

# Child with fever in past 2 weeks took Artesunate injection or intravenous
KRdata <- KRdata %>%
  mutate(ml_artes_inj = case_when(
    ml_fever==1 & ml_antimal==1 & ml13ab!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13ab==1 ~ 1,
    b5==0  ~ 99),
    ml_artes_inj = set_label(ml_artes_inj, label = "Child took Artesunate injection or intravenous"))%>%
  replace_with_na(replace = list(ml_artes_inj = c(99)))

# Child with fever in past 2 weeks took other antimalarial
KRdata <- KRdata %>%
  mutate(ml_antimal_other = case_when(
    ml_fever==1 & ml_antimal==1 & ml13h!=1 ~ 0,
    ml_fever==1 & ml_antimal==1 & ml13h==1 ~ 1,
    b5==0  ~ 99),
    ml_antimal_other = set_label(ml_antimal_other, label = "Child took other antimalarial"))%>%
  replace_with_na(replace = list(ml_antimal_other = c(99)))
