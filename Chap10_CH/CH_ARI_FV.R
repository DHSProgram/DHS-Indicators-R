# /*****************************************************************************************************
# Program: 			  CH_ARI_FV.R
# Purpose: 			  Code ARI and fever variables.
# Data inputs: 		KR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: July 29, 2022 by Shireen Assaf 
# Notes:          Check notes for ARI/fever care and treatment variables which are country specific. 		
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_ari				      "ARI symptoms in the 2 weeks before the survey"
# ch_ari_care			    "Advice or treatment sought for ARI symptoms"
# ch_ari_care_day		  "Advice or treatment sought for ARI symptoms on the same or next day"
# 
# ch_ari_govh			    "ARI treatment sought from government hospital among children with ARI"
# ch_ari_govh_trt		  "ARI treatment sought from government hospital among children with ARI that sought treatment"
# ch_ari_govcent 		  "ARI treatment sought from government health center among children with ARI"
# ch_ari_govcent_trt 	"ARI treatment sought from government health center among children with ARI that sought treatment"
# ch_ari_pclinc 		  "ARI treatment sought from private hospital/clinic among children with ARI"
# ch_ari_pclinc_trt 	"ARI treatment sought from private hospital/clinic  among children with ARI that sought treatment"
# ch_ari_pdoc			    "ARI treatment sought from private doctor among children with ARI"
# ch_ari_pdoc_trt		  "ARI treatment sought from private doctor among children with ARI that sought treatment"
# ch_ari_pharm		    "ARI treatment sought from pharmacy among children with ARI"
# ch_ari_pharm_trt	  "ARI treatment sought from pharmacy among children with ARI that sought treatment"
# 
# ch_fever			      "Fever symptoms in the 2 weeks before the survey"
# ch_fev_care			    "Advice or treatment sought for fever symptoms"
# ch_fev_care_day		  "Advice or treatment sought for ARI symptoms on the same or next day"
# ch_fev_antib		    "Antibiotics taken for fever symptoms"
# ----------------------------------------------------------------------------*/

# weight variable 
 KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# ** ARI indicators ***
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

# //ARI symptoms
# ARI definition differs by survey according to whether h31c is included or not
if ("TRUE" %in% (!("h31c" %in% names(KRdata))))
  KRdata [[paste("h31c")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$h31c)))
{ h31c_included <- 0} else { h31c_included <- 1}

if (h31c_included==1) {
  KRdata <- KRdata %>%
    mutate(ch_ari = 
             case_when(
               h31b==1 & (h31c==1 | h31c==3) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
} else {
  KRdata <- KRdata %>%
    mutate(ch_ari = 
             case_when(
               h31b==1 & (h31==2) & b5==1 ~ 1 ,
               b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
}


# survey specific changes
# if survey is "IAKR23" or "PHKR31" 
# KRdata <- KRdata %>%
#   mutate(ch_ari = 
#            case_when(
#              h31b==1 & (h31==2|h31==1) ~ 1 ,
#              b5==1 ~ 0 )) %>%
#   set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
#   set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
# 

# //ARI care-seeking
# This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
# The code below only excludes traditional practitioner (usually h32t). 
# The variable for traditional healer may be different for different surveys (you can check this by checking all the h32* variables). 
# Some surveys also exclude pharmacies, shop, or other sources.
# If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
# h32k from the code below.

KRdata <- KRdata %>%
  mutate(ch_ari_care =
           case_when(
             (ch_ari==1 &  b5==1) & 
             (h32a == 1 | h32b == 1 | h32c == 1 | h32d == 1 | h32e == 1 | h32f == 1 |
              h32g == 1 | h32h == 1 | h32i == 1 | h32j == 1 | h32k == 1 | h32l == 1 |
              h32m == 1 | h32n == 1 | h32o == 1 | h32p == 1 | h32q == 1 | h32r == 1 |
              h32s == 1 |             h32u == 1 | h32v == 1 | h32w == 1 | h32x == 1 )  ~ 1 ,
              b5==1 & ch_ari==1 ~ 0)) %>%
  set_value_labels(ch_ari_care = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_care = "Advice or treatment sought for ARI symptoms")

# //ARI care-seeking same or next day
# some surveys do not have the variable needed to code this indicator which is h46b

if ("TRUE" %in% (!("h46b" %in% names(KRdata))))
  KRdata [[paste("h46b")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$h46b)))
{h46b_included <- 0} else {h46b_included <- 1}

if (h46b_included==1) {
  KRdata <- KRdata %>%
    mutate(ch_ari_care_day = 
             case_when(
               ch_ari==1 & h46b<2 & b5==1 ~ 1 ,
               ch_ari==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_care_day = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_care_day = "Advice or treatment sought for ARI symptoms on the same or next day")
} else {
  KRdata <- KRdata %>%
    mutate(ch_ari_care_day = NA)
}

# *** ARI treatment by source *** 
# Two population bases: 1. among children with ARI symptoms, 2. among children with ARI symptoms that sought treatment
# This is country specific and needs to be checked to produce the specific source of interest. 
# Some sources are coded below and the same logic can be used to code other sources. h32a-z indicates the source.

# //ARI treatment in government hospital
KRdata <- KRdata %>%
  mutate(ch_ari_govh = 
           case_when(
             ch_ari==1 & h32a==1 & b5==1 ~ 1 ,
             ch_ari==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_govh = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_govh = "ARI treatment sought from government hospital among children with ARI")

KRdata <- KRdata %>%
  mutate(ch_ari_govh_trt = 
           case_when(
             ch_ari_care==1 & h32a==1 & b5==1 ~ 1 ,
             ch_ari_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_govh_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_govh_trt = "ARI treatment sought from government hospital among children with ARI that sought treatment")

# //ARI treamtment in government health center
KRdata <- KRdata %>%
  mutate(ch_ari_govcent = 
           case_when(
             ch_ari==1 & h32b==1 & b5==1 ~ 1 ,
             ch_ari==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_govcent = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_govcent = "ARI treatment sought from government health center among children with ARI")

KRdata <- KRdata %>%
  mutate(ch_ari_govcent_trt = 
           case_when(
             ch_ari_care==1 & h32b==1 & b5==1 ~ 1 ,
             ch_ari_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_govcent_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_govcent_trt = "ARI treatment sought from government health center among children with ARI that sought treatment")

# //ARI treatment from a private hospital/clinic
KRdata <- KRdata %>%
  mutate(ch_ari_pclinc = 
           case_when(
             ch_ari==1 & h32j==1 & b5==1 ~ 1 ,
             ch_ari==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_pclinc = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_pclinc = "ARI treatment sought from private hospital/clinic among children with ARI")

KRdata <- KRdata %>%
  mutate(ch_ari_pclinc_trt = 
           case_when(
             ch_ari_care==1 & h32j==1 & b5==1 ~ 1 ,
             ch_ari_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_pclinc_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_pclinc_trt = "ARI treatment sought from private hospital/clinic among children with ARI that sought treatment")

# //ARI treatment from a private doctor
KRdata <- KRdata %>%
  mutate(ch_ari_pdoc = 
           case_when(
             ch_ari==1 & h32l==1 & b5==1 ~ 1 ,
             ch_ari==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_pdoc = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_pdoc = "ARI treatment sought from private doctor  among children with ARI")

KRdata <- KRdata %>%
  mutate(ch_ari_pdoc_trt = 
           case_when(
             ch_ari_care==1 & h32l==1 & b5==1 ~ 1 ,
             ch_ari_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_pdoc_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_pdoc_trt = "ARI treatment sought from private doctor among children with ARI that sought treatment")

# //ARI treatment from a pharmacy
KRdata <- KRdata %>%
  mutate(ch_ari_pharm = 
           case_when(
             ch_ari==1 & h32k==1 & b5==1 ~ 1 ,
             ch_ari==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_pharm = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_pharm = "ARI treatment sought from a pharmacy among children with ARI")

KRdata <- KRdata %>%
  mutate(ch_ari_pharm_trt = 
           case_when(
             ch_ari_care==1 & h32k==1 & b5==1 ~ 1 ,
             ch_ari_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_ari_pharm_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_ari_pharm_trt = "ARI treatment sought from a pharmacy among children with ARI that sought treatment")

# *** Fever indicators ***

# //Fever 
KRdata <- KRdata %>%
  mutate(ch_fever = 
           case_when(
             h22==1 & b5==1 ~ 1,
             b5==1 ~ 0  )) %>%
  set_value_labels(ch_fever = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_fever = "Fever symptoms in the 2 weeks before the survey")

# //Fever care-seeking
# This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
# The code below only excludes traditional practitioner (usually h32t). 
# The variable for traditional healer may be different for different surveys (you can check this by checking all the h32* variables). 
# Some surveys also exclude pharmacies, shop, or other sources.
# If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
# h32k from the code below.

KRdata <- KRdata %>%
  mutate(ch_fev_care =
           case_when(
             (ch_fever==1 &  b5==1) & 
               (h32a == 1 | h32b == 1 | h32c == 1 | h32d == 1 | h32e == 1 | h32f == 1 |
                h32g == 1 | h32h == 1 | h32i == 1 | h32j == 1 | h32k == 1 | h32l == 1 |
                h32m == 1 | h32n == 1 | h32o == 1 | h32p == 1 | h32q == 1 | h32r == 1 |
                h32s == 1 |             h32u == 1 | h32v == 1 | h32w == 1 | h32x == 1 )  ~ 1 ,
             b5==1 & ch_fever==1 ~ 0)) %>%
  set_value_labels(ch_fev_care = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_fev_care = "Advice or treatment sought for fever symptoms")

# //Fever care-seeking same or next day

if (h46b_included==1) {
  KRdata <- KRdata %>%
    mutate(ch_fev_care_day = 
             case_when(
               ch_fever==1 & h46b<2 & b5==1 ~ 1 ,
               ch_fever==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_fev_care_day = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_fev_care_day = "Advice or treatment sought for fever symptoms on the same or next day")
} else {
  KRdata <- KRdata %>%
    mutate(ch_fev_care_day = NA)
}

# //Given antibiotics for fever 
KRdata <- KRdata %>%
  mutate(ch_fev_antib = 
           case_when(
             ch_fever==1 & (h37i==1 | h37j==1)  ~ 1,
             ch_fever==1 & (ml13i==1 | ml13j ==1)  ~ 1,
             ch_fever==1 & b5==1 ~ 0  )) %>%
  set_value_labels(ch_fev_antib = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_fev_antib = "Antibiotics taken for fever symptoms")
