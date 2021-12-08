# ******************************************************************************
# Program: 		    HV_CIRCUM.R
# Purpose: 		    Code for Circumcision and HIV  
# Data inputs: 		MR survey list
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# Note:	This is using the merged file IRMRARmerge 
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# hv_hiv_circum_skilled	"Circumcised by a health professional"
# hv_hiv_circum_trad		"Circumcised by a traditional practitioner, family, or friend"
# hv_hiv_circum_pos		"Circumcised"
# hv_hiv_uncircum_pos		"Uncircumcised and HIV positive"
# -----------------------------------------------------------------------------#

#Circumcised by health professional
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv_circum_skilled = case_when(
    !(v483==1 & v483b==2) & (!is.na(hiv03))   ~ 0,
    v483==1 & v483b==2 & (!is.na(hiv03)) ~ 1),
    hv_hiv_circum_skilled = add_labels(hv_hiv_circum_skilled, labels = c("No"=0, "Yes"=1)),
    hv_hiv_circum_skilled = set_label(hv_hiv_circum_skilled, label = "Circumcised by a health professional"))

#Circumcised by traditional practitioner/family/friend 
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv_circum_trad = case_when(
    !(v483==1 & v483b==1) & (!is.na(hiv03))   ~ 0,
    v483==1 & v483b==1 & (!is.na(hiv03)) ~ 1),
    hv_hiv_circum_trad = add_labels(hv_hiv_circum_trad, labels = c("No"=0, "Yes"=1)),
    hv_hiv_circum_trad = set_label(hv_hiv_circum_trad, label = "Circumcised by a traditional practitioner, family, or friend"))

#Circumcised
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv_circum_pos = case_when(
    v483!=1 & (!is.na(hiv03)) ~ 0,
    v483==1 & (!is.na(hiv03)) ~ 1),
    hv_hiv_circum_pos = add_labels(hv_hiv_circum_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv_circum_pos = set_label(hv_hiv_circum_pos, label = "Circumcised"))

#Uncircumcised
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv_uncircum_pos = case_when(
    v483!=1 & (!is.na(hiv03)) ~ 1,
    v483==1 & (!is.na(hiv03)) ~ 0),
    hv_hiv_uncircum_pos = add_labels(hv_hiv_circum_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv_uncircum_pos = set_label(hv_hiv_circum_pos, label = "Uncircumcised"))