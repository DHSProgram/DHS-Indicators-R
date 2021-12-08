# ******************************************************************************
# Program: 		HV_PREV.R
# Purpose: 		Code for HIV prevalence  
# Data inputs: 		IR or MR survey list
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 08, 2021 by Mahmoud Elkasabi
# Note:	This is using the merged file IRMRARmerge 
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# hv_hiv_pos				"HIV positive test result"
# hv_hiv1_pos				"HIV-1 positive test result"
# hv_hiv2_pos				"HIV-2 positive test result"
# hv_hiv1or2_pos			"HIV-1 or HIV-2 positive test result"

# hv_pos_ever_test	"Ever tested for HIV and received result of most recent test among HIV positive"
# hv_pos_12m_test	"Tested in the past 12 months and received result among HIV positive"
# hv_pos_more12m_test	"Tested 12 or more months ago and received result among HIV positive"
# hv_pos_ever_noresult	"Ever tested for HIV and did not receive the result of most recent test among HIV positive"
# hv_pos_nottested	"Not previously tested among HIV positive"
	
# hv_neg_ever_test	"Ever tested for HIV and received result of most recent test among HIV negative"
# hv_neg_12m_test	"Tested in the past 12 months and received result among HIV negative"
# hv_neg_more12m_test	"Tested 12 or more months ago and received result among HIV negative"
# hv_neg_ever_noresult	"Ever tested for HIV and did not receive the result of most recent test among HIV negative"
# hv_neg_nottested	"Not previously tested among HIV negative"

# -----------------------------------------------------------------------------#

#HIV positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv_pos = case_when(
    hiv03==1  ~ 1,
    TRUE ~ 0),
    hv_hiv_pos = add_labels(hv_hiv_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv_pos = set_label(hv_hiv_pos, label = "HIV positive test result"))

#HIV-1 positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv1_pos = case_when(
    hiv03==1 | hiv03==3   ~ 1,
    TRUE ~ 0),
    hv_hiv1_pos = add_labels(hv_hiv1_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv1_pos = set_label(hv_hiv1_pos, label = "HIV-1 positive test result"))

#HIV-2 positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv2_pos = case_when(
    hiv03==2  ~ 1,
    TRUE ~ 0),
    hv_hiv2_pos = add_labels(hv_hiv2_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv2_pos = set_label(hv_hiv2_pos, label = "HIV-2 positive test result"))

#HIV-1 or HIV-2 positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv1or2_pos = case_when(
    hiv03==1 | hiv03==3 | hiv03==2  ~ 1,
    TRUE ~ 0),
    hv_hiv1or2_pos = add_labels(hv_hiv1or2_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv1or2_pos = set_label(hv_hiv1or2_pos, label = "HIV-1 or HIV-2 positive test result"))

#Ever tested
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_ever_test = case_when(
    (v781!=1 | v828!=1) & ( !is.na(hiv03))    ~ 0,
    v781==1 & v828==1  & ( !is.na(hiv03)) ~ 1),
    hv_ever_test = add_labels(hv_ever_test, labels = c("No"=0, "Yes"=1)),
    hv_ever_test = set_label(hv_ever_test, label = "Ever tested for HIV and received result of most recent test"))

#Ever tested among HIV positive
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_pos_ever_test = case_when(
    (v781!=1 | v828!=1) & (hiv03 == 1 | hiv03==3) ~ 0,
    v781==1 & v828==1  & (hiv03 == 1 | hiv03==3) ~ 1),
    hv_pos_ever_test = add_labels(hv_pos_ever_test, labels = c("No"=0, "Yes"=1)),
    hv_pos_ever_test = set_label(hv_pos_ever_test, label = "Ever tested for HIV and received result of most recent test among HIV positive"))

#Tested in the last 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_12m_test = case_when(
    v781==1 & v826a<12 & v828==1 & ( !is.na(hiv03))   ~ 1,
    (v781!=1 | !v826a<12 | v828!=1) & ( !is.na(hiv03))   ~ 0),
    hv_12m_test = add_labels(hv_12m_test, labels = c("No"=0, "Yes"=1)),
    hv_12m_test = set_label(hv_12m_test, label = "Tested in the past 12 months and received result"))

#Tested in the last 12 months among HIV positive
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_pos_12m_test = case_when(
    v781==1 & v826a<12 & v828==1 & (hiv03 == 1 | hiv03==3)   ~ 1,
    (v781!=1 | !v826a<12 | v828!=1) & (hiv03 == 1 | hiv03==3)   ~ 0),
    hv_pos_12m_test = add_labels(hv_pos_12m_test, labels = c("No"=0, "Yes"=1)),
    hv_pos_12m_test = set_label(hv_pos_12m_test, label = "Tested in the past 12 months and received result among HIV positive"))


#Tested 12 or more months
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_more12m_test = case_when(
    v781==1 & v826a>=12 & v828==1 & ( !is.na(hiv03))   ~ 1,
    (v781!=1 | !v826a>=12 | v828!=1) & ( !is.na(hiv03))   ~ 0),
    hv_more12m_test = add_labels(hv_more12m_test, labels = c("No"=0, "Yes"=1)),
    hv_more12m_test = set_label(hv_more12m_test, label = "Tested 12 or more months ago and received result"))

#Tested 12 or more months among HIV positive
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_pos_more12m_test = case_when(
    v781==1 & v826a>=12 & v828==1 & (hiv03 == 1 | hiv03==3)   ~ 1,
    (v781!=1 | !v826a>=12 | v828!=1) & (hiv03 == 1 | hiv03==3)   ~ 0),
    hv_pos_more12m_test = add_labels(hv_pos_more12m_test, labels = c("No"=0, "Yes"=1)),
    hv_pos_more12m_test = set_label(hv_pos_more12m_test, label = "Tested 12 or more months ago and received result among HIV positive"))


#Ever tested but did not receive most recent result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_ever_noresult = case_when(
    v781==1 & v828!=1 & ( !is.na(hiv03))   ~ 1,
    (v781!=1 | v828==1) & ( !is.na(hiv03))  ~ 0),
    hv_ever_noresult = add_labels(hv_ever_noresult, labels = c("No"=0, "Yes"=1)),
    hv_ever_noresult = set_label(hv_ever_noresult, label = "Ever tested for HIV and did not receive the result of most recent test"))


#Ever tested but did not receive most recent result among HIV positive
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_pos_ever_noresult = case_when(
    v781==1 & v828!=1 & (hiv03 == 1 | hiv03==3)    ~ 1,
    (v781!=1 | v828==1) & (hiv03 == 1 | hiv03==3)  ~ 0),
    hv_pos_ever_noresult = add_labels(hv_pos_ever_noresult, labels = c("No"=0, "Yes"=1)),
    hv_pos_ever_noresult = set_label(hv_pos_ever_noresult, label = "Ever tested for HIV and did not receive the result of most recent test among HIV positive"))


#Not previously tested
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_nottested = case_when(
    v781!=1  & ( !is.na(hiv03))  ~ 1,
    v781==1 & ( !is.na(hiv03))  ~ 0),
    hv_nottested = add_labels(hv_nottested, labels = c("No"=0, "Yes"=1)),
    hv_nottested = set_label(hv_nottested, label = "Not previously tested"))


#Not previously tested among HIV positive
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_pos_nottested = case_when(
    v781!=1  & (hiv03 == 1 | hiv03==3)  ~ 1,
    v781==1 & (hiv03 == 1 | hiv03==3)  ~ 0),
    hv_pos_nottested = add_labels(hv_pos_nottested, labels = c("No"=0, "Yes"=1)),
    hv_pos_nottested = set_label(hv_pos_nottested, label = "Not previously tested among HIV positive"))

#Ever tested among HIV negative
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_neg_ever_test = case_when(
    (v781!=1 | v828!=1) & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9) ~ 0,
    v781==1 & v828==1  & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9) ~ 1),
    hv_neg_ever_test = add_labels(hv_neg_ever_test, labels = c("No"=0, "Yes"=1)),
    hv_neg_ever_test = set_label(hv_neg_ever_test, label = "Ever tested for HIV and received result of most recent test among HIV negative"))

#Tested in the last 12 months among HIV negative
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_neg_12m_test = case_when(
    v781==1 & v826a<12 & v828==1 & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)   ~ 1,
    (v781!=1 | !v826a<12 | v828!=1) & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)  ~ 0),
    hv_neg_12m_test = add_labels(hv_neg_12m_test, labels = c("No"=0, "Yes"=1)),
    hv_neg_12m_test = set_label(hv_neg_12m_test, label = "Tested in the past 12 months and received result among HIV negative"))

#Tested 12 or more months among HIV negative
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_neg_more12m_test = case_when(
    v781==1 & v826a>=12 & v828==1 & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)   ~ 1,
    (v781!=1 | !v826a>=12 | v828!=1) & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)   ~ 0),
    hv_neg_more12m_test = add_labels(hv_neg_more12m_test, labels = c("No"=0, "Yes"=1)),
    hv_neg_more12m_test = set_label(hv_neg_more12m_test, label = "Tested 12 or more months ago and received result among HIV negative"))

#Ever tested but did not receive most recent result among HIV negative
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_neg_ever_noresult = case_when(
    v781==1 & v828!=1 & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)   ~ 1,
    (v781!=1 | v828==1) & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)  ~ 0),
    hv_neg_ever_noresult = add_labels(hv_neg_ever_noresult, labels = c("No"=0, "Yes"=1)),
    hv_neg_ever_noresult = set_label(hv_neg_ever_noresult, label = "Ever tested for HIV and did not receive the result of most recent test among HIV negative"))

#Not previously tested among HIV negative
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_neg_nottested = case_when(
    v781!=1  & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)  ~ 1,
    v781==1 & (hiv03 == 0 | hiv03==2 | hiv03 == 7 | hiv03==9)  ~ 0),
    hv_neg_nottested = add_labels(hv_neg_nottested, labels = c("No"=0, "Yes"=1)),
    hv_neg_nottested = set_label(hv_neg_nottested, label = "Not previously tested among HIV negative"))



