# ******************************************************************************
# Program: 		HV_TEST_COVG.R
# Purpose: 		Code to compute coverage of HIV testing  
# Data inputs: 		PR file merged with AR file
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# hv_hiv_test_wm	"Testing status among women eligible for HIV testing"
# hv_hiv_test_mn	"Testing status among men eligible for HIV testing"
# hv_hiv_test_tot	"Testing status among total eligible for HIV testing"
# -----------------------------------------------------------------------------#
# Note: The denominator is among de facto population that is selected for HIV testing. 
# Here hv027 (selected for men's survey) are those selected for HIV. 

# HIV testing status among women
AR_PMRdata <- AR_PMRdata %>%
  mutate(test = case_when(
    !(hv103==0 | hv027==0) & ha63==1 & (hiv03 != 8 | is.na(hiv03))  ~ 1,
    !(hv103==0 | hv027==0) & ha63==3  ~ 2,
    !(hv103==0 | hv027==0) & ha63==2  ~ 3,
    !(hv103==0 | hv027==0) & (hiv03==8 | (ha63==4 | ha63==5 | ha63==6 | ha63==9))  ~ 4,
    hv103==0 | hv027==0 ~ 99)) %>%
  replace_with_na(replace = list(test = c(99))) %>%
  mutate(interv = case_when(
    ha65==1  ~ 1,
    ha65>=2 & ha65<=7   ~ 2)) %>%
  mutate(hv_hiv_test_wm = case_when(
    test==1 & interv == 1  ~ 1,
    test==2 & interv == 1  ~ 3,
    test==3 & interv == 1  ~ 5,
    test==4 & interv == 1  ~ 7,
    is.na(test) & interv == 1  ~ 8,
    test==1 & interv == 2  ~ 2,
    test==2 & interv == 2  ~ 4,
    test==3 & interv == 2  ~ 6,
    test==4 & interv == 2  ~ 8,
    is.na(test) & interv == 2  ~ 8),
    hv_hiv_test_wm = add_labels(hv_hiv_test_wm, labels = c("DHS tested/interviewed"=1, "DHS tested/not interveiwed"=2, 
                                                           "Refused to give blood/interviewed"=3, "Refused to give blood/not interviewed"=4, 
                                                           "Not available for blood collection/interveiwed"=5, "Not available for blood collection/not interveiwed"=6, 
                                                           "Other or missing/interviewed"=7, "Other or missing/not interveiwed"=8)),
    hv_hiv_test_wm = set_label(hv_hiv_test_wm, label = "Testing status among women eligible for HIV testing"))%>%
  mutate(test = NULL)%>%
  mutate(interv = NULL)

# HIV testing status among men
AR_PMRdata <- AR_PMRdata %>%
  mutate(test = case_when(
    (hv103!=0 & hv027!=0) & hb63==1 & (hiv03!=8 | is.na(hiv03))  ~ 1,
    (hv103!=0 & hv027!=0) & hb63==3  ~ 2,
    (hv103!=0 & hv027!=0) & hb63==2  ~ 3,
    (hv103!=0 & hv027!=0) & (hiv03==8 | (hb63==4 | hb63==5 | hb63==6 | hb63==9))  ~ 4,
    hv103==0 | hv027==0 ~ 99,
    TRUE ~ 99)) %>%
  replace_with_na(replace = list(test = c(99))) %>%
  mutate(interv = case_when(
    hb65==1  ~ 1,
    hb65>=2 & hb65<=7   ~ 2)) %>%
  mutate(hv_hiv_test_mn = case_when(
    test==1 & interv == 1  ~ 1,
    test==2 & interv == 1  ~ 3,
    test==3 & interv == 1  ~ 5,
    test==4 & interv == 1  ~ 7,
    is.na(test) & interv == 1  ~ 8,
    test==1 & interv == 2  ~ 2,
    test==2 & interv == 2  ~ 4,
    test==3 & interv == 2  ~ 6,
    test==4 & interv == 2  ~ 8,
    is.na(test) & interv == 2  ~ 8),
    hv_hiv_test_mn = add_labels(hv_hiv_test_mn, labels = c("DHS tested/interviewed"=1, "DHS tested/not interveiwed"=2, 
                                                           "Refused to give blood/interviewed"=3, "Refused to give blood/not interviewed"=4, 
                                                           "Not available for blood collection/interveiwed"=5, "Not available for blood collection/not interveiwed"=6, 
                                                           "Other or missing/interviewed"=7, "Other or missing/not interveiwed"=8)),
    hv_hiv_test_mn = set_label(hv_hiv_test_mn, label = "Testing status among men eligible for HIV testing")) %>%
  mutate(test = NULL)%>%
  mutate(interv = NULL)

# HIV testing status among population (men and women)
AR_PMRdata <- AR_PMRdata %>%
  mutate(hv_hiv_test_tot = case_when(
    hv104==2  ~ hv_hiv_test_wm,
    hv104==1  ~ hv_hiv_test_mn),
    hv_hiv_test_tot = add_labels(hv_hiv_test_tot, labels = c("DHS tested/interviewed"=1, "DHS tested/not interveiwed"=2, 
                                                           "Refused to give blood/interviewed"=3, "Refused to give blood/not interviewed"=4, 
                                                           "Not available for blood collection/interveiwed"=5, "Not available for blood collection/not interveiwed"=6, 
                                                           "Other or missing/interviewed"=7, "Other or missing/not interveiwed"=8)),
    hv_hiv_test_tot = set_label(hv_hiv_test_tot, label = "Testing status among total eligible for HIV testing"))
