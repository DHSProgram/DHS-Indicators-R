# ******************************************************************************
# Program: 		ML_IPTP.R - DHS8 update
# Purpose: 		Code malaria IPTP indicators  
# Data inputs: 		NR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: August 07, 2024 by Courtney Allen
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_one_iptp		"One or more doses of SP/Fansidar"
# ml_two_iptp		"Two or more doses of SP/Fansidar"
# ml_three_iptp	"Three or more doses of SP/Fansidar"
# -----------------------------------------------------------------------------#

# keep only live birth and/or stillbirth in the 2 years before the survey
# NOTE: if any woman has had a livebirth AND stillbirth in the last 2 years, look at most recent birth

NRdata <- NRdata %>% 
  filter(p19<24 & m80%in%c(1, 3)) %>%
  group_by(caseid, p19) %>%  
  slice_min(order_by = p0) %>% #keep 1st of multiple index if there are multiples born on same date
  ungroup() %>%
  group_by(caseid) %>%
  slice_min(order_by = p19) %>% #keep most recent still/livebirth if there are >1 in last 24 months
  ungroup() 

  
# 1+ doses SP/Fansidar
NRdata <- NRdata %>%
  mutate(ml_one_iptp = case_when(
    m49a==1  ~ 1,
    m49a!=1  ~ 0)) %>%
  set_value_labels(ml_one_iptp = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_one_iptp = "One or more doses of SP/Fansidar")

# 2+ doses SP/Fansidar
NRdata <- NRdata %>%
  mutate(ml_two_iptp = case_when(
    !(m49a==1 & ml1 >=2 & ml1<=97)  ~ 0,
    m49a==1 & ml1 >=2 & ml1<=97  ~ 1)) %>%
  set_value_labels(ml_two_iptp = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_two_iptp = "Two or more doses of SP/Fansidar")

# 3+ doses SP/Fansidar
NRdata <- NRdata %>%
  mutate(ml_three_iptp = case_when(
    !(m49a==1 & ml1 >=3 & ml1<=97) ~ 0,
    m49a==1 & ml1 >=3 & ml1<=97 ~ 1)) %>%
  set_value_labels(ml_three_iptp = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_three_iptp = "Three or more doses of SP/Fansidar")


