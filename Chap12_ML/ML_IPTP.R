# ******************************************************************************
# Program: 		ML_IPTP.R
# Purpose: 		Code malaria IPTP indicators  
# Data inputs: 		IR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: January 12, 2022 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_one_iptp		"One or more doses of SP/Fansidar"
# ml_two_iptp		"Two or more doses of SP/Fansidar"
# ml_three_iptp	"Three or more doses of SP/Fansidar"
# -----------------------------------------------------------------------------#

# Age of most recent birth
# If b19_01 is not available in the data use v008 - b3_01
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

# 1+ doses SP/Fansidar
IRdata <- IRdata %>%
  mutate(ml_one_iptp = case_when(
    m49a_1==1 & age<24  ~ 1,
    m49a_1!=1 & age<24  ~ 0),
    ml_one_iptp = set_label(ml_one_iptp, label = "One or more doses of SP/Fansidar"))

# 2+ doses SP/Fansidar
IRdata <- IRdata %>%
  mutate(ml_two_iptp = case_when(
    m49a_1==1 & ml1_1 >=2 & ml1_1<=97 & age<24  ~ 1,
    !(m49a_1==1 & ml1_1 >=2 & ml1_1<=97) & age<24  ~ 0),
    ml_two_iptp = set_label(ml_two_iptp, label = "Two or more doses of SP/Fansidar"))

# 3+ doses SP/Fansidar
IRdata <- IRdata %>%
  mutate(ml_three_iptp = case_when(
    m49a_1==1 & ml1_1 >=3 & ml1_1<=97 & age<24  ~ 1,
   !(m49a_1==1 & ml1_1 >=3 & ml1_1<=97) & age<24  ~ 0),
    ml_three_iptp = set_label(ml_three_iptp, label = "Three or more doses of SP/Fansidar"))
