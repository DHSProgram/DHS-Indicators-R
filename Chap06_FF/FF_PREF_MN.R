# ******************************************************************************
# Program: 		FF_PREF.R
# Purpose: 		Code to compute fertility preferences in men  
# Data inputs: 		IR or MR survey list
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: September 03 2021 by Mahmoud Elkasabi
# ******************************************************************************
#The five indicators below can be computed for men. 
#For men the indicator is computed for age 15-49 in line 56. This can be commented out if the indicators are required for all men.
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ff_want_type		"Type of desire for children"
# ff_want_nomore	"Want no more children"
# ff_ideal_num		"Ideal number of children"
# ff_ideal_mean_all	"Mean ideal number of children for all"
# ff_ideal_mean_mar	"Mean ideal number of children for married"
# -----------------------------------------------------------------------------#

# indicators from MR file
MRdata <- MRdata %>%
  mutate(wt = mv005/1000000) %>%
  mutate(ff_want_type = case_when(
    mv502 == 1 ~ mv605,
    mv502 != 1 ~ 99)) %>%
  replace_with_na(replace = list(ff_want_type = c(99))) %>%
  mutate(ff_want_type = set_label(ff_want_type, label = "Type of desire for children")) %>%
  mutate(ff_want_nomore = case_when(
    mv502 == 1 & (mv605==5 | mv605==6) ~ 1, 
    mv502 == 1 & (mv605!=5 & mv605!=6) ~ 0),
    ff_want_nomore = add_labels(ff_want_nomore, labels = c("No"=0, "Yes"=1)),
    ff_want_nomore = set_label(ff_want_nomore, label = "Want no more children")) %>%
  mutate(ff_ideal_num = case_when(
    mv613 >= 0 & mv613 <= 5 ~ mv613,
    mv613 >= 6 ~ 6,
    mv613 >= 95 & mv613 <= 99 ~ 9),
    rc_litr = add_labels(ff_ideal_num, labels = c("6+"=6, "non-numeric response"=9)),
    rc_litr = set_label(ff_ideal_num, label = "Ideal number of children"))%>%
  mutate(numch = case_when(
    mv213!=1 & mv218 <= 6 ~ mv218,
    mv213==1 & mv218 <= 5 ~ mv218+1,
    mv213!=1 & mv218 > 6 ~ 6,
    mv213==1 & mv218 > 5 ~ 6),
    numch = add_labels(numch, labels = c("6+"=6)),
    numch = set_label(numch, label = "Number of living children"))

