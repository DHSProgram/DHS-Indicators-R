# /*****************************************************************************************************
# Program: 			RH_probs.R
# Purpose: 			Code indicators on problems accessing health care for women
# Data inputs: 	IR dataset
# Data outputs:	Coded variables
# Author:				Shireen Assaf
# Date last modified: Sept 13 2021 by Shireen Assaf 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------//
# Variables created in this file:
# rh_prob_permit	"Problem health care access: permission to go"
# rh_prob_money		"Problem health care access: getting money"
# rh_prob_dist		"Problem health care access: distance to facility"
# rh_prob_alone		"Problem health care access: not wanting to go alone"
# rh_prob_minone	"At least one problem in accessing health care"
# /----------------------------------------------------------------------------*/

# //Permission to go 
IRdata <- IRdata %>%
  mutate(rh_prob_permit =
           case_when(
             v467b %in% c(0,2,9, NA)   ~ 0 ,  
             v467b ==1 ~ 1)) %>%
  set_value_labels(rh_prob_permit = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_prob_permit = "Problem health care access: permission to go")

# //Getting money
IRdata <- IRdata %>%
  mutate(rh_prob_money =
           case_when(
             v467c %in% c(0,2,9, NA)   ~ 0 ,  
             v467c ==1 ~ 1)) %>%
  set_value_labels(rh_prob_money = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_prob_money = "Problem health care access: getting money")
 	
# //Distance to facility
IRdata <- IRdata %>%
  mutate(rh_prob_dist =
           case_when(
             v467d %in% c(0,2,9, NA)   ~ 0 ,  
             v467d ==1 ~ 1)) %>%
  set_value_labels(rh_prob_dist = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_prob_dist = "Problem health care access: distance to facility")

# //Not wanting to go alone
IRdata <- IRdata %>%
  mutate(rh_prob_alone =
           case_when(
             v467f %in% c(0,2,9, NA)   ~ 0 ,  
             v467f ==1 ~ 1)) %>%
  set_value_labels(rh_prob_alone = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_prob_alone = "Problem health care access: not wanting to go alone")

# //At least one problem
IRdata <- IRdata %>%
  mutate(rh_prob_minone =
           ifelse(rh_prob_permit+rh_prob_money+rh_prob_dist+rh_prob_alone>=1,1,0)) %>%
  set_value_labels(rh_prob_minone = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_prob_minone = "At least one problem in accessing health care")
	
