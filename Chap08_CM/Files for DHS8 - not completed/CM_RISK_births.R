# ******************************************************************************
# Program: 		CM_RISK_birth.R
# Purpose: 		Code to compute high risk births  
# Data inputs: 		KR survey list
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: September 24 2021 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# cm_riskb_none			"Births not in any high risk category"
# cm_riskb_unavoid		"Births with unavoidable risk- first order birth between age 18 and 34"
# cm_riskb_any_avoid		"Births in any avoidable high-risk category"

# cm_riskb_u18			"Births to mothers less than 18 years"
# cm_riskb_o34			"Births to mothers over 34 years"
# cm_riskb_interval		"Births born <24mos since preceding birth"
# cm_riskb_order			"Births with a birth order 4 or higher"
# cm_riskb_any_single		"Birth with any single high-risk category"

# cm_riskb_mult1			"Births with multiple risks - under age 18 and short interval"
# cm_riskb_mult2			"Births with multiple risks - over age 34 and short interval"
# cm_riskb_mult3			"Births with multiple risks - over age 34 and high order"
# cm_riskb_mult4			"Births with multiple risks - over age 34 and short interval and high order"
# cm_riskb_mult5			"Births with multiple risks - short interval and high order"
# cm_riskb_any_mult		"Births with any multiple risk category"

# cm_riskb_u18_avoid		"Births with individual avoidable risk - mothers less than 18 years"
# cm_riskb_o34_avoid		"Births with individual avoidable risk - mothers over 34 years"
# cm_riskb_interval_avoid	"Births with individual avoidable risk - born <24mos since preceding birth"
# cm_riskb_order_avoid	"Births with individual avoidable risk - birth order 4 or higher"
# -----------------------------------------------------------------------------#

KRdata_RISK <- KRdata %>%
  mutate(wt = v005/1000000) %>%
  mutate(age_of_mother= as.integer((b3-v011)/12)) %>%
  mutate(bord_adj = case_when(
    b0>1 ~ bord-b0+1,
    TRUE ~ bord)) %>%
  mutate(young = case_when(
    age_of_mother<18 ~ 1,
    TRUE ~ 0)) %>%
  mutate(old = case_when(
    age_of_mother>34 ~ 1,
    TRUE ~ 0)) %>%
  mutate(soon = case_when(
    b11<24 ~ 1,
    TRUE ~ 0)) %>%
  mutate(many = case_when(
    bord_adj>3 ~ 1,
    TRUE ~ 0)) %>%
  mutate(cm_riskb_unavoid = case_when(
    bord_adj==1 & young==0 & old==0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_unavoid = set_label(cm_riskb_unavoid, label = "Births with unavoidable risk- first order birth between age 18 and 34")) %>%
  mutate(cm_riskb_u18 = case_when(
    young==1 & old==0 & soon==0 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_u18 = set_label(cm_riskb_u18, label = "Births to mothers less than 18 years")) %>%
  mutate(cm_riskb_o34 = case_when(
    young==0 & old==1 & soon==0 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_o34 = set_label(cm_riskb_o34, label = "Births to mothers over 34 years")) %>%  
  mutate(cm_riskb_interval = case_when(
    young==0 & old==0 & soon==1 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_interval = set_label(cm_riskb_interval, label = "Births born <24mos since preceding birth")) %>%    
  mutate(cm_riskb_order = case_when(
    young==0 & old==0 & soon==0 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskb_order = set_label(cm_riskb_order, label = "Births with a birth order 4 or higher")) %>%      
  mutate(cm_riskb_any_single = case_when(
    cm_riskb_u18+cm_riskb_o34+cm_riskb_interval+cm_riskb_order>0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_any_single = set_label(cm_riskb_any_single, label = "Birth with any single high-risk category")) %>%    
  mutate(cm_riskb_mult1 = case_when(
    (young==1 & old==0 & soon==1 & many==0) | (young==1 & old==0 & soon==0 & many==1) | (young==1 & old==0 & soon==1 & many==1) ~ 1, 
    TRUE ~ 0),
    cm_riskb_mult1 = set_label(cm_riskb_mult1, label = "Births with multiple risks - under age 18 and short interval")) %>%   
  mutate(cm_riskb_mult2 = case_when(
    young==0 & old==1 & soon==1 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_mult2 = set_label(cm_riskb_mult2, label = "Births with multiple risks - over age 34 and short interval")) %>%   
  mutate(cm_riskb_mult3 = case_when(
    young==0 & old==1 & soon==0 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskb_mult3 = set_label(cm_riskb_mult3, label = "Births with multiple risks - over age 34 and high order")) %>%   
  mutate(cm_riskb_mult4 = case_when(
    young==0 & old==1 & soon==1 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskb_mult4 = set_label(cm_riskb_mult4, label = "Births with multiple risks - over age 34 and short interval and high order")) %>%   
  mutate(cm_riskb_mult5 = case_when(
    young==0 & old==0 & soon==1 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskb_mult5 = set_label(cm_riskb_mult5, label = "Births with multiple risks - short interval and high order")) %>%   
  mutate(cm_riskb_any_mult = case_when(
    cm_riskb_mult1+cm_riskb_mult2+cm_riskb_mult3+cm_riskb_mult4+cm_riskb_mult5 >0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_any_mult = set_label(cm_riskb_any_mult, label = "Births with any multiple risk category")) %>%     
  mutate(cm_riskb_any_avoid = case_when(
    cm_riskb_any_single+cm_riskb_any_mult>0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_any_avoid = set_label(cm_riskb_any_avoid, label = "Births in any avoidable high-risk category")) %>%     
  mutate(cm_riskb_none = case_when(
    cm_riskb_unavoid==0 & cm_riskb_any_avoid==0 ~ 1, 
    TRUE ~ 0),
    cm_riskb_none = set_label(cm_riskb_none, label = "Births not in any high risk category")) %>%     
  mutate(cm_riskb_u18_avoid = young,
         cm_riskb_u18_avoid = set_label(cm_riskb_u18_avoid, label = "Births with individual avoidable risk - mothers less than 18 years")) %>%       
  mutate(cm_riskb_o34_avoid = old,
         cm_riskb_o34_avoid = set_label(cm_riskb_o34_avoid, label = "Births with individual avoidable risk - mothers over 34 years")) %>%       
  mutate(cm_riskb_interval_avoid = soon,
         cm_riskb_interval_avoid = set_label(cm_riskb_interval_avoid, label = "Births with individual avoidable risk - born <24mos since preceding birth")) %>%       
  mutate(cm_riskb_order_avoid = many,
         cm_riskb_order_avoid = set_label(cm_riskb_order_avoid, label = "Births with individual avoidable risk - birth order 4 or higher"))
