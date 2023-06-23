# ******************************************************************************
# Program: 		CM_RISK_wm.R
# Purpose: 		Code to compute high risk birth in women  
# Data inputs: 		IR survey list
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: September 24 2021 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# cm_riskw_none			"Currently married women not in any high-risk category"
# cm_riskw_unavoid		"Currently married women with unavoidable risk- first order birth between age 18 and 34"
# cm_riskw_any_avoid		"Currently married women in any avoidable high-risk category"

# cm_riskw_u18			"Currently married women less than 18 years"
# cm_riskw_o34			"Currently married women over 34 years"
# cm_riskw_interval		"Currently married women with <24mos since preceding birth"
# cm_riskw_order			"Currently married women with a birth order 4 or higher"
# cm_riskw_any_single		"Currently married women in any single high-risk category"

# cm_riskw_mult1			"Currently married women with multiple risks - under age 18 and short interval"
# cm_riskw_mult2			"Currently married women with multiple risks - over age 34 and short interval"
# cm_riskw_mult3			"Currently married women with multiple risks - over age 34 and high order"
# cm_riskw_mult4			"Currently married women with multiple risks - over age 34 and short interval and high order"
# cm_riskw_mult5			"Currently married women with multiple risks - short interval and high order"
# cm_riskw_any_mult		"Currently married women in any multiple risk category"

# cm_riskw_u18_avoid		"Currently married women with individual avoidable risk - less than 18 years"
# cm_riskw_o34_avoid		"Currently married women with individual avoidable risk - over 34 years"
# cm_riskw_interval_avoid	"Currently married women with individual avoidable risk - <24mos since preceding birth"
# cm_riskw_order_avoid	"Currently married women with individual avoidable risk - birth order 4 or higher"
# -----------------------------------------------------------------------------#

IRdata_RISK <- IRdata %>%
  filter(v502==1) %>%             # Indicators are computed for currently married women
  mutate(wt = v005/1000000) %>%
  mutate(age_of_mother= v008-v011) %>%     # woman's age
  mutate(young = case_when(
    age_of_mother<((17*12)+3) & (v312!=6) ~ 1,
    TRUE ~ 0)) %>%
  mutate(old = case_when(
    age_of_mother>((34*12)+2) & (v312!=6) ~ 1,
    TRUE ~ 0)) %>%
  mutate(soon = case_when(
    (v222<15) & (v312!=6) ~ 1,
    TRUE ~ 0)) %>%
  mutate(many = case_when(
    (v201>2) & (v312!=6) ~ 1,
    TRUE ~ 0)) %>%
  mutate(firstbirth = case_when(
    (v201==0) & (v312!=6) ~ 1,
    TRUE ~ 0)) %>%
  mutate(cm_riskw_unavoid = case_when(
    firstbirth==1 & young==0 & old==0 & soon==0 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_unavoid = set_label(cm_riskw_unavoid, label = "Currently married women with unavoidable risk - first order birth between age 18 and 34")) %>%
  mutate(cm_riskw_u18 = case_when(
    young==1 & old==0 & soon==0 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_u18 = set_label(cm_riskw_u18, label = "Currently married women less than 18 years")) %>%
  mutate(cm_riskw_o34 = case_when(
    young==0 & old==1 & soon==0 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_o34 = set_label(cm_riskw_o34, label = "Currently married women over 34 years")) %>%
  mutate(cm_riskw_interval = case_when(
    young==0 & old==0 & soon==1 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_interval = set_label(cm_riskw_interval, label = "Currently married women with <24mos since preceding birth")) %>%
  mutate(cm_riskw_order = case_when(
    young==0 & old==0 & soon==0 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskw_order = set_label(cm_riskw_order, label = "Currently married women with a birth order 4 or higher")) %>%
  mutate(cm_riskw_any_single = case_when(
    cm_riskw_u18+cm_riskw_o34+cm_riskw_interval+cm_riskw_order>0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_any_single = set_label(cm_riskw_any_single, label = "Currently married women in any single high-risk category")) %>%  
  mutate(cm_riskw_mult1 = case_when(
    (young==1 & old==0 & soon==1 & many==0) | (young==1 & old==0 & soon==0 & many==1) | (young==1 & old==0 & soon==1 & many==1) ~ 1, 
    TRUE ~ 0),
    cm_riskw_mult1 = set_label(cm_riskw_mult1, label = "Currently married women with multiple risks - under age 18 and short interval")) %>%  
  mutate(cm_riskw_mult2 = case_when(
    young==0 & old==1 & soon==1 & many==0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_mult2 = set_label(cm_riskw_mult2, label = "Currently married women with multiple risks - over age 34 and short interval")) %>%  
  mutate(cm_riskw_mult3 = case_when(
    young==0 & old==1 & soon==0 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskw_mult3 = set_label(cm_riskw_mult3, label = "Currently married women with multiple risks - over age 34 and high order")) %>%  
  mutate(cm_riskw_mult4 = case_when(
    young==0 & old==1 & soon==1 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskw_mult4 = set_label(cm_riskw_mult4, label = "Currently married women with multiple risks - over age 34 and short interval and high order")) %>%  
  mutate(cm_riskw_mult5 = case_when(
    young==0 & old==0 & soon==1 & many==1 ~ 1, 
    TRUE ~ 0),
    cm_riskw_mult5 = set_label(cm_riskw_mult5, label = "Currently married women with multiple risks - short interval and high order")) %>%  
  mutate(cm_riskw_any_mult = case_when(
    cm_riskw_mult1+cm_riskw_mult2+cm_riskw_mult3+cm_riskw_mult4+cm_riskw_mult5 >0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_any_mult = set_label(cm_riskw_any_mult, label = "Currently married women in any multiple risk category")) %>%  
  mutate(cm_riskw_any_avoid = case_when(
    cm_riskw_any_single+cm_riskw_any_mult>0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_any_avoid = set_label(cm_riskw_any_avoid, label = "Currently married women in any avoidable high-risk category")) %>%  
  mutate(cm_riskw_none = case_when(
    cm_riskw_unavoid==0 & cm_riskw_any_avoid==0 & firstbirth==0 ~ 1, 
    TRUE ~ 0),
    cm_riskw_none = set_label(cm_riskw_none, label = "Currently married women not in any high-risk category")) %>%    
  mutate(cm_riskw_u18_avoid = young,
    cm_riskw_u18_avoid = set_label(cm_riskw_u18_avoid, label = "Currently married women with individual avoidable risk - mothers less than 18 years")) %>%    
  mutate(cm_riskw_o34_avoid = old,
         cm_riskw_o34_avoid = set_label(cm_riskw_o34_avoid, label = "Currently married women with individual avoidable risk - mothers over 34 years")) %>%    
  mutate(cm_riskw_interval_avoid = soon,
         cm_riskw_interval_avoid = set_label(cm_riskw_interval_avoid, label = "Currently married women with individual avoidable risk - born <24mos since preceding birth")) %>%    
  mutate(cm_riskw_order_avoid = many,
         cm_riskw_order_avoid = set_label(cm_riskw_order_avoid, label = "Currently married women with individual avoidable risk - birth order 4 or higher"))  
  
