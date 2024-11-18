# /*****************************************************************************************************
# Program: 			  PH_HNDWSH.R
# Purpose: 			  Code to compute handwashing indicators
# Data inputs: 		PR dataset
# Data outputs:		coded variables
# Author:			  	Shireen Assaf
# Date last modified: November 16, 2021 by Shireen Assaf 
# Note:				The HR file can also be used to code these indicators among households. 
#             The condition hv102 would need to be removed. 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# ph_hndwsh_place_fxd		"Fixed place for handwashing"
# ph_hndwsh_place_mob		"Mobile place for handwashing"
# ph_hndwsh_place_any		"Either fixed or mobile place for handwashing"
# ph_hndwsh_water				"Place observed for handwashing with water"
# ph_hndwsh_soap				"Place observed for handwashing with soap"
# ph_hndwsh_clnsagnt		"Place observed for handwashing with cleansing agent other than soap"
# ph_hndwsh_basic				"Basic handwashing facility"
# ph_hndwsh_limited			"Limited handwashing facility"
# 
# ----------------------------------------------------------------------------*/
PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)

# //Fixed place for handwashing
PRdata <- PRdata %>%
  mutate(ph_hndwsh_place_fxd = 
           case_when(
             hv230a==1 & hv102==1 ~ 1,
             hv230a>1 & hv102==1 ~ 0 )) %>%
  set_value_labels(ph_hndwsh_place_fxd = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_place_fxd = "Fixed place for handwashing")

# //Mobile place for handwashing
PRdata <- PRdata %>%
  mutate(ph_hndwsh_place_mob = 
           case_when(
             hv230a==2 & hv102==1 ~ 1,
             hv230a!=2 & hv102==1 ~ 0 )) %>%
  set_value_labels(ph_hndwsh_place_mob = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_place_mob = "Mobile place for handwashing")

# //Fixed or mobile place for handwashing
PRdata <- PRdata %>%
  mutate(ph_hndwsh_place_any = 
           case_when(
             hv230a<=2 & hv102==1 ~ 1,
             hv230a>2 & hv102==1 ~ 0 )) %>%
  set_value_labels(ph_hndwsh_place_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_place_any = "Either fixed or mobile place for handwashing")

# //Place observed for handwashing with water
PRdata <- PRdata %>%
  mutate(ph_hndwsh_water = 
           case_when(
              hv230b==1 & hv102==1 ~ 1,
              hv230a<=2 & hv102==1 ~ 0)) %>%
  set_value_labels(ph_hndwsh_water = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_water = "Place observed for handwashing with water")

# //Place observed for handwashing with soap
PRdata <- PRdata %>%
  mutate(ph_hndwsh_soap = 
           case_when(
             hv232==1 & hv102==1 ~ 1,
             hv230a<=2 & hv102==1 ~ 0)) %>%
  set_value_labels(ph_hndwsh_soap = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_soap = "Place observed for handwashing with soap")

# //Place observed for handwashing with cleansing agent other than soap
PRdata <- PRdata %>%
  mutate(ph_hndwsh_clnsagnt = 
           case_when(
             hv232b==1 & hv102==1 ~ 1,
             hv230a<=2 & hv102==1 ~ 0)) %>%
  set_value_labels(ph_hndwsh_clnsagnt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_clnsagnt = "Place observed for handwashing with cleansing agent other than soap")

# //Basic handwashing facility
PRdata <- PRdata %>%
  mutate(ph_hndwsh_basic = 
           case_when(
             hv230b==1 & hv232==1 & hv102==1 ~ 1,
             hv230a<=3 & hv102==1 ~ 0)) %>%
  set_value_labels(ph_hndwsh_basic = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_basic = "Basic handwashing facility")

# //Limited handwashing facility
PRdata <- PRdata %>%
  mutate(ph_hndwsh_limited = 
           case_when(
             hv230b==0 | hv232==0 & hv102==1 ~ 1,
             hv230a<=3 & hv102==1 ~ 0)) %>%
  set_value_labels(ph_hndwsh_limited = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_hndwsh_limited = "Limited handwashing facility")