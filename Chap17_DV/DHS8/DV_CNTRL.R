# /*****************************************************************************************************
# Program: 			  DV_cntrl.R
# Purpose: 			  Code marital control  indicators from the IR file
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen and translated to R by Shireen Assaf 
# Date last modified: September 17 2021
# *****************************************************************************************************/

# ______________________________________________________________________________
# Variables created in this file:
# 
# //MARITAL CONTROL
# 	dv_prtnr_jeals		"Current or most recent partner ever was jealous if spoke to other men"
# 	dv_prtnr_accus		"Current or most recent partner ever accused her of being unfaithful"
# 	dv_prtnr_friends	"Current or most recent partner ever prevented her from meeting female friends"
# 	dv_prtnr_fam		"Current or most recent partner ever tried to limit her contact with her family"
# 	dv_prtnr_where		"Current or most recent partner ever insisted on knowing where she is at all times"
# 	dv_prtnr_money		"Current or most recent partner ever did not trust her with money"
# 	dv_prtnr_cntrl_3 	"Current or most recent partner displays 3 or more control behaviors"
# 	dv_prtnr_cntrl_0 	"Current or most recent partner displays no control behaviors"
# _____________________________________________________________________________
 
# ********************************************************************************	
# **Marital control by current or most recent partner
# ********************************************************************************	

# //Partner jealous if spoke to other men
IRdata <- IRdata %>%
  mutate(dv_prtnr_jeals =
           case_when(
             d101a==1 ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_jeals = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_jeals = "Current or most recent partner ever was jealous if spoke to other men")

# //Accused her of being unfaithful
IRdata <- IRdata %>%
  mutate(dv_prtnr_accus =
           case_when(
             d101b==1 ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_accus = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_accus = "Current or most recent partner ever accused her of being unfaithful")

# //Prevented her from meeting female friends
IRdata <- IRdata %>%
  mutate(dv_prtnr_friends =
           case_when(
             d101c==1 ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_friends = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_friends = "Current or most recent partner ever prevented her from meeting female friends")

# //Tried to limit her contact with her family
IRdata <- IRdata %>%
  mutate(dv_prtnr_fam =
           case_when(
             d101d==1 ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_fam = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_fam = "Current or most recent partner ever tried to limit her contact with her family")

# //Insisted on knowing where she is at all times
IRdata <- IRdata %>%
  mutate(dv_prtnr_where =
           case_when(
             d101e==1 ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_where = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_where = "Current or most recent partner ever insisted on knowing where she is at all times")

# //Did not trust her with money
IRdata <- IRdata %>%
  mutate(dv_prtnr_money =
           case_when(
             d101f==1 ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_money = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_money = "Current or most recent partner ever did not trust her with money")

# //Partner displays marital control behaviors
IRdata <- IRdata %>%
  mutate(dv_prtnr_cntrl_temp = dv_prtnr_jeals+ dv_prtnr_accus+ dv_prtnr_friends+ dv_prtnr_fam+ dv_prtnr_where+ dv_prtnr_money)
  IRdata[["dv_prtnr_cntrl_temp"]] <- ifelse(IRdata[["v044"]]!=1 | IRdata[["v502"]]==0, NA, IRdata[["dv_prtnr_cntrl_temp"]]) 	  
IRdata <- IRdata %>%
  mutate(dv_prtnr_cntrl_cat =          
           case_when(
             dv_prtnr_cntrl_temp %in% c(1,2) ~ 1,
             dv_prtnr_cntrl_temp %in% c(3,4) ~ 2,
             dv_prtnr_cntrl_temp %in% c(5,6) ~ 3,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_cntrl_cat = c("5-6"=3,"3-4"=2,"1-2" = 1, "0"=0)) %>%
  set_variable_labels(dv_prtnr_cntrl_cat = "Current or most recent partner displays control behaviors")

# //Partner displays 3 or more of marital control behaviors
IRdata <- IRdata %>%
  mutate(dv_prtnr_cntrl_3 =          
           case_when(
             dv_prtnr_cntrl_temp %in% c(3,4,5,6) ~ 1,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_cntrl_3 = c("Yes"= 1, "No"=0 )) %>%
  set_variable_labels(dv_prtnr_cntrl_3 = "Current or most recent partner displays 3 or more control behaviors")

# //Partner displays none of these marital control behaviors
IRdata <- IRdata %>%
  mutate(dv_prtnr_cntrl_0 =          
           case_when(
             dv_prtnr_cntrl_temp >0 ~ 0,
             v044==1 & v502>0 ~ 1 )) %>% 
  set_value_labels(dv_prtnr_cntrl_0 = c("1 or more behaviors"= 0, "No behaviors"=1 )) %>%
  set_variable_labels(dv_prtnr_cntrl_0 = "Current or most recent partner displays no control behaviors")

