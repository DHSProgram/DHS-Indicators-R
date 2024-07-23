# ******************************************************************************
# Program: 			  FP_COMM.R
# Purpose: 		    Code communication related indicators: exposure to FP messages,
#                 decision on use/nonuse, discussions.  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29 2021 by Courtney Allen
# ******************************************************************************
#   

# -----------------------------------------------------------------------------#
# # Variables created in this file:
# fp_message_radio		"Exposure to family planning message by radio"
# fp_message_tv			  "Exposure to family planning message by TV"
# fp_message_paper		"Exposure to family planning message by newspaper/magazine"
# fp_message_mobile		"Exposure to family planning message by mobile phone"
#fp_message_socialm		"Exposure to family planning message by social media" - NEW Indicator in DHS8
#fp_message_poster		"Exposure to family planning message by poster/leaflet/brochure" - NEW Indicator in DHS8
#fp_message_signs		"Exposure to family planning message by outdoor sign/billboard" - NEW Indicator in DHS8
#fp_message_comnty		"Exposure to family planning message by community meetings or events" - NEW Indicator in DHS8
#fp_message_noneof8		"Not exposed to any of the eight media sources" - NEW Indicator in DHS8

#fp_dec_who				"Who makes the decision to use or not use family planning" - NEW Indicator in DHS8
#fp_dec_wm				"Woman participated in decisionmaking about family planning" - NEW Indicator in DHS8

#fp_preg_pressure		"Pressured by husbands/partners or other family to get pregnant when they did not want to" - NEW Indicator in DHS8
# 
# fp_decyes_user			"Who makes the decision to use family planning among users"
# fp_decno_nonuser		"Who makes decision not to use family planning among non-users"
# 
# fp_fpvisit_discuss	"Women non-users that were visited by a FP worker who discussed FP"
# fp_hf_discuss			  "Women non-users who visited a health facility in last 12 months and discussed FP"
# fp_hf_notdiscuss		"Women non-users who visited a health facility in last 12 months and did not discuss FP"
# fp_any_notdiscuss		"Women non-users who did not discuss FP neither with FP worker or in a health facility"
# ------------------------------------------------------------------------------
  

## FAMILY PLANNING MESSAGES

# Family planning messages by radio 
IRdata <- IRdata %>%
  mutate(fp_message_radio = 
           ifelse(v384a == 1, 1, 0)) %>%
  set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")


# Family planning messages by TV 
IRdata <- IRdata %>%
  mutate(fp_message_tv = 
           ifelse(v384b == 1, 1, 0)) %>%
  set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_paper = 
           ifelse(v384c == 1, 1, 0)) %>%
  set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_mobile = 
           ifelse(v384d == 1, 1, 0))%>%
  set_value_labels(fp_message_mobile = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_mobile = "Exposure to family planning message by mobile phone")


# Did not hear a family planning message from any of the 4 media sources
IRdata <- IRdata %>%
  mutate(fp_message_noneof4 =
           ifelse(v384a!=1 & v384b!=1 & v384c!=1 & v384d!=1, 1, 0))%>%
  set_value_labels(fp_message_noneof4 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof4 = "Exposure to family planning message any of four sources (TV, radio, paper, mobile)")


# Did not hear a family planning message from radio, TV or paper
IRdata <- IRdata %>%
  mutate(fp_message_noneof3 =
           ifelse(v384a!=1 & v384b!=1 & v384c!=1, 1, 0)) %>%
  set_value_labels(fp_message_noneof3 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof3 = "Not exposed to TV, radio, or paper media sources")



## FAMILY PLANNING DECISION MAKING AND DISCUSSION
  
# Decision to use among users
IRdata <- IRdata %>%
  mutate(fp_decyes_user =  
           ifelse((v502!=1 | v213!=0 | v312==0),NA,v632))  %>%
  set_value_labels(fp_decyes_user = val_labels(IRdata$v632)) %>%
  set_variable_labels(fp_decyes_user <-  "Who makes the decision to use family planning among users")

# Decision to not use among non users
IRdata <- IRdata %>%
  mutate(fp_decno_nonuser =  
           ifelse((v502!=1 | v213!=0 | v312!=0),NA,v632))%>%
  set_value_labels(fp_decno_nonuser = val_labels(IRdata$v632)) %>%
  set_variable_labels(fp_decno_nonuser = "Who makes decision not to use family planning among non-users")

# Discussed with FP worker
IRdata <- IRdata %>%
  mutate(fp_fpvisit_discuss =
           case_when(
             v393a==1 ~ 1,
             v312!=0 ~ NA))  %>%
      set_value_labels(fp_fpvisit_discuss = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_fpvisit_discuss = "Women non-users that were visited by a FP worker who discussed FP")

      
# Discussed FP in a health facility
IRdata <- IRdata %>%
  mutate(fp_hf_discuss = 
           case_when(
             v394==1 & v395==1 ~ 1,
             v312!=0 ~ NA)) %>%
  set_value_labels(fp_hf_discuss = c(yes=1, no=0)) %>%
  set_variable_labels(fp_hf_discuss = "Women non-users who visited a health facility in last 12 months and discussed FP")


# Did not discuss FP in health facility
IRdata <- IRdata %>%
  mutate(fp_hf_notdiscuss = 
           case_when(
             394==1 & v395!=1 ~ 1,
             v312!=0 ~ NA)) %>%
  set_value_labels(fp_hf_notdiscuss = c(yes=1, no=0)) %>%
  set_variable_labels(fp_hf_notdiscuss = "Women non-users who visited a health facility in last 12 months and did not discuss FP")


# Did not discuss FP in health facility or with FP worker
IRdata <- IRdata %>%
  mutate(fp_any_notdiscuss = 
           case_when(
            v393a!=1 & v395!=1 ~ 1,
            v312!=0 ~ NA)) %>%
  set_value_labels(fp_any_notdiscuss = c(yes=1, no=0)) %>%
  set_variable_labels(fp_any_notdiscuss = "Women non-users who did not discuss FP neither with FP worker or in a health facility")



#-------------------------------------------------------------------------------      


## indicators from MR file


## FAMILY PLANNING MESSAGES



# Family planning messages by radio 
IRdata <- IRdata %>%
  mutate(fp_message_radio = 
           ifelse(mv384a == 1, 1, 0)) %>%
  set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")


# Family planning messages by TV 
IRdata <- IRdata %>%
  mutate(fp_message_tv = 
           ifelse(mv384b == 1, 1, 0)) %>%
  set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_paper = 
           ifelse(mv384c == 1, 1, 0)) %>%
  set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")


# Family planning messages by newspaper and/or magazine 
IRdata <- IRdata %>%
  mutate(fp_message_mobile = 
           ifelse(v384d == 1, 1, 0))%>%
  set_value_labels(fp_message_mobile = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_mobile = "Exposure to family planning message by mobile phone")

# Family planning messages by social media - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_message_socialm = if_else(v384e == 1, 1, 0)) %>%
  set_value_labels(fp_message_socialm = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_socialm = "Exposure to family planning message by social media")


# Family planning messages by poster/leaflet/brochure - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_message_poster = if_else(v384f == 1, 1, 0)) %>%
  set_value_labels(fp_message_poster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_poster = "Exposure to family planning message by poster/leaflet/brochure")

# Family planning messages by outdoor sign/billboard - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_message_signs = if_else(v384g == 1, 1, 0)) %>%
  set_value_labels(fp_message_signs = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_signs = "Exposure to family planning message by outdoor sign/billboard")

# Family planning messages by community meetings or events - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_message_comnty = if_else(v384h == 1, 1, 0)) %>%
  set_value_labels(fp_message_comnty = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_comnty = "Exposure to family planning message by community meetings or events")

# Did not hear a family planning message from any of the eight media sources
IRdata <- IRdata %>%
  mutate(fp_message_noneof8 = if_else(
    v384a != 1 & v384b != 1 & v384c != 1 & v384d != 1 & v384e != 1 & v384f != 1 & v384g != 1 & v384h != 1, 
    1, 
    0
  )) %>%
  set_value_labels(fp_message_noneof8 = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_noneof8 = "Not exposed to any of the eight media sources")



#  Family Planning decision making and discussion  


# Who makes decision to use family planning - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_dec_who = case_when(
    v632 == 6 ~ 4,
    (v502 != 1 | v213 != 0) ~ NA,
    TRUE ~ v632
  )) %>%
  set_value_labels(fp_dec_who = val_labels(IRdata$v632)) %>%
  set_variable_labels(fp_dec_who = "Who makes the decision to use or not use family planning")

# Woman participated in decisionmaking about family planning - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_dec_wm = case_when(
    v632 == 1 | v632 == 3 ~ 1,
    (v502 != 1 | v213 != 0) ~ NA,
    TRUE ~ 0
  )) %>%
  set_value_labels(fp_dec_wm = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_dec_wm = "Woman participated in decisionmaking about family planning")

# Pressured to get pregnant - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(fp_preg_pressure = case_when(
    v636 == 1 & v502 == 1 ~ 1,  v502!=1 ~NA,
    TRUE ~ 0
  )) %>%
  set_value_labels(fp_preg_pressure = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_preg_pressure = "Pressured by husbands/partners or other family to get pregnant when they did not want to")



