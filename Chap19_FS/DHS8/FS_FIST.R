# /*****************************************************************************************************
# Program: 			  FS_FIST.R
# Purpose: 			  Code to compute fistula indicators among women
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: September 27, 2022 by Shireen Assaf 
# 
# Notes:				
# There are no standard variables names for these variables. 
# These variables will likely be included in the data file as country-specific variables. 
# Country-specific variables begin with an “s”. 
# There is some variation in the presentation of these results among final reports.

# This do file provides a guide to how the fistula indicators can be computed but it is necessary to check the country-specific variables 
# for each survey and adjust the code accordingly. 

# Change variable names to recode according to the survey: the variables below are for Afghanistan 2015 survey. 
# Use look_for {labelled} command to find fistula related variables as shown in the example below.  

#look_for(IRdata,"ever heard of fistula")
#this gives variable s1102 in the Afghanistan 2015 survey

# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:

# fs_heard		  "Ever heard of fistula"
# fs_ever_exp		"Ever experienced fistula"
# fs_current		"Currently have fistula"
# 	
# fs_cause		  "Reported cause of fistula"
# fs_days_symp	"Reported number of days since symptoms began"
# 	
# fs_trt_provid	  "Provider type for fistula treatment"
# fs_trt_outcome	"Outcome of treatment sought for fistula"
# fs_trt_operat	  "Had operation for fistula"
# fs_notrt_reason	"Reason for not seeking treatment"
# ----------------------------------------------------------------------------*/

# weight variable 
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# //Heard of fistula
# use variable labeled as "ever heard of fistula"
IRdata <- IRdata %>%
  mutate(fs_heard = 
           case_when(
             s1102==1 ~ 1 ,
             TRUE ~ 0 )) %>%
  set_value_labels(fs_heard = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fs_heard = "Ever heard of fistula")

# //Ever experienced fistula
# use variable labeled as "involuntary loss of urine and/or feces through the vagina"
IRdata <- IRdata %>%
  mutate(fs_ever_exp = 
           case_when(
             s1101==1 ~ 1 ,
             TRUE ~ 0 )) %>%
  set_value_labels(fs_ever_exp = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fs_ever_exp = "Ever experienced fistula")

# //Reported cause of fistula
# use two variables one labeled as "problem start after normal or difficult labor and delivery" here this is s1104 and "problem start after delivered a baby or had a stillbirth" here this is s1103
# you may need to add more country-specific categories here. To do this find a variable with the label "Reported cause of fistula"
IRdata <- IRdata %>%
  mutate(fs_cause = 
           case_when(
             s1104==1 & s1103==1 ~ 1 ,
             s1104==1 & s1103==2 ~ 2 ,
             s1104==2 & s1103==1 ~ 3 ,
             s1104==2 & s1103==2 ~ 4 ,
             s1103<3 ~ 9)) %>%
  set_value_labels(fs_cause = c("Normal labor and delivery, baby born alive" = 1, 
                                "Normal labor and delivery, baby stillborn"= 2 ,
                                "Very difficult labor and delivery, baby born" = 3, 
                                "Very difficult labor and delivery, baby stillborn"= 4,
                                "Missing" =9)) %>%
  set_variable_labels(fs_cause = "Reported cause of fistula")

# //Reported number of days since symptoms began
# find variable with label "days after problem leakage started" here this is s1106
IRdata <- IRdata %>%
  mutate(fs_ever_exp = 
           case_when(
             s1101==1 ~ 1 ,
             TRUE ~ 0 )) %>%
  set_value_labels(fs_ever_exp = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fs_ever_exp = "Ever experienced fistula")

# //Reported cause of fistula
# use two variables one labeled as "problem start after normal or difficult labor and delivery" here this is s1104 and "problem start after delivered a baby or had a stillbirth" here this is s1103
# you may need to add more country-specific categories here. To do this find a variable with the label "Reported cause of fistula"
IRdata <- IRdata %>%
  mutate(fs_days_symp = 
           case_when(
             s1106<2 ~ 1 ,
             inrange(s1106,2,4) ~ 2 ,
             inrange(s1106,5,7) ~ 3 ,
             inrange(s1106,8,90) ~ 4 ,
             s1103<3 ~ 9)) 
IRdata[["fs_days_symp"]] <- ifelse((is.na(IRdata$fs_cause)), NA, IRdata[["fs_days_symp"]]) 	  
IRdata <- IRdata %>%
set_value_labels(fs_days_symp = c("0-1 day" = 1 ,"2-4 days"= 2 ,"5-7 days"= 3 , 
                                  "8+ days"= 4,"Missing" =9)) %>%
set_variable_labels(fs_days_symp = "Reported number of days since symptoms began")

# //Provider type for fistula treatment
# * use two variables one labeled as "sought treatment for fistula" here this is s1107 and "from whom have you sought a treatment" here this is s1109
IRdata <- IRdata %>%
  mutate(fs_trt_provid = 
           case_when(
             is.na(s1109) & s1107==1  ~ 9, 
             s1109==6  ~ 4 ,
             s1109==3  ~ 3 ,
             s1109==2  ~ 2 ,
             s1109==1 ~ 1 ,
             s1107!=1 | (is.na(s1107) & s1101==1)~ 0))%>%
  set_value_labels(fs_trt_provid = c("No treatment" =0 ,"Doctor" = 1 ,"Nurse/Midwife"= 2 ,"Community/village health worker"= 3 , 
                                    "Other"= 4,"Missing" =9)) %>%
  set_variable_labels(fs_trt_provid = "Provider type for fistula treatment")

# //Outcome of treatment sought for fistula
# * find a variable with "treatment" or "leakage" in the label 
IRdata <- IRdata %>%
  mutate(fs_trt_outcome = 
           case_when(
             is.na(s1111) & s1107==1  ~ 9, 
             s1111==4  ~ 4 ,
             s1111==3  ~ 3 ,
             s1111==2  ~ 2 ,
             s1111==1 ~ 1 ))%>%
  set_value_labels(fs_trt_outcome = c("Leakage stopped completely" = 1 ,"Not stopped but reduced"= 2 ,"Not stopped at all"= 3 , 
                                     "Did not receive any treatment"= 4,"Missing" =9)) %>%
  set_variable_labels(fs_trt_outcome = "Outcome of treatment sought for fistula")

# //Had operation for fistula
IRdata <- IRdata %>%
  mutate(fs_trt_operat = 
           case_when(
             s1110==1 ~ 1 ,
             s1101==1 & s1107==1 ~ 0 )) %>%
  set_value_labels(fs_trt_operat = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fs_trt_operat = "Had operation for fistula")

# //Reason for not seeking treatment
IRdata <- IRdata %>%
  mutate(fs_notrt_reason = 
           case_when(
             s1108a==1 ~ 1 ,
             s1108b==1  ~ 2 ,
             s1108c==1  ~ 3 ,
             s1108g==1  ~ 4 ,
             s1108d==1 ~ 5,
             s1108h==1 ~ 6,
             s1108f==1 ~ 7,
             (s1108x==1 | s1108e==1)  ~ 8,
             s1101==1 & (s1107==0 |  is.na(s1107)) ~ 9))%>%
  set_value_labels(fs_notrt_reason = c("Did not know the problem can be fixed" = 1 ,"Did not know where to go"= 2 ,"Too expensive"= 3 , 
                                      "Embarrassment"= 4, "Too far"=5, "Problem disappeard"=6, "Could not get permission"=7, "Other" =8, "Missing" =9)) %>%
  set_variable_labels(fs_notrt_reason = "Reason for not seeking treatment")
