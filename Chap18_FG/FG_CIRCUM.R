# /*****************************************************************************************************
# Program: 			FG_CIRCUM.R
# Purpose: 			Code  to compute female circumcision indicators indicators among women and knowledge and opinion on female circumcision among men
# Data inputs: 	IR or MR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: September 19, 2022 by Shireen Assaf 
# 
# Note:			Heard of female circumcision and opinions on female circumcision can be computed for men and women
# 					In older surveys there may be alternative variable names related to female circumcision. 
# 					Please check Chapter 18 in Guide to DHS Statistics and the section "Changes over Time" to find alternative names.
# 					Link:	https://www.dhsprogram.com/Data/Guide-to-DHS-Statistics/index.htm#t=Knowledge_of_Female_Circumcision.htm%23Percentage_of_women_and8bc-1&rhtocid=_21_0_0
# 					
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# fg_heard			  "Heard of female circumcision"
# 	
# fg_fcircum_wm		"Circumcised among women age 15-49"
# fg_type_wm			"Type of female circumcision among women age 15-49"
# fg_age_wm			  "Age at circumcision among women age 15-49"
# fg_sewn_wm			"Female circumcision type is sewn closed among women age 15-49"	
# fg_who_wm			  "Person who performed the circumcision among women age 15-49"
# 	
# fg_relig			  "Opinion on whether female circumcision is required by their religion" 
# fg_cont				  "Opinion on whether female circumcision should continue" 
# ----------------------------------------------------------------------------*/
# 
# indicators from IR file

# weight variable 
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# //Heard of female circumcision
IRdata <- IRdata %>%
  mutate(fg_heard = 
           case_when(
             g100==1 | g101==1 ~ 1 ,
             g100==0 & g101==0 ~ 0 )) %>%
  set_value_labels(fg_heard = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fg_heard = "Heard of female circumcision")

# //Circumcised women
IRdata <- IRdata %>%
  mutate(fg_fcircum_wm = 
           case_when(
             g102==1 ~ 1 ,
             g102==0 | g100==0 | g100==1 ~ 0)) %>%
  set_value_labels(fg_fcircum_wm = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fg_fcircum_wm = "Circumcised among women age 15-49")

# //Type of circumcision
IRdata <- IRdata %>%
  mutate(fg_type_wm = 
           case_when(
             g104==1 & g105!=1 ~ 1,
             g103==1 & g105!=1 ~ 2,
             g105==1 ~ 3 ,
             g102==1 ~ 9 )) %>%
  set_value_labels(fg_type_wm = c("No flesh removed" = 1, "Flesh removed"=2, "Sewn closed"=3,"Don't know/missing"=9 )) %>%
  set_variable_labels(fg_type_wm = "Type of female circumcision among women age 15-49")

# //Age at circumcision
IRdata <- IRdata %>%
  mutate(fg_age_wm = 
           case_when(
             inrange(g106,0,4) | g106==95 ~ 1,
             inrange(g106,5,9)  ~ 2,
             inrange(g106,10,14)  ~ 3 ,
             inrange(g106,15,49) ~ 4 ,
             g102==1 | g106==98~ 9 )) %>%
  set_value_labels(fg_age_wm = c("<5" = 1, "5-9"=2, "10-14"=3, "15+"=4, "Don't know/missing"=9 )) %>%
  set_variable_labels(fg_age_wm = "Age at circumcision among women age 15-49")

# //Sewn close
IRdata <- IRdata %>%
  mutate(fg_sewn_wm = 
           case_when(
             g105==1 & g102==1 ~ 1 ,
             g105!=1 & g102==1  ~ 0)) %>%
  set_value_labels(fg_sewn_wm = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fg_sewn_wm = "Female circumcision type is sewn closed among women age 15-49")

# //Person performing the circumcision among women age 15-49
IRdata <- IRdata %>%
  mutate(fg_who_wm = 
           case_when(
             g107==21 ~ 1, g107==22 ~ 2, g107==26 ~ 3 ,
             g107==11 ~ 4,  g107==12 ~ 5, g107==16 ~ 6 ,
             g107==96 ~ 7, g107>=98 ~ 9)) %>%
  set_value_labels(fg_who_wm = c("traditional circumciser" = 1, "traditional birth attendant"=2, 
                                 "other traditional agent"=3, "doctor"=4, "nurse/midwife"=5,
                                 "other health professional"=6, "other"=7, "Don't know/missing"=9 )) %>%
  set_variable_labels(fg_who_wm = "Person who performed the circumcision among women age 15-49")

# //Opinion on whether female circumcision is required by their religion
IRdata <- IRdata %>%
  mutate(fg_relig = 
           case_when(
             g118==1 & fg_heard==1 ~ 1 ,
             g118==0 & fg_heard==1 ~ 0,
             g118==3 & fg_heard==1 ~ 2,
             g118>=8 & fg_heard==1 ~ 9)) %>%
  set_value_labels(fg_relig = c("No religion"=2, "Yes" = 1, "No"=0, "Don't know/missing"=9 )) %>%
  set_variable_labels(fg_relig = "Opinion on whether female circumcision is required by their religion")
 
# //Opinion on whether female circumcision should continue
IRdata <- IRdata %>%
  mutate(fg_cont = 
           case_when(
             g119==1 & fg_heard==1 ~ 1 ,
             g119==2 & fg_heard==1 ~ 0,
             g119>=3 & fg_heard==1 ~ 9)) %>%
  set_value_labels(fg_cont = c("Yes continue" = 1, "No, not continue"=0, "Don't know/depends/missing"=9 )) %>%
  set_variable_labels(fg_cont = "Opinion on whether female circumcision should continue")

######################################################################################

# indicators from MR file

# weight variable 
MRdata <- MRdata %>%
  mutate(wt = mv005/1000000)
 
# //Heard of female circumcision
MRdata <- MRdata %>%
  mutate(fg_heard = 
           case_when(
             mg100==1 | mg101==1 ~ 1 ,
             mg100==0 & mg101==0 ~ 0 )) %>%
  set_value_labels(fg_heard = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fg_heard = "Heard of female circumcision")

# //Opinion on whether female circumcision is required by their religion
MRdata <- MRdata %>%
mutate(fg_relig = 
         case_when(
           mg118==1 & fg_heard==1 ~ 1 ,
           mg118==0 & fg_heard==1 ~ 0,
           mg118==3 & fg_heard==1 ~ 2,
           mg118>=8 & fg_heard==1 ~ 9)) %>%
  set_value_labels(fg_relig = c("No religion"=2, "Yes" = 1, "No"=0, "Don't know/missing"=9 )) %>%
  set_variable_labels(fg_relig = "Opinion on whether female circumcision is required by their religion")

# //Opinion on whether female circumcision should continue
MRdata <- MRdata %>%
  mutate(fg_cont = 
           case_when(
             mg119==1 & fg_heard==1 ~ 1 ,
             mg119==2 & fg_heard==1 ~ 0,
             mg119>=3 & fg_heard==1 ~ 9)) %>%
  set_value_labels(fg_cont = c("Yes continue" = 1, "No, not continue"=0, "Don't know/depends/missing"=9 )) %>%
  set_variable_labels(fg_cont = "Opinion on whether female circumcision should continue")
