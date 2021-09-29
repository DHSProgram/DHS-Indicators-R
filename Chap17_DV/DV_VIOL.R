# /*****************************************************************************************************
# Program: 			DV_viol.R
# Purpose: 			Code domestic violence indicators from the IR file
# Data inputs: 	IR dataset
# Data outputs:	coded variables
# Author:				Courtney Allen and translated to R by Shireen Assaf 
# Date last modified: September 16, 2021 by Shireen Assaf
# *****************************************************************************************************/
# 
# ______________________________________________________________________________
# Variables created in this file:
# 
# //EXPERIENCE OF PHYSICAL, SEXUAL, EMOTIONAL VIOLENCE
# 	dv_phy				  "Experienced physical violence since age 15"
# 	dv_phy_12m		  "Experienced physical violence in the past 12 months"
# 	dv_phy_preg		  "Experienced physical violence during pregnancy"
# 	dv_sex				  "Ever experienced sexual violence"
# 	dv_sex_12m		  "Experienced sexual violence in the past 12 months"
# 	dv_sex_age		  "Specific age experienced sexual violence"
# 	dv_phy_only		  "Experienced physical violence only"
# 	dv_sex_only		  "Experienced sexual violence only"
# 	dv_phy_sex_all  "Experienced physical and sexual violence"
# 	dv_phy_sex_any	"Experienced physical or sexual violence"
# 	dv_viol_type		"Experienced physical only, sexual only, or both"
# 
# //PERSONS COMMITTING PHYSICAL OR SEXUAL VIOLENCE
# 	dv_phy_hus_curr		  "Person committing physical violence: current husband/partner"
# 	dv_phy_hus_form		  "Person committing physical violence: former husband/partner"
# 	dv_phy_bf_curr		  "Person committing physical violence: current boyfriend"
# 	dv_phy_bf_form		  "Person committing physical violence: former boyfriend"
# 	dv_phy_father		    "Person committing physical violence: father/step-father"
# 	dv_phy_mother		    "Person committing physical violence: mother/step-mother"
# 	dv_phy_sibling		  "Person committing physical violence: sister or bother"
# 	dv_phy_bychild		  "Person committing physical violence: daughter/son"
# 	dv_phy_other_rel	  "Person committing physical violence: other relative"
# 	dv_phy_mother_inlaw	"Person committing physical violence: mother-in-law"
# 	dv_phy_father_inlaw	"Person committing physical violence: father-in-law"
# 	dv_phy_other_inlaw	"Person committing physical violence: other-in-law"
# 	dv_phy_teacher		  "Person committing physical violence: teacher"
# 	dv_phy_atwork		    "Person committing physical violence: employer/someone at work"
# 	dv_phy_police		    "Person committing physical violence: police/soldier"
# 	dv_phy_other		    "Person committing physical violence: other"
# 	
# 	dv_sex_hus_curr		  "Person committing sexual violence: current husband/partner"
# 	dv_sex_hus_form		  "Person committing sexual violence: former husband/partner"
# 	dv_sex_bf			      "Person committing sexual violence: current/former boyfriend"
# 	dv_sex_father		    "Person committing sexual violence: father/step-father"
# 	dv_sex_brother		  "Person committing sexual violence: brother/step-brother"
# 	dv_sex_other_rel	  "Person committing sexual violence: other relative"
# 	dv_sex_inlaw		    "Person committing sexual violence: in-law"
# 	dv_sex_friend		    "Person committing sexual violence: friend/acquaintance"
# 	dv_sex_friend_fam	  "Person committing sexual violence: family friend"
# 	dv_sex_teacher		  "Person committing sexual violence: teacher"
# 	dv_sex_atwork		    "Person committing sexual violence: employer/someone at work"
# 	dv_sex_relig		    "Person committing sexual violence: priest or religious leader"
# 	dv_sex_police		    "Person committing sexual violence: police/soldier"
# 	dv_sex_stranger		  "Person committing sexual violence: stranger"
# 	dv_sex_other		    "Person committing sexual violence: other"
# 	dv_sex_missing		  "Person committing sexual violence: missing"			
# 	
# //SEEKING HELP AFTER VIOLENCE	
# 	dv_help_seek		    "Help-seeking behavior of women who ever experienced physical or sexual violence"
# 	dv_help_phy			    "Sources of help sought for physical violence among women who sought help"
# 	dv_help_sex			    "Sources of help sought for sexual violence among women who sought help"
# 	dv_help_phy_sex_all	"Sources of help sought for physical and sexual violence among women who sought help"
# 	dv_help_phy_sex_any	"Sources of help sought for physical or sexual violence among women who sought help"
# 	dv_help_fam 		    "Source of help: own family"
# 	dv_help_hfam 		    "Source of help: husband's family"
# 	dv_help_husb        "Source of help: husband"
# 	dv_help_bf 			    "Source of help: boyfriend"
# 	dv_help_friend 	    "Source of help: friend"
# 	dv_help_neighbor    "Source of help: neighbor"
# 	dv_help_relig 	    "Source of help: religious"
# 	dv_help_doc         "Source of help: doctor"
# 	dv_help_police      "Source of help: police"
# 	dv_help_lawyer      "Source of help: lawyer"
# 	dv_help_sw          "Source of help: social worker"
# 	dv_help_other       "Source of help: other"
#  ______________________________________________________________________________


# EXPERIENCED PHYSICAL VIOLENCE

# //Ever
IRdata <- IRdata %>%
  mutate(dv_phy =
           case_when(
             d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0 ~ 1, # violence by current partner
             d130a>0  ~ 1,  # violence by former partner
             d115y==0 ~ 1, # violence by anyone other than partner
             d118y==0 ~ 1, # violence during pregnancy 
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy = "Experienced physical violence since age 15")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_phy_12m =
           case_when(
             d105a %in% c(1,2) | d105b %in% c(1,2) | d105c %in% c(1,2) | d105d%in% c(1,2) | 
             d105e %in% c(1,2) | d105f %in% c(1,2) | d105g %in% c(1,2) |d105j %in% c(1,2) ~ 1, 
             d117a==1 | d117a==2 | d130a==1 ~ 1,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_12m = "Experienced physical violence in the past 12 mos")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_phy_12m_f =
           case_when(
             d105a==1 | d105b==1 | d105c==1 | d105d==1 | d105e==1 | d105f==1 | d105g==1 |d105j==1 | d117a==1 ~ 1 ,
             d105a==2 | d105b==2 | d105c==2 | d105d==2 | d105e==2 | d105f==2 | d105g==2 |d105j==2 | d117a==2 ~ 2 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_12m_f = "Experienced physical violence in the past 12 mos, frequency")

#//Physical violence during pregnancy
IRdata <- IRdata %>%
  mutate(dv_phy_preg =
           case_when(
             d118y==0 ~ 1, 
             v044==1  & (v201>0 | v213==1 | v228==1) ~ 0 )) %>% 
  set_value_labels(dv_phy_preg = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_preg = "Experienced physical violence during pregnancy")

# **PERSONS COMMITTING PHYSICAL VIOLENCE ** #

# //Current partner
IRdata <- IRdata %>%
  mutate(dv_phy_hus_curr =
           case_when(
             v502==1 & (d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0 | d118a==1) ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_hus_curr = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_hus_curr = "Person committing physical violence: current husband/partner")
 
# //Former partner
IRdata <- IRdata %>%
  mutate(dv_phy_hus_form =
           case_when(
             v502>0 & (d115j==1 | d118j==1 | d130a>0) ~ 1, 
             v502==2 & (d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0) ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_hus_form = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_hus_form = "Person committing physical violence: former husband/partner")

# //Current boyfriend
IRdata <- IRdata %>%
  mutate(dv_phy_bf_curr =
           case_when(
             d115k==1 | d118k==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_bf_curr = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_bf_curr = "Person committing physical violence: current boyfriend")

# //Former boyfriend
IRdata <- IRdata %>%
  mutate(dv_phy_bf_form =
           case_when(
             d115l==1 | d118l==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_bf_form = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_bf_form = "Person committing physical violence: former boyfriend")

# //Father step-father
IRdata <- IRdata %>%
  mutate(dv_phy_father =
           case_when(
             d115c==1 | d118c==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_father = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_father = "Person committing physical violence: father/step-father")

# //Mother or step-mother
IRdata <- IRdata %>%
  mutate(dv_phy_mother =
           case_when(
             d115b==1 | d118b==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_mother = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_mother = "Person committing physical violence: mother/step-mother")

# //Sister or brother
IRdata <- IRdata %>%
  mutate(dv_phy_sibling =
           case_when(
             d115f==1 | d118f==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_sibling = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sibling = "Person committing physical violence: sister or bother")

# //Daughter or son
IRdata <- IRdata %>%
  mutate(dv_phy_bychild =
           case_when(
             d115d==1 | d118d==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_bychild = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_bychild = "Person committing physical violence: daughter or son")

# //Other relative
IRdata <- IRdata %>%
  mutate(dv_phy_other_rel =
           case_when(
             d115g==1 | d118g==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_other_rel = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_other_rel = "Person committing physical violence: other relative")

# //Mother-in-law
IRdata <- IRdata %>%
  mutate(dv_phy_mother_inlaw =
           case_when(
             d115o==1 | d118o==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_mother_inlaw = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_mother_inlaw = "Person committing physical violence: mother-in-law")

# //Father-in-law
IRdata <- IRdata %>%
  mutate(dv_phy_father_inlaw =
           case_when(
             d115p==1 | d118p==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_father_inlaw = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_father_inlaw = "Person committing physical violence: father-in-law")

# //Other-in-law
IRdata <- IRdata %>%
  mutate(dv_phy_other_inlaw =
           case_when(
             d115q==1 | d118q==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_other_inlaw = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_other_inlaw = "Person committing physical violence: other-in-law")

# //Teacher
IRdata <- IRdata %>%
  mutate(dv_phy_teacher =
           case_when(
             d115v==1 | d118v==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_teacher = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_teacher = "Person committing physical violence: teacher")

# //Employer/someone at work
IRdata <- IRdata %>%
  mutate(dv_phy_atwork =
           case_when(
             d115w==1 | d118w==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_atwork = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_atwork = "Person committing physical violence: employer/someone at work")

# //Police/soldier
IRdata <- IRdata %>%
  mutate(dv_phy_police =
           case_when(
             d115xe==1 | d118xe==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_police = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_police = "Person committing physical violence: police/soldier")

# //Other
IRdata <- IRdata %>%
  mutate(dv_phy_other =
           case_when(
             d115x==1 | d118x==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_other = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_other = "Person committing physical violence: other")

## **EXPERIENCED SEXUAL VIOLENCE ##

# //Ever 
IRdata <- IRdata %>%
  mutate(dv_sex =
           case_when(
             d105h>0 | d105i>0 | d105k>0  ~ 1, # violence by current partner
             d130b>0  ~ 1,  # violence by former partner
             d124==1  ~ 1, # violence by anyone other than partner
             d125==1 ~ 1, # forced to perform unwanted acts 
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex = "Ever experienced sexual violence")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_sex_12m =
           case_when(
             d105h %in% c(1,2) | d105i %in% c(1,2) | d105k %in% c(1,2)  ~ 1, 
             d130b==1 | d124==1  ~ 1,  
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_12m = "Experienced sexual violence in past 12 mos")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_sex_12m_f =
           case_when(
             d105h==1 | d105i==1 | d105k==1 | d117a==1   ~ 1 ,
             d105h==2 | d105i==2 | d105k==2 | d117a==2 ~ 2 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_12m_f = "Experienced sexual violence in the past 12 mos, frequency")

# **EXPERIENCED PHYSICAL AND SEXUAL VIOLENCE
# //Ever
IRdata <- IRdata %>%
  mutate(dv_phy_sex =
           case_when(
             dv_phy==1 & dv_sex==1 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_sex = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sex = "Ever experienced physical AND sexual violence")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_phy_sex_12m =
           case_when(
             dv_phy_12m==1 & dv_sex_12m==1 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sex_12m = "Experienced physical AND sexual violence in the last 12 months")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_phy_sex_12m_f =
           case_when(
             dv_phy_12m==1 & dv_sex_12m==1  ~ 1 ,
             dv_phy_12m==2 & dv_sex_12m==2 ~ 2 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_sex_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sex_12m_f = "Experienced physical AND sexual violence in the last 12 months, frequency")

# **EXPERIENCED PHYSICAL OR SEXUAL VIOLENCE
# //Ever
IRdata <- IRdata %>%
  mutate(dv_phy_sex_any =
           case_when(
             dv_phy==1 | dv_sex==1 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_sex_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sex_any = "Ever experienced physical OR sexual violence")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_phy_sex_any_12m =
           case_when(
             dv_phy_12m==1 | dv_sex_12m==1 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_sex_any_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sex_any_12m = "Ever experienced physical OR sexual violence in the last 12 months")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_phy_sex_any_12m_f =
           case_when(
             dv_phy_12m==1 | dv_sex_12m==1  ~ 1 ,
             dv_phy_12m==2 | dv_sex_12m==2 ~ 2 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_sex_any_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_sex_any_12m_f = "Experienced physical OR sexual violence in the last 12 months, frequency")

# //Which type
IRdata <- IRdata %>%
  mutate(dv_viol_type =
           case_when(
             dv_phy==1 & dv_sex==0 ~ 1 ,
             dv_phy==0 & dv_sex==1 ~ 2 ,
             dv_phy==1 & dv_sex==1 ~ 3 ,
             dv_phy_sex_any==1  ~ 0 )) %>% 
  set_value_labels(dv_viol_type = c("Both"=3, "Sexual only"=2, "Physical only" = 1, "None"=0)) %>%
  set_variable_labels(dv_viol_type = "Ever experienced physical only, sexual only, or both")

# //Physical only
IRdata <- IRdata %>%
  mutate(dv_phy_only =
           case_when(
             dv_phy==1 & dv_sex==0 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_only = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_only = "Ever experienced only physical violence")

# //Sexual only
IRdata <- IRdata %>%
  mutate(dv_sex_only =
           case_when(
             dv_phy==0 & dv_sex==1 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_only = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_only = "Ever experienced only sexual violence")

# **AGE EXPERIENCED SEXUAL VIOLENCE ** #

# //By age 10
IRdata <- IRdata %>%
  mutate(dv_sex_age_10 =
           case_when(
             d126<10 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_age_10 = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_age_10 = "First experienced sexual violence by age 10")

# //By age 12
IRdata <- IRdata %>%
  mutate(dv_sex_age_12 =
           case_when(
             d126<12 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_age_12 = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_age_12 = "First experienced sexual violence by age 12")

# //By age 15
IRdata <- IRdata %>%
  mutate(dv_sex_age_15 =
           case_when(
             d126<15 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_age_15 = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_age_15 = "First experienced sexual violence by age 15")

# //By age 18
IRdata <- IRdata %>%
  mutate(dv_sex_age_18 =
           case_when(
             d126<18 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_age_18 = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_age_18 = "First experienced sexual violence by age 18")

# //By age 22
IRdata <- IRdata %>%
  mutate(dv_sex_age_22 =
           case_when(
             d126<22 ~ 1 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_sex_age_22 = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_age_22 = "First experienced sexual violence by age 22")

# **PERSONS COMMITTING SEXUAL VIOLENCE ** #

# //Current partner
IRdata <- IRdata %>%
  mutate(dv_sex_hus_curr =
           case_when(
             v502==1 & (d105h>0 | d105i>0 |d105k>0 | d127==1) ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_hus_curr = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_hus_curr = "Person committing sexual violence: current husband/partner")

# //Former partner
IRdata <- IRdata %>%
  mutate(dv_sex_hus_form =
           case_when(
             v502>0 & d130b>0 ~ 1, 
             v502==1 & d127==2 & v503>1 ~ 1, 
             v502==2 & (d105h>0 | d105i>0 |d105k>0 | d127==2) ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_hus_form = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_hus_form = "Person committing sexual violence: former husband/partner")

# //Current or former boyfriend
IRdata <- IRdata %>%
  mutate(dv_sex_bf =
           case_when(
             (v502==1 & d127==2 & v503==1) | (v502==0 & (d127==1 | d127==2))~ 1, 
             d127==3 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_bf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_bf = "Person committing sexual violence: current/former boyfriend")

# //Father step-father
IRdata <- IRdata %>%
  mutate(dv_sex_father =
           case_when(
             d127==4 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_father = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_father = "Person committing sexual violence: father/step-father")

# //Brother
IRdata <- IRdata %>%
  mutate(dv_sex_brother =
           case_when(
             d127==5 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_brother = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_brother = "Person committing sexual violence: bother")

# //Other relative
IRdata <- IRdata %>%
  mutate(dv_sex_other_rel =
           case_when(
             d127==6 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_other_rel = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_other_rel = "Person committing sexual violence: other relative")

# //In-law
IRdata <- IRdata %>%
  mutate(dv_sex_inlaw =
           case_when(
             d127==7 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_inlaw = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_inlaw = "Person committing sexual violence: an in-law")

# //Friend or acquaintance
IRdata <- IRdata %>%
  mutate(dv_sex_friend =
           case_when(
             d127==8 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_friend = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_friend = "Person committing sexual violence: own friend/acquaintance")

# //Friend of the family
IRdata <- IRdata %>%
  mutate(dv_sex_friend_fam =
           case_when(
             d127==9 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_friend_fam = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_friend_fam = "Person committing sexual violence: a family friend")

# //Teacher
IRdata <- IRdata %>%
  mutate(dv_sex_teacher =
           case_when(
             d127==10 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_teacher = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_teacher = "Person committing sexual violence: teacher")

# //Employer/someone at work
IRdata <- IRdata %>%
  mutate(dv_sex_atwork =
           case_when(
             d127==11 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_atwork = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_atwork = "Person committing sexual violence: employer/someone at work")

# //Police/soldier
IRdata <- IRdata %>%
  mutate(dv_sex_police =
           case_when(
             d127==12 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_police = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_police = "Person committing sexual violence: police/soldier")

# //Priest/religious leader
IRdata <- IRdata %>%
  mutate(dv_sex_relig =
           case_when(
             d127==13 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_relig = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_relig = "Person committing sexual violence: a priest or religious leader")

# //Stranger
IRdata <- IRdata %>%
  mutate(dv_sex_stranger =
           case_when(
             d127==14 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_stranger = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_stranger = "Person committing sexual violence: stranger")

# //Other
IRdata <- IRdata %>%
  mutate(dv_sex_other =
           case_when(
             d127==96 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_other = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_other = "Person committing sexual violence: other")

# //Missing
IRdata <- IRdata %>%
  mutate(dv_sex_missing =
           case_when(
             d127==99 ~ 1 ,
             dv_sex==1 ~ 0 )) %>% 
  set_value_labels(dv_sex_missing = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_sex_missing = "Person committing sexual violence: missing")

# ********************************************************************************
# **Seeking help after violence
# ********************************************************************************

# //Sought help
IRdata <- IRdata %>%
  mutate(dv_help_seek =
           case_when(
             d119y==0 ~ 1 ,
             d119y==1 & d128==1 ~ 2 ,
             d119y==1 & d128==0 ~ 3,
             dv_phy_sex_any==1 ~ 0 )) %>% 
  set_value_labels(dv_help_seek = c("Sought help"=1, "Didn't seek help, told someone"=2, "Didn't seek help, didn't tell someone"=3)) %>%
  set_variable_labels(dv_help_seek = "Sought help to stop violence")

# //Sources of help: own family
IRdata <- IRdata %>%
  mutate(dv_help_fam =
           case_when(
             d119b>0 | d119c>0 | d119d>0 | d119e>0 | d119f>0 | d119g>0 | d119h>0 |d119m>0 | d119n>0  ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_fam = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_fam = "Sought help from own family")

# //Sources of help: husband's family
IRdata <- IRdata %>%
  mutate(dv_help_hfam =
           case_when(
             d119i>0 | d119o>0 | d119p>0 | d119q>0 | d119r>0   ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_hfam = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_hfam = "Sought help from husband's family")

# //Sources of help: husband
IRdata <- IRdata %>%
  mutate(dv_help_husb =
           case_when(
             d119j>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_husb = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_husb = "Sought help from husband")

# //Sources of help: boyfriend
IRdata <- IRdata %>%
  mutate(dv_help_bf =
           case_when(
             d119k>0 | d119l>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_bf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_bf = "Sought help from boyfriend")

# //Sources of help: friend
IRdata <- IRdata %>%
  mutate(dv_help_friend =
           case_when(
             d119s>0 | d119t>0 | d119xd>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_friend = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_friend = "Sought help from friend")

# //Sources of help: neighbor
IRdata <- IRdata %>%
  mutate(dv_help_neighbor =
           case_when(
             d119u>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_neighbor = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_neighbor = "Sought help from neighbor")

# //Sources of help: religious leader
IRdata <- IRdata %>%
  mutate(dv_help_relig =
           case_when(
             d119xf>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_relig = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_relig = "Sought help from religious leader")

# //Sources of help: doctor or medical personnel
IRdata <- IRdata %>%
  mutate(dv_help_doc =
           case_when(
             d119xh>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_doc = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_doc = "Sought help from doctor or medical personnel")

# //Sources of help: police
IRdata <- IRdata %>%
  mutate(dv_help_police =
           case_when(
             d119xe>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_police = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_police = "Sought help from police")

# //Sources of help: lawyer
IRdata <- IRdata %>%
  mutate(dv_help_lawyer =
           case_when(
             d119xg>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_lawyer = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_lawyer = "Sought help from lawyer")

# //Sources of help: social work organization
IRdata <- IRdata %>%
  mutate(dv_help_sw =
           case_when(
             d119xb>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_sw = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_sw = "Sought help from social work organization")

# //Sources of help: other
IRdata <- IRdata %>%
  mutate(dv_help_other =
           case_when(
             d119v>0 | d119w>0 | d119x>0 | d119xa>0 | d119xc>0 | d119xi>0 | d119xj>0 | d119xk>0 ~ 1 ,
             dv_help_seek==1 ~ 0 )) %>% 
  set_value_labels(dv_help_other = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_help_other = "Sought help from other")
