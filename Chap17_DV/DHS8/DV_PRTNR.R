# /*****************************************************************************************************
# Program: 			  DV_prtnr.R
# Purpose: 			  Code domestic violence for spousal violence indicators from the IR file
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen and translated to R by Shireen Assaf 
# Date last modified: September 17, 2021
# *****************************************************************************************************/

# ______________________________________________________________________________
# Variables created in this file:
# 	
# //CURRENT PARTNER VIOLENCE	
# 	----------------------------------------------------------------------------
# 	*each indicator in this series has an additional 2 variables with the suffixes
# 	"12m"	indicates event occurred in last 12 months, and 
# 	"12m_f" indicates a new variable that describes if the event that occurred 
# 	often or sometimes.*
# 	
# 	For example:
# 	dv_prtnr_slap				"Ever slapped by partner"
# 	dv_prtnr_slap_12m			"Slapped in past 12 mos. by partner"
# 	dv_prtnr_slap_12m_f		"Slapped in past 12 mos. by partner, frequency"
# 	----------------------------------------------------------------------------
# 
# 	//physical
# 	dv_prtnr_phy	  
# 	dv_prtnr_push				"Ever pushed, shook, or had something thrown at her in past 12 mos. by partner"
# 	dv_prtnr_slap				"Ever slapped by partner"
# 	dv_prtnr_twist			"Ever had arm twisted or hair pulled mos. by partner"
# 	dv_prtnr_punch			"Ever punched with first or something else that could hurt her by partner"
# 	dv_prtnr_kick				"Ever kicked, dragged, or beat up by partner"
# 	dv_prtnr_choke			"Ever tried to choke or burn her by partner"
# 	dv_prtnr_weapon			"Ever threatened or attacked with a knife, gun, or other weapon by partner"
# 		 
# 	//sexual
# 	dv_prtnr_sex				"Ever experienced any sexual violence by partner"
# 	dv_prtnr_force			"Physically forced to have sex when she did not want to by partner"
# 	dv_prtnr_force_act	"Physically forced to perform other sexual acts when she did not want to by partner"
# 	dv_prtnr_threat_act	"Forced with threats or in any other way to perform sexual acts she did not want to by partner"
# 		
# 	//emotional
# 	dv_prtnr_emot				"Any emotional violence by partner"
# 	dv_prtnr_humil			"Humiliated in front of others by partner"
# 	dv_prtnr_threat			"Threatened to hurt or harm her or someone she cared about by partner"
# 	dv_prtnr_insult			"Insulted or made to feel bad about herself by partner"
# 		
# 	//combinations of violence (combinations do not need a _freq variable)
# 	dv_prtnr_phy_sex			"Ever experienced physical AND sexual violence by partner"
# 	dv_prtnr_phy_sex_emot		"Ever experienced physical AND sexual AND emotional violence by partner"
# 	dv_prtnr_phy_sex_any		"Ever experienced physical OR sexual violence by partner"
# 	dv_prtnr_phy_sex_emot_any	"Ever experienced physical OR sexual OR emotional violence by partner"
# 				
# //ANY PARTNER VIOLENCE	
# 	dv_aprtnr_phy				"Experienced physical by any partner"
# 	dv_aprtnr_sex				"Experienced sexual violence by any partner"
# 	dv_aprtnr_emot			"Experienced emotional violence by any partner"
# 	dv_aprtnr_phy_sex		"Experienced physical and sexual violence by any partner"
# 	dv_aprtnr_phy_sex_any	"Ever experienced physical OR sexual violence by any partner"
# 	dv_aprtnr_phy_sex_emot		"Ever experienced physical AND sexual AND emotional violence by any partner"
# 	dv_aprtnr_phy_sex_emot_any	"Ever experienced physical OR sexual OR emotional violence by any partner"
# 		
# 	dv_prtnr_viol_years	"Experience of physical or sexual violence by partner by specific exact years since marriage - among women only married once"
# 	dv_prtnr_viol_none	"Did not experience physical or sexual violence by partner - among women only married once"
# 	
# //VIOLENCE BY MARRIAGE DURATION
# 	dv_mar_viol_0		"Exp"erience of violence by exact marriage duration: before marriage"
# 	dv_mar_viol_2		"Exp"erience of violence by exact marriage duration: 2 yrs of marriage"
# 	dv_mar_viol_5		"Exp"erience of violence by exact marriage duration: 5 yrs of marriage"
# 	dv_mar_viol_10		"Exp"erience of violence by exact marriage duration: 10 yrs of marriage"
# 
# //INITIATION OF SPOUSAL VIOLENCE BY WOMEN
# 	dv_prtnr_cuts		"Have cuts, bruises, or aches as a result of the violence by partner"
# 	dv_prtnr_injury		"Have eye injuries, sprains, dislocations, or burns as a result of the violence by partner"
# 	dv_prtnr_broken		"Deep wounds, broken bones, broken teeth, or any other serious injury as a result of the violence by partner"
# 	dv_prtnr_injury_any	"Have any injury as a result of the violence by partner"
# 		
# 	dv_abusedhus_phy	  "Ever committed physical violence against partner when he was not already beating or physically hurting her"
# 	dv_abusedhus_phy_12m "Committed physical violence against partner in past 12 mos. when he was not already beating or physically hurting her"
# 
# _//SEEKING HELP AFTER VIOLENCE	
# 	dv_help_seek		"Help-seeking behavior of women who ever experienced physical or sexual violence"
# 	dv_help_phy			"Sources of help sought for physical violence among women who sought help"
# 	dv_help_sex			"Sources of help sought for sexual violence among women who sought help"
# 	dv_help_phy_sex_all	"Sources of help sought for physical and sexual violence among women who sought help"
# 	dv_help_phy_sex_any	"Sources of help sought for physical or sexual violence among women who sought help"
# 	
# 	
# 	dv_help_fam 		"Source of help: own family"
# 	dv_help_hfam 		"Source of help: husband's family"
# 	dv_help_husb    "Source of help: husband"
# 	dv_help_bf 			"Source of help: boyfriend"
# 	dv_help_friend 		"Source of help: friend"
# 	dv_help_neighbor 	"Source of help: neighbor"
# 	dv_help_relig 		"Source of help: religious"
# 	dv_help_doc       "Source of help: doctor"
# 	dv_help_police    "Source of help: police"
# 	dv_help_lawyer    "Source of help: lawyer"
# 	dv_help_sw        "Source of help: social worker"
# 	dv_help_other     "Source of help: other"
# _____________________________________________________________________________

# ********************************************************************************	
# **Current partner physical violence, by types of violence
# ********************************************************************************	

# //ANY PARTNER VIOLENCE

# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy =
           case_when(
             d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy = "Any physical violence by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_12m =
           case_when(
             d105a %in% c(1,2) | d105b %in% c(1,2) | d105c %in% c(1,2) | d105d%in% c(1,2) | 
             d105e %in% c(1,2) | d105f %in% c(1,2) | d105g %in% c(1,2) |d105j %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_12m = "Any physical violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_12m_f =
           case_when(
             d105a==1 | d105b==1 | d105c==1 | d105d==1 | d105e==1 | d105f==1 | d105g==1 |d105j==1 ~ 1 ,
             d105a==2 | d105b==2 | d105c==2 | d105d==2 | d105e==2 | d105f==2 | d105g==2 |d105j==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_12m_f = "Any physical violence in past 12 mos. by partner, frequency")

# //TYPE OF VIOLENCE: PUSHED, SHOOK, OR SOMETHING THROWN AT HER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_push =
           case_when(
             d105a>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_push = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_push = "Ever pushed, shook, or had something thrown at her by partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_push_12m =
           case_when(
             d105a %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_push_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_push_12m = "Pushed, shook, or had something thrown at her in past 12 mos. by partner")
 
# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_push_12m_f =
           case_when(
             d105a==1  ~ 1 ,
             d105a==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_push_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_push_12m_f = "Pushed, shook, or had something thrown at her in past 12 mos. by partner, frequency")

# //TYPE OF VIOLENCE: SLAPPED
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_slap =
           case_when(
             d105b>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_slap = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_slap = "Ever slapped by partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_slap_12m =
           case_when(
             d105b %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_slap_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_slap_12m = "Slapped in past 12 mos. by partner")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_slap_12m_f =
           case_when(
             d105b==1  ~ 1 ,
             d105b==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_slap_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_slap_12m_f = "Slapped in past 12 mos. by partner, frequency")

# //TYPE OF VIOLENCE: ARM TWISTED OR HAIR PULLED
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_twist =
           case_when(
             d105j>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_twist = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_twist = "Had arm twisted or hair pulled in past partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_twist_12m =
           case_when(
             d105j %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_twist_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_twist_12m = "Had arm twisted or hair pulled in past 12 mos. by partner")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_twist_12m_f =
           case_when(
             d105j==1  ~ 1 ,
             d105j==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_twist_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_twist_12m_f = "Had arm twisted or hair pulled in past 12 mos. by partner, frequency")
 	
# //TYPE OF VIOLENCE: PUNCHED
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_punch =
           case_when(
             d105c>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_punch = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_punch = "Ever punched by partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_punch_12m =
           case_when(
             d105c %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_punch_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_punch_12m = "Punched in past 12 mos. by partner")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_punch_12m_f =
           case_when(
             d105c==1  ~ 1 ,
             d105c==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_punch_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_punch_12m_f = "Punched in past 12 mos. by partner, frequency")

# //TYPE OF VIOLENCE: KICKED, DRAGGED, BEAT UP
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_kick =
           case_when(
             d105d>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_kick = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_kick = "Ever kicked by partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_kick_12m =
           case_when(
             d105d %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_kick_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_kick_12m = "Kicked in past 12 mos. by partner")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_kick_12m_f =
           case_when(
             d105d==1  ~ 1 ,
             d105d==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_kick_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_kick_12m_f = "Kicked in past 12 mos. by partner, frequency")

# //TYPE OF VIOLENCE: CHOKED
# 	//Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_choke =
           case_when(
             d105e>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_choke = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_choke = "Ever choked or burned by partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_choke_12m =
           case_when(
             d105e %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_choke_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_choke_12m = "Choked or burned in past 12 mos. by partner")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_choke_12m_f =
           case_when(
             d105e==1  ~ 1 ,
             d105e==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_choke_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_choke_12m_f = "Choked or burned in past 12 mos. by partner, frequency")

# //TYPE OF VIOLENCE: THREATENED WITH WEAPON
# 	//Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_weapon =
           case_when(
             d105f>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_weapon = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_weapon = "Threatened or attacked with a knife, gun, or weapon by partner")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_weapon_12m =
           case_when(
             d105f %in% c(1,2) ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_weapon_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_weapon_12m = "Threatened or attacked with a knife, gun, or weapon in past 12 mos. by partner")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_prtnr_weapon_12m_f =
           case_when(
             d105f==1  ~ 1 ,
             d105f==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_weapon_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_weapon_12m_f = "Threatened or attacked with a knife, gun, or weapon in past 12 mos. by partner, frequency")
	
# ********************************************************************************	
# **Current partner sexual violence: types of violence
# ********************************************************************************
# //ANY SEXUAL VIOLENCE
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_sex =
           case_when(
             d105h>0 | d105i>0 | d105k>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_sex = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_sex = "Any sexual violence in past by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_sex_12m =
           case_when(
             d105h %in% c(1,2) | d105i %in% c(1,2) | d105k %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_sex_12m = "Any sexual violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_sex_12m_f =
           case_when(
             d105h==1 | d105i==1 | d105k==1 ~ 1 ,
             d105h==2 | d105i==2 | d105k==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_sex_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_sex_12m_f = "Any sexual violence in past 12 mos. by partner, frequency")

# 	//TYPE OF VIOLENCE: FORCED TO HAVE SEX
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_force =
           case_when(
             d105h>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_force = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_force = "Physically forced to have unwanted sex by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_force_12m =
           case_when(
             d105h %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_force_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_force_12m = "Physically forced to have unwanted sex in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_force_12m_f =
           case_when(
             d105h==1 ~ 1 ,
             d105h==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_force_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_force_12m_f = "Physically forced to have unwanted sex in past 12 mos. by partner, frequency")

# 	//TYPE OF VIOLENCE: FORCED TO PERFORM OTHER SEXUAL ACTS
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_force_act =
           case_when(
             d105k>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_force_act = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_force_act = "Physically forced to perform other sexual acts when she did not want to by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_force_act_12m =
           case_when(
             d105k %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_force_act_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_force_act_12m = "Physically forced to perform other sexual acts when she did not want to in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_force_act_12m_f =
           case_when(
             d105k==1 ~ 1 ,
             d105k==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_force_act_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_force_act_12m_f = "Physically forced to perform other sexual acts when she did not want to in past 12 mos. by partner, frequency")

# 	//TYPE OF VIOLENCE: FORCED WITH THREATS
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_threat_act =
           case_when(
             d105i>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_threat_act = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_threat_act = "Ever forced with threats or other ways to perform sexual acts she did not want to by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_threat_act_12m =
           case_when(
             d105i %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_threat_act_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_threat_act_12m = "Forced with threats or other ways to perform sexual acts she did not want to in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_threat_act_12m_f =
           case_when(
             d105i==1 ~ 1 ,
             d105i==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_threat_act_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_threat_act_12m_f = "Forced with threats or other ways to perform sexual acts she did not want to in past 12 mos. by partner, frequency")

# ********************************************************************************	
# **Current partner emotional violence: types of violence
# ********************************************************************************
# 	
# //EXPERIENCED AND EMOTIONAL VIOLENCE
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_emot =
           case_when(
             d103a>0 | d103b>0 | d103c>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_emot = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_emot = "Any emotional violence by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_emot_12m =
           case_when(
             d103a %in% c(1,2) | d103b %in% c(1,2) | d103c %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_emot_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_emot_12m = "Any emotional violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_emot_12m_f =
           case_when(
             d103a==1 | d103b==1 | d103c==1 ~ 1 ,
             d103a==2 | d103b==2 | d103c==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_emot_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_emot_12m_f = "Any emotional violence in past 12 mos. by partner, frequency")

# //HUMILIATED IN FRONT OF OTHERS
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_humil =
           case_when(
             d103a>0   ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_humil = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_humil = "Humiliated in front of others by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_humil_12m =
           case_when(
             d103a %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_humil_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_humil_12m = "Humiliated in front of others in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_humil_12m_f =
           case_when(
             d103a==1  ~ 1 ,
             d103a==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_humil_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_humil_12m_f = "Humiliated in front of others in past 12 mos. by partner, frequency")

# //THREATENED TO HURT OR HARM HER OR SOMEONE SHE CARED ABOUT
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_threat =
           case_when(
             d103b>0   ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_threat = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_threat = "Threatened to hurt or harm her or someone she cared about by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_threat_12m =
           case_when(
             d103b %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_threat_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_threat_12m = "Threatened to hurt or harm her or someone she cared about in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_threat_12m_f =
           case_when(
             d103b==1  ~ 1 ,
             d103b==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_threat_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_threat_12m_f = "Threatened to hurt or harm her or someone she cared about in past 12 mos. by partner, frequency")

# 	//INSULTED OR MADE TO FEEL BAD ABOUT HERSELF
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_insult =
           case_when(
             d103c>0   ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_insult = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_insult = "Insulted or made to feel bad about herself by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_insult_12m =
           case_when(
             d103c %in% c(1,2)  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_insult_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_insult_12m = "Insulted or made to feel bad about herself in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_insult_12m_f =
           case_when(
             d103c==1  ~ 1 ,
             d103c==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_insult_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_insult_12m_f = "Insulted or made to feel bad about herself in past 12 mos. by partner, frequency")

# ********************************************************************************	
# **Combinations of types of violence
# ********************************************************************************
# 	
# //EXPERIENCED PHYSICAL AND SEXUAL VIOLENCE BY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex =
           case_when(
             dv_prtnr_phy==1 & dv_prtnr_sex==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex = "Ever experienced physical and sexual violence by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_12m =
           case_when(
             dv_prtnr_phy_12m==1 & dv_prtnr_sex_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_12m = "Experienced physical and sexual violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_12m_f =
           case_when(
             dv_prtnr_phy_12m_f==1 & dv_prtnr_sex_12m_f==1 ~ 1 ,
             dv_prtnr_phy_12m_f==2 & dv_prtnr_sex_12m_f==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_12m_f = "Experienced physical and sexual violence in past 12 mos. by partner, frequency")

# 	//EXPERIENCED PHYSICAL AND SEXUAL AND EMOTIONAL VIOLENCE BY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_emot =
           case_when(
             dv_prtnr_phy==1 & dv_prtnr_sex==1 & dv_prtnr_emot==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_emot = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_emot = "Ever experienced physical and sexual and emotional violence by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_emot_12m =
           case_when(
             dv_prtnr_phy_12m==1 & dv_prtnr_sex_12m==1 & dv_prtnr_emot_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_emot_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_emot_12m = "Experienced physical and sexual and emotional violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_emot_12m_f =
           case_when(
             dv_prtnr_phy_12m_f==1 & dv_prtnr_sex_12m_f==1 & dv_prtnr_emot_12m_f==1 ~ 1 ,
             dv_prtnr_phy_12m_f==2 & dv_prtnr_sex_12m_f==2 & dv_prtnr_emot_12m_f==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_emot_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_emot_12m_f = "Experienced physical and sexual violence and emotional in past 12 mos. by partner, frequency")

# 	//EXPERIENCED PHYSICAL OR SEXUAL VIOLENCE BY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_any =
           case_when(
             dv_prtnr_phy==1 | dv_prtnr_sex==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_any = "Ever experienced physical OR sexual violence by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_any_12m =
           case_when(
             dv_prtnr_phy_12m==1 | dv_prtnr_sex_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_any_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_any_12m = "Experienced physical OR sexual violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_any_12m_f =
           case_when(
             dv_prtnr_phy_12m_f==1 | dv_prtnr_sex_12m_f==1 ~ 1 ,
             dv_prtnr_phy_12m_f==2 | dv_prtnr_sex_12m_f==2  ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_any_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_any_12m_f = "Experienced physical OR sexual violence in past 12 mos. by partner, frequency")

# 	//EXPERIENCED PHYSICAL OR SEXUAL OR EMOTIONAL VIOLENCE BY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_emot_any =
           case_when(
             dv_prtnr_phy==1 | dv_prtnr_sex==1 | dv_prtnr_emot==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_emot_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_emot_any = "Ever experienced physical OR sexual OR emotional violence by partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_emot_any_12m =
           case_when(
             dv_prtnr_phy_12m==1 | dv_prtnr_sex_12m==1 | dv_prtnr_emot_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_emot_any_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_emot_any_12m = "Experienced physical OR sexual OR emotional violence in past 12 mos. by partner")

# //In last 12 months, freq
IRdata <- IRdata %>%
  mutate(dv_prtnr_phy_sex_emot_any_12m_f =
           case_when(
             dv_prtnr_phy_12m_f==1 | dv_prtnr_sex_12m_f==1 | dv_prtnr_emot_12m_f==1 ~ 1 ,
             dv_prtnr_phy_12m_f==2 | dv_prtnr_sex_12m_f==2 | dv_prtnr_emot_12m_f==2 ~ 2 ,
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_sex_emot_any_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_sex_emot_any_12m_f = "Experienced physical OR sexual violence OR emotional in past 12 mos. by partner, frequency")

# ********************************************************************************	
# **Violence by ANY partner in the 12 months before the survey
# ********************************************************************************
# 	
# //EXPERIENCED PHYSICAL VIOLENCE BY ANY PARTNER IN THE 12 MONTHS BEFORE SURVEY
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy =
           case_when(
             dv_prtnr_phy==1 | d130a>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy = "Experienced physical violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_12m =
           case_when(
             dv_prtnr_phy_12m==1 | d130a==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_12m = "Experienced physical violence in past 12 mos. by any partner")

# 	//EXPERIENCED SEXUAL VIOLENCE BY ANY PARTNER IN THE 12 MONTHS BEFORE SURVEY
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_sex =
           case_when(
             dv_prtnr_sex==1 | d130b>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_sex = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_sex = "Experienced sexual violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_sex_12m =
           case_when(
             dv_prtnr_sex_12m==1 | d130b==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_sex_12m = "Experienced sexual violence in past 12 mos. by any partner")

# 	//EXPERIENCED EMOTIONAL VIOLENCE BY ANY PARTNER IN THE 12 MONTHS BEFORE SURVEY
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_emot =
           case_when(
             dv_prtnr_emot==1 | d130c>0  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_emot = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_emot = "Experienced emotional violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_emot_12m =
           case_when(
             dv_prtnr_emot_12m==1 | d130c==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_emot_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_emot_12m = "Experienced emotional violence in past 12 mos. by any partner")

# //EXPERIENCED PHYSICAL AND SEXUAL VIOLENCE BY ANY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex =
           case_when(
             dv_aprtnr_phy==1 & dv_aprtnr_sex==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex = "Experienced physical AND sexual violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_12m =
           case_when(
             dv_aprtnr_phy_12m==1 & dv_aprtnr_sex_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_12m = "Experienced physical AND sexual violence in past 12 mos. by any partner")

# 	//EXPERIENCED PHYSICAL AND SEXUAL VIOLENCE AND EMOTIONAL BY ANY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_emot =
           case_when(
             dv_aprtnr_phy==1 & dv_aprtnr_sex==1 & dv_aprtnr_emot==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex_emot = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_emot = "Experienced physical AND sexual AND emotional violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_emot_12m =
           case_when(
             dv_aprtnr_phy_12m==1 & dv_aprtnr_sex_12m==1 & dv_aprtnr_emot_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex_emot_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_emot_12m = "Experienced physical AND sexual AND emotional violence in past 12 mos. by any partner")

# 	//EXPERIENCED PHYSICAL OR SEXUAL VIOLENCE BY ANY PARTNER
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_any =
           case_when(
             dv_aprtnr_phy==1 | dv_aprtnr_sex==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_any = "Experienced physical OR sexual violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_any_12m =
           case_when(
             dv_aprtnr_phy_12m==1 | dv_aprtnr_sex_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex_any_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_any_12m = "Experienced physical OR sexual violence in past 12 mos. by any partner")

# 	//EXPERIENCED PHYSICAL OR SEXUAL VIOLENCE OR EMOTIONAL BY ANY PARTNER IN THE 12 MONTHS BEFORE SURVEY
# //Ever
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_emot_any =
           case_when(
             dv_aprtnr_phy==1 | dv_aprtnr_sex==1 | dv_aprtnr_emot==1  ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex_emot_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_emot_any = "Experienced physical OR sexual OR emotional violence by any partner")

# //In last 12 months
IRdata <- IRdata %>%
  mutate(dv_aprtnr_phy_sex_emot_any_12m =
           case_when(
             dv_aprtnr_phy_12m==1 | dv_aprtnr_sex_12m==1 | dv_aprtnr_emot_12m==1 ~ 1, 
             v044==1 & v502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_sex_emot_any_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_sex_emot_any_12m = "Experienced physical OR sexual OR emotional violence in past 12 mos. by any partner")

# ********************************************************************************
# **Experience of violence by marital duration
# ********************************************************************************
# //TIMING OF FIRST VIOLENT EVENT IN MARRIAGE (among married women who only married once)
# 	//before marriage
IRdata <- IRdata %>%
  mutate(dv_mar_viol_0 =
           case_when(
             d109==95 & v502==1 & v503==1  ~ 1, 
             v044==1 & v502==1 & v503==1 & v044==1 ~ 0 )) %>% 
  set_value_labels(dv_mar_viol_0 = c("before marriage" = 1, "other"=0)) %>%
  set_variable_labels(dv_mar_viol_0 = "Experience of violence by exact marriage duration: before marriage")

# //by 2 years of marriage
IRdata <- IRdata %>%
  mutate(dv_mar_viol_2 =
           case_when(
             (d109<2 | d109==95) & v502==1 & v503==1  ~ 1, 
             v044==1 & v502==1 & v503==1 & v044==1 ~ 0 )) %>% 
  set_value_labels(dv_mar_viol_2 = c("by 2 years of marriage" = 1, "other"=0)) %>%
  set_variable_labels(dv_mar_viol_2 = "Experience of violence by exact marriage duration: 2 yrs of marriage")

# //by 5 years of marriage
IRdata <- IRdata %>%
  mutate(dv_mar_viol_5 =
           case_when(
             (d109<5 | d109==95) & v502==1 & v503==1  ~ 1, 
             v044==1 & v502==1 & v503==1 & v044==1 ~ 0 )) %>% 
  set_value_labels(dv_mar_viol_5 = c("by 5 years of marriage" = 1, "other"=0)) %>%
  set_variable_labels(dv_mar_viol_5 = "Experience of violence by exact marriage duration: 5 yrs of marriage")

# //by 10 years of marriage
IRdata <- IRdata %>%
  mutate(dv_mar_viol_10 =
           case_when(
             (d109<10 | d109==95) & v502==1 & v503==1 ~ 1, 
             v044==1 & v502==1 & v503==1 & v044==1 ~ 0 )) %>% 
  set_value_labels(dv_mar_viol_10 = c("by 10 years of marriage" = 1, "other"=0)) %>%
  set_variable_labels(dv_mar_viol_10 = "Experience of violence by exact marriage duration: 10 yrs of marriage")

# ********************************************************************************
# **Injuries due to spousal violence
# ********************************************************************************
# 	
# 	//cuts, bruises aches
IRdata <- IRdata %>%
  mutate(dv_prtnr_cuts =
           case_when(
             d110a==1  ~ 1, 
             v044==1 & v502>0 & dv_prtnr_phy_sex_any==1 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_cuts = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_cuts = "Have cuts, bruises, or aches as a result of the violence by partner")

# //eye injuries, sprains, dislocations, burns
IRdata <- IRdata %>%
  mutate(dv_prtnr_injury =
           case_when(
             d110b==1  ~ 1, 
             v044==1 & v502>0 & dv_prtnr_phy_sex_any==1 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_injury = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_injury = "Have eye injuries, sprains, dislocations, or burns from violence by partner")

# //deep wounds, broken bones
IRdata <- IRdata %>%
  mutate(dv_prtnr_broken =
           case_when(
             d110d==1  ~ 1, 
             v044==1 & v502>0 & dv_prtnr_phy_sex_any==1 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_broken = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_broken = "Deep wounds, broken bones/teeth, other serious injury from violence by partner")
 
# //any injury as result of partner violence
IRdata <- IRdata %>%
  mutate(dv_prtnr_injury_any =
           case_when(
             d110a==1 | d110b==1 | d110d==1  ~ 1, 
             v044==1 & v502>0 & dv_prtnr_phy_sex_any==1 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_injury_any = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_injury_any = "Have any injury from violence by partner")

# ********************************************************************************
# **Initiation of spousal violence by women
# ********************************************************************************
# 
# //committed violence against partner 
IRdata <- IRdata %>%
  mutate(dv_abusedhus_phy =
           case_when(
             d112==1 | d112==2  ~ 1, 
             v044==1 & v502>0  ~ 0 )) %>% 
  set_value_labels(dv_abusedhus_phy = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_abusedhus_phy = "Ever committed violence against partner when he was not already beating her")

# //committed violence against partner in last 12 months
IRdata <- IRdata %>%
  mutate(dv_abusedhus_phy_12m =
           case_when(
             d112a==1 | d112a==2  ~ 1, 
             v044==1 & v502>0  ~ 0 )) %>% 
  set_value_labels(dv_abusedhus_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_abusedhus_phy_12m = "Committed violence against partner in past 12 mos. when he was not already beating her")
