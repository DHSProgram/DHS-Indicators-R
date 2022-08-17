# /*****************************************************************************************************
# Program: 			  CH_KNOW_ORS.R
# Purpose: 			  Code knowledge of ORS variable.
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: August 17 2022 by Shireen Assaf 
				
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_know_ors		"Know about ORS as treatment for diarrhea among women with birth in the last 5 years"
# ----------------------------------------------------------------------------*/

# weight variable 
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# //Know ORS
IRdata <- IRdata %>%
  mutate(ch_know_ors = 
           case_when(v416>0 & v416<3 & v208>0 ~ 1, v416==0 & v208>0 ~ 0  )) %>%
  set_value_labels(ch_know_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_know_ors = "Know about ORS as treatment for diarrhea among women with birth in the last 5 years")
