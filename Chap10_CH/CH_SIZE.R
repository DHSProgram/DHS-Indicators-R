# /*****************************************************************************************************
# Program: 			  CH_SIZE.R
# Purpose: 		  	Code child size variables.
# Data inputs: 		KR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: August 1 2022 by Shireen Assaf 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_size_birth	"Size of child at birth as reported by mother"
# ch_report_bw	"Has a reported birth weight"
# ch_below_2p5	"Birth weight less than 2.5 kg"
# ----------------------------------------------------------------------------*/

# weight variable 
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# //Child's size at birth
KRdata <- KRdata %>%
  mutate(ch_size_birth =
           case_when(
             m18==5  ~ 1 ,
             m18==4  ~ 2 ,
             m18<=3  ~ 3 ,
             m18==8 | m18==9 ~ 9)) %>%
  set_value_labels(ch_size_birth = c("Very small" = 1, "Smaller than average"=2, "Average or larger"=3, "Don't know/missing"=9)) %>%
  set_variable_labels(ch_size_birth = "Size of child at birth as reported by mother")

# //Child's reported birth weight
KRdata <- KRdata %>%
  mutate(ch_report_bw = 
           case_when(
             m19<=9000 ~ 1 ,
             TRUE ~ 0 )) %>%
  set_value_labels(ch_report_bw = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_report_bw = "Has a reported birth weight")
 
# //Child before 2.5kg
KRdata <- KRdata %>%
  mutate(ch_below_2p5 = 
           case_when(
             m19<2500 & ch_report_bw==1 ~ 1 ,
             ch_report_bw==1 ~ 0 )) %>%
  set_value_labels(ch_below_2p5 = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_below_2p5 = "Birth weight less than 2.5 kg")