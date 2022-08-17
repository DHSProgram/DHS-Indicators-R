# /*****************************************************************************************************
# Program: 			  CH_STOOL.R
# Purpose: 			  Code disposal of child's stool variables
# Data inputs: 		KR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: Aug 16 2022 by Shireen Assaf 
# Notes:				This do file will create a subset of the KR data file KRstool 
# 					    This is because the denominator is the youngest child under age 2 living with the mother. 			
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_stool_dispose	"How child's stool was disposed"
# ch_stool_safe		"Child's stool was disposed of appropriately"
# ----------------------------------------------------------------------------*/

#create subset of KRfile to select for children for Stool indicators
KRstool <- KRdata %>%
  subset(age < 24 & b9==0) %>% # children under 24 months living at home
  arrange(caseid, bidx) %>% # make sure the data is sorted
  subset(is.na(lag(caseid)) | caseid!=lag(caseid)) # select just the youngest

# weight variable 
KRstool <- KRstool %>%
  mutate(wt = v005/1000000)


# //Stool disposal method
KRstool <- KRstool %>%
  mutate(ch_stool_dispose = 
           case_when(v465==1 ~ 1, v465==2  ~ 2 , v465==5  ~ 3, v465==3 ~ 4, v465==4 ~ 5,
                     v465==9 ~ 6, v465==96 ~ 96, v465>=98 ~ 99)) %>%
  set_value_labels(ch_stool_dispose = c("Child used toilet or latrine" = 1, "Put/rinsed into toilet or latrine"=2,
                                        "Buried"=3, "Put/rinsed into drain or ditch"=4, "Thrown into garbage"=5,
                                        "Left in the open"=6, "Other"=96, "DK/Missing"=99)) %>%
  set_variable_labels(ch_stool_dispose = "How child's stool was disposed among youngest children under age 2 living with mother")

# //Safe stool disposal
KRstool <- KRstool %>%
  mutate(ch_stool_safe = 
           case_when(ch_stool_dispose<=3 ~ 1, TRUE ~ 0 )) %>%
  set_value_labels(ch_stool_safe = c("Safe disposal" = 1, "Not safe"=0)) %>%
  set_variable_labels(ch_stool_safe = "Child's stool was disposed of appropriately among youngest children under age 2 living with mother")

