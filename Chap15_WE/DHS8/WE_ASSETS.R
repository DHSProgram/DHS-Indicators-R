# /*****************************************************************************************************
# Program: 			  WE_ASSETS.R
# Purpose: 			  Code to compute employment, earnings, and asset ownership in men and women
# Data inputs: 		IR or MR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: Nov 18, 2021 by Shireen Assaf 
# Note:				    The indicators below can be computed for men and women. 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# we_empl				      "Employment status in the last 12 months among those currently in a union"
# we_empl_earn		    "Type of earnings among those employed in the past 12 months and currently in a union"
# we_earn_wm_decide	  "Who decides on wife's cash earnings for employment in the last 12 months"
# we_earn_wm_compare	"Comparison of cash earnings with husband's cash earnings"
# we_earn_mn_decide	  "Who decides on husband's cash earnings for employment in the last 12 months among men currently in a union"
# we_earn_hs_decide	  "Who decides on husband's cash earnings for employment in the last 12 months among women currently in a union"
# we_own_house		    "Ownership of housing"
# we_own_land			    "Ownership of land"
# we_house_deed		    "Title or deed possession for owned house"
# we_land_deed		    "Title or deed possession for owned land"
# we_bank				      "Use an account in a bank or other financial institution"
# we_mobile			      "Own a mobile phone"
# we_mobile_finance	  "Use mobile phone for financial transactions"
# ----------------------------------------------------------------------------*/

# indicators from IR file
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# *** Employment and earnings ***
# 
# //Employment in the last 12 months
IRdata <- IRdata %>%
  mutate(we_empl =
           case_when(
             v502==1 & v731 == 0   ~ 0 ,
             v502==1 & v731>0 & v731<8 ~ 1,
             v502==1 & v731 >=8 ~ 99)) %>%
  replace_with_na(replace = list(we_empl = c(99))) %>%
  set_value_labels(we_empl = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_empl = "Employment status in the last 12 months among those currently in a union")

# //Employment by type of earnings
IRdata <- IRdata %>%
  mutate(we_empl_earn =
           case_when(v502==1 & v731 %in% c(1,2,3)  ~ v741 )) %>%
  set_variable_labels(we_empl_earn = "Type of earnings among those employed in the past 12 months and currently in a union")

# //Control over earnings
IRdata <- IRdata %>%
  mutate(we_earn_wm_decide =
           case_when(v502==1 & v731 %in% c(1,2,3) & v741 %in% c(1,2) ~ v739 )) %>%
  set_variable_labels(we_earn_wm_decide = "Who decides on wife's cash earnings for employment in the last 12 months")

# //Comparison of earnings with husband/partner
IRdata <- IRdata %>%
  mutate(we_earn_wm_compare =
           case_when(v502==1 & v731 %in% c(1,2,3) & v741 %in% c(1,2) & (v746==4 | v743f==7) ~ 4, 
                     v502==1 & v731 %in% c(1,2,3) & v741 %in% c(1,2) & v746==3 ~ 3,
                     v502==1 & v731 %in% c(1,2,3) & v741 %in% c(1,2) & v746==2 ~ 2,
                     v502==1 & v731 %in% c(1,2,3) & v741 %in% c(1,2) & v746==1 ~ 1,
                     v502==1 & v731 %in% c(1,2,3) & v741 %in% c(1,2) & v746>=8 ~ 9)) %>%
  set_value_labels(we_earn_wm_compare = c("More than him" = 1, "Less than him"=2 , "About the same"=3, "Husband has no earnings"=4, "Don't know/missing"=9 )) %>%
  set_variable_labels(we_earn_wm_compare = "Comparison of cash earnings with husband's cash earnings")


# //Who decides on how husband's cash earnings are used
IRdata <- IRdata %>%
  mutate(we_earn_hs_decide =
           case_when(v502==1  ~ v743f,
                     v502==1 & v743f==8 ~ 9,
                     v502==1 & v746==4  ~ 7)) %>%
  replace_with_na(replace = list(we_earn_hs_decide = c(7))) %>%
  set_variable_labels(we_earn_hs_decide = "Who decides on husband's cash earnings for employment in the last 12 months among women currently in a union")

# *** Ownership of assets ***
# 
# //Own a house
IRdata <- IRdata %>%
  mutate(we_own_house = v745a) %>%
  set_variable_labels(we_own_house = "Ownership of housing")

# //Own land
IRdata <- IRdata %>%
  mutate(we_own_land = v745b) %>%
  set_variable_labels(we_own_land = "Ownership of land")

# //Ownership of house deed
IRdata <- IRdata %>%
  mutate(we_house_deed =
           case_when(
             v745a %in% c(1,2,3) & v745c==1 ~ 1 ,
             v745a %in% c(1,2,3) & v745c==2 ~ 2 ,
             v745a %in% c(1,2,3) & v745c==0 ~ 0 ,
             v745a %in% c(1,2,3) & v745c>=3 ~ 9 )) %>%
  set_value_labels(we_house_deed = c("Respondent's name on title/deed" = 1, "Respondent's name is not on title/deed"=2 , "Does not have title/deed"=0, "Don't know/missing"=9 )) %>%
  set_variable_labels(we_house_deed = "Title or deed possession for owned house")

# //Ownership of land deed
IRdata <- IRdata %>%
  mutate(we_land_deed =
           case_when(
             v745b %in% c(1,2,3) & v745d==1 ~ 1 ,
             v745b %in% c(1,2,3) & v745d==2 ~ 2 ,
             v745b %in% c(1,2,3) & v745d==0 ~ 0 ,
             v745b %in% c(1,2,3) & v745d>=3 ~ 9 )) %>%
  set_value_labels(we_land_deed = c("Respondent's name on title/deed" = 1, "Respondent's name is not on title/deed"=2 , "Does not have title/deed"=0, "Don't know/missing"=9 )) %>%
  set_variable_labels(we_land_deed = "Title or deed possession for owned land")

# //Own a bank account
IRdata <- IRdata %>%
  mutate(we_bank =
           case_when(
             v170==1 ~ 1 ,
             v170 %in% c(0,8,9) ~ 0 )) %>%
  set_value_labels(we_bank = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_bank = "Use an account in a bank or other financial institution")

# //Own a mobile phone
IRdata <- IRdata %>%
  mutate(we_mobile =
           case_when(
             v169a==1 ~ 1 ,
             v169a %in% c(0,8,9) ~ 0 )) %>%
  set_value_labels(we_mobile = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_mobile = "Own a mobile phone")

# //Use mobile for finances
IRdata <- IRdata %>%
  mutate(we_mobile_finance =
           case_when(
             v169a==1 & v169b==1 ~ 1 ,
             v169a==1 & v169b %in% c(0,8,9)  ~ 0 )) %>%
  set_value_labels(we_mobile_finance = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_mobile_finance = "Use mobile phone for financial transactions")

############################################################################################

# * indicators from MR file

MRdata <- MRdata %>%
  mutate(wt = mv005/1000000)

# *** Employment and earnings ***
# 
# //Employment in the last 12 months
MRdata <- MRdata %>%
  mutate(we_empl =
           case_when(
             mv502==1 & mv731 == 0   ~ 0 ,
             mv502==1 & mv731>0 & mv731<8 ~ 1,
             mv502==1 & mv731 >=8 ~ 99)) %>%
  replace_with_na(replace = list(we_empl = c(99))) %>%
  set_value_labels(we_empl = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_empl = "Employment status in the last 12 months among those currently in a union")

# //Employment by type of earnings
MRdata <- MRdata %>%
  mutate(we_empl_earn =
           case_when(mv502==1 & mv731 %in% c(1,2,3)  ~ mv741 )) %>%
  set_variable_labels(we_empl_earn = "Type of earnings among those employed in the past 12 months and currently in a union")

# //Who decides on how husband's cash earnings are used
MRdata <- MRdata %>%
  mutate(we_earn_mn_decide =
           case_when(mv502==1 & mv731 %in% c(1,2,3) & mv741 %in% c(1,2) ~ mv739,
                     mv502==1 & mv739==8 ~ 9)) %>%
  replace_with_na(replace = list(we_earn_mn_decide = c(7))) %>%
  set_variable_labels(we_earn_mn_decide = "Who decides on husband's cash earnings for employment in the last 12 months among men currently in a union")

# *** Ownership of assets ***

# //Own a house
MRdata <- MRdata %>%
  mutate(we_own_house = mv745a) %>%
  set_variable_labels(we_own_house = "Ownership of housing")
# 
# //Own land
MRdata <- MRdata %>%
mutate(we_own_land = mv745b) %>%
  set_variable_labels(we_own_land = "Ownership of land")

# //Ownership of house deed
MRdata <- MRdata %>%
  mutate(we_house_deed =
           case_when(
             mv745a %in% c(1,2,3) & mv745c==1 ~ 1 ,
             mv745a %in% c(1,2,3) & mv745c==2 ~ 2 ,
             mv745a %in% c(1,2,3) & mv745c==0 ~ 0 ,
             mv745a %in% c(1,2,3) & mv745c>=3 ~ 9 )) %>%
  set_value_labels(we_house_deed = c("Respondent's name on title/deed" = 1, "Respondent's name is not on title/deed"=2 , "Does not have title/deed"=0, "Don't know/missing"=9 )) %>%
  set_variable_labels(we_house_deed = "Title or deed possession for owned house")

# //Ownership of land deed
MRdata <- MRdata %>%
  mutate(we_land_deed =
           case_when(
             mv745b %in% c(1,2,3) & mv745d==1 ~ 1 ,
             mv745b %in% c(1,2,3) & mv745d==2 ~ 2 ,
             mv745b %in% c(1,2,3) & mv745d==0 ~ 0 ,
             mv745b %in% c(1,2,3) & mv745d>=3 ~ 9 )) %>%
  set_value_labels(we_land_deed = c("Respondent's name on title/deed" = 1, "Respondent's name is not on title/deed"=2 , "Does not have title/deed"=0, "Don't know/missing"=9 )) %>%
  set_variable_labels(we_land_deed = "Title or deed possession for owned land")

# //Own a bank account
MRdata <- MRdata %>%
  mutate(we_bank =
           case_when(
             mv170==1 ~ 1 ,
             mv170 %in% c(0,8,9) ~ 0 )) %>%
  set_value_labels(we_bank = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_bank = "Use an account in a bank or other financial institution")

# //Own a mobile phone
MRdata <- MRdata %>%
  mutate(we_mobile =
           case_when(
             mv169a==1 ~ 1 ,
             mv169a %in% c(0,8,9) ~ 0 )) %>%
  set_value_labels(we_mobile = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_mobile = "Own a mobile phone")

# //Use mobile for finances
MRdata <- MRdata %>%
  mutate(we_mobile_finance =
           case_when(
             mv169a==1 & mv169b==1 ~ 1 ,
             mv169a==1 & mv169b %in% c(0,8,9)  ~ 0 )) %>%
  set_value_labels(we_mobile_finance = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_mobile_finance = "Use mobile phone for financial transactions")
