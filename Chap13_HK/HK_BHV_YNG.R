# /*******************************************************************************************************************************
# Program: 				HK_BHV_YNG.R
# Data inputs: 		IR or MR datasets
# Data outputs:		coded variables for sexual behaviors among young people
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# Note:				The indicators are computed for all men. No age selection is made here. 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_sex_15			"Had sexual intercourse before age 15 among those age 15-24"
# hk_sex_18			"Had sexual intercourse before age 18 among those age 18-24"
# hk_nosex_youth		"Never had sexual intercourse among never-married age 15-24"
# hk_sex_youth_test	"Had sexual intercourse in the past 12 months and received HIV test and results among those age 15-24"


# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)


# SEXUAL BEHAVIORS AMONG YOUNG PEOPLE (GIRLS AND WOMEN) ------------------------
# //Sex before 15
IRdata <- IRdata %>% mutate(hk_sex_15 = case_when(
  v531 %in% 1:14  & v012 <24 ~ 1,
  v012 < 24 ~ 0)) %>%
  set_value_labels(hk_sex_15 = yesno) %>%
  set_variable_labels(hk_sex_15 = "Had sexual intercourse before 15 among age 15-24")

# //Sex before 18
IRdata <- IRdata %>% mutate(hk_sex_18 = case_when(
  v531 %in% 1:17 & v012>=18 & v012<=24 ~ 1,
  v012>=18 & v012<=24 ~ 0)) %>%
  set_value_labels(hk_sex_18 = yesno) %>%
  set_variable_labels(hk_sex_18 = "Had sexual intercourse before 18 among age 18-24")

# //Never had sexual
IRdata <- IRdata %>% mutate(hk_nosex_youth = case_when(
  (v525==0 | v525==99) & v501==0 & v012 %in% 15:24 ~ 1,
  v501==0 & v012 %in% 15:24 ~ 0)) %>%
  set_value_labels(hk_nosex_youth = yesno) %>%
  set_variable_labels(hk_nosex_youth = "Never had sexual intercourse among never-married age 15-24")

# //Tested and received HIV test results
IRdata <- IRdata %>% mutate(hk_sex_youth_test = case_when(
  (v527 %in% 100:251 | v527 %in% 300:311) & v828==1 & v826a %in% 0:11 & v012 %in% 15:24 ~ 1,
  (v527 %in% 100:251 | v527 %in% 300:311) & v012 %in% 15:24 ~ 0)) %>%
  set_value_labels(hk_sex_youth_test = yesno) %>%
  set_variable_labels(hk_sex_youth_test = "Had sexual intercourse in past 12 mnths and received HIV test and results among 15-24")





# SEXUAL BEHAVIORS AMONG YOUNG PEOPLE (BOYS AND MEN) ------------------------

# //Sex before 15
MRdata <- MRdata %>% mutate(hk_sex_15 = case_when(
  mv531 %in% 1:14  & mv012 <=24 ~ 1,
  mv012 <= 24 ~ 0)) %>%
  set_value_labels(hk_sex_15 = yesno) %>%
  set_variable_labels(hk_sex_15 = "Had sexual intercourse before 15 among age 15-24")

# //Sex before 18
MRdata <- MRdata %>% mutate(hk_sex_18 = case_when(
  mv531 %in% 1:17 & mv012>=18 & mv012<=24 ~ 1,
  mv012>=18 & mv012<=24 ~ 0)) %>%
  set_value_labels(hk_sex_18 = yesno) %>%
  set_variable_labels(hk_sex_18 = "Had sexual intercourse before 18 among age 18-24")

# //Never had sexual
MRdata <- MRdata %>% mutate(hk_nosex_youth = case_when(
  (mv525==0 | mv525==99) & mv501==0 & mv012 %in% 15:24 ~ 1,
  mv501==0 & mv012 %in% 15:24 ~ 0)) %>%
  set_value_labels(hk_nosex_youth = yesno) %>%
  set_variable_labels(hk_nosex_youth = "Never had sexual intercourse among never-married age 15-24")

# //Tested and received HIV test results
MRdata <- MRdata %>% mutate(hk_sex_youth_test = case_when(
  (mv527 %in% 100:251 | mv527 %in% 300:311) & mv828==1 & mv826a %in% 0:11 & mv012 %in% 15:24 ~ 1,
  (mv527 %in% 100:251 | mv527 %in% 300:311) & mv012 %in% 15:24 ~ 0)) %>%
  set_value_labels(hk_sex_youth_test = yesno) %>%
  set_variable_labels(hk_sex_youth_test = "Had sexual intercourse in past 12 mnths and received HIV test and results among 15-24")



