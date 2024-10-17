# ******************************************************************************
# Program: 			  HK_TEST_CONSL.R - DHS8 update
# Purpose: 			  Code to compute indicators on HIV rior testing and counseling
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 3 2024 by Courtney Allen 
# ******************************************************************************

# Variables created in this file ------------------------------------------------
# hk_test_prior			"Prior HIV testing status and whether received test result"
# hk_test_ever			"Ever been tested for HIV"
# hk_test_12m				"Tested for HIV in the past 12 months and received results of the last test"
# hk_test_life	    "Number of times tested for HIV in lifetime" - NEW indicator in DHS8
#
# hk_hiv_selftest_heard	"Ever heard of HIV self-test kits"
# hk_hiv_selftest_use		"Ever used a HIV self-test kit"
#
# 
# FOR WOMEN ONLY
# hk_test_anc_result	    "Received HIV test during ANC visit and received test results among women with a birth 2 years before the survey" - NEW indicator in DHS8
# hk_test_anc_noresult	  "Received HIV test during ANC visit and did not receive test results among women with a birth 2 years before the survey"
# hk_test_anclbr_result	  "Received HIV test during ANC visit or labor and received results among women with a birth 2 years before the survey"
# hk_test_anclbr_noresult	"Received HIV test during ANC visit or labor but did not receive results among women with a birth 2 years before the survey"

# NOTES ------------------------------------------------------------------------
# The indicators below can be computed for men and women. No age selection is made here. 
# Several indicators have also been discontiued in DHS8. Please check the excel indicator list for these indicators.

# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno<- c("Yes" = 1, "No" = 0)


# COVERAGE OF PRIOR HIV TESTING (WOMEN)-----------------------------------------

# Had prior HIV test and whether they received results
IRdata <- IRdata %>% mutate(hk_test_prior = case_when(
  v781==1 & v828==1 ~ 1,
  v781==1 & v828==0 ~ 2,
  v781==0 ~ 3)) %>%
  set_value_labels(hk_test_prior = c("Tested and received results"= 1,
                                   "Tested and did not receive results" = 2,
                                   "Never tested" = 3)) %>%
  set_variable_labels(hk_test_prior = "Prior HIV testing status and whether received test result")

# Ever tested
IRdata <- IRdata %>% mutate(hk_test_ever = case_when(
  v781==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_test_ever = yesno) %>%
  set_variable_labels(hk_test_ever = "Ever been tested for HIV")

# Tested in last 12 months and received test results
IRdata <- IRdata %>% mutate(hk_test_12m = case_when(
  v828==1 & v826a %in% 0:11 ~ 1,
  TRUE ~ 0)) %>%
    set_value_labels(hk_test_12m = yesno) %>%
    set_variable_labels(hk_test_12m = "Tested for HIV in the past 12 months and received results of the last test")

# Number of lifetime HIV tests - NEW Indicator in DHS8
IRdata <- IRdata %>% mutate(hk_test_life = case_when(
  v864>=6 ~ 6,
  v781==0 | is.na(v781) ~ 0,
  TRUE ~ v864)) %>%
  set_value_labels(hk_test_life= c("6+"=6)) %>%
  set_variable_labels(hk_test_life = "Number of times tested for HIV in lifetime")

# Heard of self-test kits
IRdata <- IRdata %>% mutate(hk_hiv_selftest_heard = case_when(
  v856 %in% 1:3 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_hiv_selftest_heard = yesno) %>%
  set_variable_labels(hk_hiv_selftest_heard = "Ever heard of HIV self-test kits")

# Ever used a self-test kit
IRdata <- IRdata %>% mutate(hk_hiv_selftest_use = case_when(
  v856==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_hiv_selftest_use = yesno) %>%
  set_variable_labels(hk_hiv_selftest_use = "Ever used a HIV self-test kit")



# PREGNANT WOMEN COUNSELED, TESTED FOR HIV -------------------------------------

  
# NOTE - HOW TO CHECK FOR B19 -------------------------  -----------------------
# Indicators are among women with a live birth in the two years preceiding the survey. 

# To make this restriction we need to compute the age of most recent child (agec).
IRdata <- IRdata %>% mutate(agec = v008 - b3_01)

# check if survey has b19, which should be used instead to compute age. 
b19_check <-"b19_01" %in% colnames(IRdata)

# if b19 is present
ifelse(b19_check=="TRUE",
       b19_included <- 1,
       b19_included <- 0)

# check if b19 actually has values or is empty
ifelse(is.na(sd(IRdata$b19_01, na.rm=TRUE)) | sd(IRdata$b19_01) == 0, 
       b19_included <- 0,)
IRdata <- IRdata %>% mutate(b19_included = b19_included)

# if b19 is present or has values, replace agec var created above
IRdata$agec <- ifelse(IRdata$b19_included==1, IRdata$b19_01, IRdata$agec)


# Tested for HIV during ANC visit and received results - NEW Indicator in DHS8
IRdata <- IRdata %>% mutate(hk_test_anc_result = case_when(
  v840==1 & v841==1 & agec<24 ~ 1,
  agec<24 ~ 0)) %>%
  set_value_labels(hk_test_anc_result = yesno) %>%
  set_variable_labels(hk_test_anc_result = "Received HIV test during ANC visit and received test results among women with a birth 2 years before the survey")

# Tested for HIV during ANC visit and did not receive test results
IRdata <- IRdata %>% mutate(hk_test_anc_noresult = case_when(
  v840==1 & v841!=1 & agec<24 ~ 1,
  agec<24 ~ 0)) %>%
  set_value_labels(hk_test_anc_noresult = yesno) %>%
  set_variable_labels(hk_test_anc_noresult = "Received HIV test during ANC visit and did not receive test results among women with birth <2yrs before survey")

# Received HIV test during ANC or labor and received results
IRdata <- IRdata %>% mutate(hk_test_anclbr_result = case_when(
  (v840==1 | v840a==1) & (v841==1 | v841a==1) & agec<24 ~ 1,
  agec<24 ~ 0)) %>%
  set_value_labels(hk_test_anclbr_result = yesno) %>%
  set_variable_labels(hk_test_anclbr_result	= "Received HIV test during ANC visit or labor and received results among women with birth <2yrs before survey")

# Received HIV test during ANC or labor but did not receive results
IRdata <- IRdata %>% mutate(hk_test_anclbr_noresult = case_when(
  (v840==1 | v840a==1) & (v841!=1 | is.na(v841)) & (v841a!=1 | is.na(v841a)) & agec<24 ~ 1,
  agec<24 ~ 0)) %>%
  set_value_labels(hk_test_anclbr_noresult = yesno) %>%
  set_variable_labels(hk_test_anclbr_noresult = "Received HIV test during ANC visit or labor but no results among women with birth <2yrs before survey")
  


# COVERAGE OF PRIOR HIV TESTING (MEN) -------------------------------------------
    
# Had prior HIV test and whether they received results
MRdata <- MRdata %>% mutate(hk_test_prior = case_when(
  mv781==1 & mv828==1 ~ 1,
  mv781==1 & mv828==0 ~ 2,
  mv781==0 ~ 3)) %>%
  set_value_labels(hk_test_prior = c("Tested and received results" = 1, 
                                   "Tested and did not receive results" = 2,
                                   "Never tested" = 3)) %>%
  set_variable_labels(hk_test_prior = "Prior HIV testing status and whether received test result")

# Ever tested
MRdata <- MRdata %>% mutate(hk_test_ever = case_when(
  mv781==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_test_ever = yesno) %>%
  set_variable_labels(hk_test_ever  = "Ever been tested for HIV")

# Tested in last 12 months and received test results
MRdata <- MRdata %>% mutate(hk_test_12m = case_when(
  mv828==1 & mv826a %in% 0:11 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_test_12m = yesno) %>%
  set_variable_labels(hk_test_12m = "Tested for HIV in the past 12 months and received results of the last test")

# Number of lifetime HIV tests - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_test_life = case_when(
  mv864>=6 ~ 6,
  mv781==0 | is.na(mv781) ~ 0,
  TRUE ~ mv864)) %>%
  set_value_labels(hk_test_life= c("6+"=6)) %>%
  set_variable_labels(hk_test_life = "Number of times tested for HIV in lifetime")

# Heard of self-test kits
MRdata <- MRdata %>% mutate(hk_hiv_selftest_heard = case_when(
  mv856 %in% 1:3 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_hiv_selftest_heard = yesno) %>%
  set_variable_labels(hk_hiv_selftest_heard = "Ever heard of HIV self-test kits")

# Ever used a self-test kit
MRdata <- MRdata %>% mutate(hk_hiv_selftest_use = case_when(
  mv856==1 ~ 1,
  TRUE ~ 0 )) %>%
  set_value_labels(hk_hiv_selftest_use = yesno) %>%
  set_variable_labels(hk_hiv_selftest_use = "Ever used a HIV self-test kit")


