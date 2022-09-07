# ******************************************************************************
# Program: 			  HK_STIL.R
# Purpose: 			  Code to compute indicators on Sexually Transmitted Infections (STI)
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_sti				"Had STI in the past 12 months"
# hk_gent_disch		"Had abnormal (or bad-smelling) genital discharge in the past 12 months"
# hk_gent_sore		"Had genital sore or ulcer in the past 12 months"
# hk_sti_symp			"Had STI or STI symptoms in the past 12 months"
# hk_sti_trt_doc		"Had STI or STI symptoms in the past 12 months and sought advice or treatment from a clinic/hospital/private doctor"
# hk_sti_trt_pharm	"Had STI or STI symptoms in the past 12 months and sought advice or treatment from a pharmacy"
# hk_sti_trt_other	"Had STI or STI symptoms in the past 12 months and sought advice or treatment from any other source"
# hk_sti_notrt		"Had STI or STI symptoms in the past 12 months and sought no advice or treatment"




# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)


# SELF REPORT STIS (WOMEN) -----------------------------------------------------
# STI in the past 12 months
IRdata <- IRdata %>%  mutate(hk_sti = case_when(
  v763a==1 ~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti = yesno) %>%
  set_variable_labels(hk_sti = "Had STI in the past 12 months")

# Discharge in the past 12 months
IRdata <- IRdata %>%  mutate(hk_gent_disch = case_when(
  v763c==1 ~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_disch = yesno) %>%
  set_variable_labels(hk_gent_disch= "Had abnormalgenital discharge in past 12 mnths")

# Genital sore in past 12 months
IRdata <- IRdata %>%  mutate(hk_gent_sore = case_when(
  v763b==1 ~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_sore = yesno) %>%
  set_variable_labels(hk_gent_sore = "Had genital sore or ulcer in past 12 mnths")

# STI or STI symptoms in the past 12 months
IRdata <- IRdata %>%  mutate(hk_sti_symp = case_when(
  v763a==1 | v763b==1 | v763c==1~ 1,
  v525==0 | v525==99 | is.na(v525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti_symp = yesno) %>%
  set_variable_labels(hk_sti_symp = "Had STI or STI symptoms in past 12 mnths")

# Sought care from clinic/hospital/private doctor for STI
IRdata <- IRdata %>%  mutate(hk_sti_trt_doc = case_when(
  v763a==1 | v763b==1 | v763c==1 ~ 0))

  # list of letters a:l,n:s
  sti_list <- c(letters[1:12], letters[14:19])
  sti_vars <- NULL
  for(a in sti_list) {
    sti_vars <- c(sti_vars, paste0("v770", a))
  }
  
  # create var to sum where respondents report seeking care from any source (v770a-v77l or v77m-v77s)
  IRdata <- IRdata %>% mutate(hk_sti_trt_where= select(., starts_with(sti_vars)) %>% rowSums(, na.rm=TRUE))
      
  IRdata <- IRdata %>%  
    mutate(hk_sti_trt_doc = case_when(
      v770m==1 ~ 0,
      hk_sti_trt_where>0 ~ 1,
      v763a==1 | v763b==1 | v763c==1 ~ 0)) %>%
    set_value_labels(hk_sti_trt_doc = yesno) %>%
    set_variable_labels(hk_sti_trt_doc = "Had STI or STI symptoms in past 12 mnths and sought advice or treatment from a clinic/hospital/private doctor")
    
  
# Sought care from pharmacy for STI
IRdata <- IRdata %>%  mutate( hk_sti_trt_pharm = case_when(
  v770m==1 | v770t==1 ~ 1,
  v763a==1 | v763b==1 | v763c==1 ~ 0)) %>%
  set_value_labels(hk_sti_trt_pharm = yesno) %>%
  set_variable_labels(hk_sti_trt_pharm= "Had STI or STI symptoms in past 12 mnths and sought advice or treatment from pharmacy")

# Sought care from any other source for STI
IRdata <- IRdata %>%  mutate(hk_sti_trt_other = case_when(
  v770u==1 | v770v==1 | v770w==1 | v770x==1 ~ 1,
  v763a==1 | v763b==1 | v763c==1 ~ 0)) %>%
  set_value_labels(hk_sti_trt_other = yesno) %>%
  set_variable_labels(hk_sti_trt_other= "Had STI or STI symptoms in past 12 mnths and sought advice or treatment from any other source")

# Did not seek care for STI
IRdata <- IRdata %>%  mutate(hk_sti_notrt = case_when(
  v770==0 ~ 1,
  v763a==1 | v763b==1 | v763c==1 ~ 0)) %>%
  set_value_labels( hk_sti_notrt = yesno) %>%
  set_variable_labels(hk_sti_notrt= "Had STI or STI symptoms in  past 12 mnths and sought no advice or treatment")





# SELF REPORT STIS (MEN) -------------------------------------------------------



MRdata <- MRdata %>%  mutate(hk_sti = case_when(
  mv763a==1 & mv525==1~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti = yesno) %>%
  set_variable_labels(hk_sti = "Had STI in the past 12 months")

# Discharge in the past 12 months
MRdata <- MRdata %>%  mutate(hk_gent_disch = case_when(
  mv763c==1 ~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_disch = yesno) %>%
  set_variable_labels(hk_gent_disch= "Had abnormalgenital discharge in past 12 mnths")

# Genital sore in past 12 months
MRdata <- MRdata %>%  mutate(hk_gent_sore = case_when(
  mv763b==1 ~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_sore = yesno) %>%
  set_variable_labels(hk_gent_sore = "Had genital sore or ulcer in past 12 mnths")

# STI or STI symptoms in the past 12 months
MRdata <- MRdata %>%  mutate(hk_sti_symp = case_when(
  mv763a==1 | mv763b==1 | mv763c==1~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti_symp = yesno) %>%
  set_variable_labels(hk_sti_symp = "Had STI or STI symptoms in past 12 mnths")

# Sought care from clinic/hospital/private doctor for STI
MRdata <- MRdata %>%  mutate(hk_sti_trt_doc = case_when(
  mv763a==1 | mv763b==1 | mv763c==1 ~ 0))

# list of letters a:l,n:s
sti_list <- c(letters[1:12], letters[14:19])
sti_vars <- NULL
for(a in sti_list) {
  sti_vars <- c(sti_vars, paste0("mv770", a))
}

# create var to sum where respondents report seeking care from any source (mv770a-mv77l or mv77m-mv77s)
MRdata <- MRdata %>% mutate(hk_sti_trt_where= select(., starts_with(sti_vars)) %>% rowSums(, na.rm=TRUE))

MRdata <- MRdata %>%  
  mutate(hk_sti_trt_doc = case_when(
    mv770m==1 ~ 0,
    hk_sti_trt_where>0 ~ 1,
    mv763a==1 | mv763b==1 | mv763c==1 ~ 0)) %>%
  set_value_labels(hk_sti_trt_doc = yesno) %>%
  set_variable_labels(hk_sti_trt_doc = "Had STI or STI symptoms in past 12 mnths and sought advice or treatment from a clinic/hospital/private doctor")


# Sought care from pharmacy for STI
MRdata <- MRdata %>%  mutate( hk_sti_trt_pharm = case_when(
  mv770m==1 | mv770t==1 ~ 1,
  mv763a==1 | mv763b==1 | mv763c==1 ~ 0)) %>%
  set_value_labels(hk_sti_trt_pharm = yesno) %>%
  set_variable_labels(hk_sti_trt_pharm= "Had STI or STI symptoms in past 12 mnths and sought advice or treatment from pharmacy")

# Sought care from any other source for STI
MRdata <- MRdata %>%  mutate(hk_sti_trt_other = case_when(
  mv770u==1 | mv770v==1 | mv770w==1 | mv770x==1 ~ 1,
  mv763a==1 | mv763b==1 | mv763c==1 ~ 0)) %>%
  set_value_labels(hk_sti_trt_other = yesno) %>%
  set_variable_labels(hk_sti_trt_other= "Had STI or STI symptoms in past 12 mnths and sought advice or treatment from any other source")

# Did not seek care for STI
MRdata <- MRdata %>%  mutate(hk_sti_notrt = case_when(
  mv770==0 ~ 1,
  mv763a==1 | mv763b==1 | mv763c==1 ~ 0)) %>%
  set_value_labels( hk_sti_notrt = yesno) %>%
  set_variable_labels(hk_sti_notrt= "Had STI or STI symptoms in  past 12 mnths and sought no advice or treatment")




