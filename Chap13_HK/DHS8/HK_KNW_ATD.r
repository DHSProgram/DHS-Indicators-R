# ******************************************************************************
# Program: 			  HK_KNW_ATD.R - DHS8 update
# Purpose: 			  Code to compute HIV-AIDS related knowledge and attitude indicators 
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: August 30, 2024 by Courtney Allen 
# ******************************************************************************

# Variables created in this file ------------------------------------------------

# hk_ever_heard			  "Have ever heard of HIV or AIDS"
# hk_knw_arv	        "Heard of ARVs that treat HIV" --- NEW indicator in DHS8
# hk_knw_mtct_meds	  "Know that risk of HIV mother to child transmission can be reduced by the mother taking special medicines"
# hk_knw_PrEP	        "Heard of PrEP" --- NEW indicator in DHS8
# hk_aprov_PrEP	      "Heard of PrEP and approve of people who take PrEP to reduce their risk of HIV" --- NEW indicator in DHS8

# hk_atd_child_nosch	"Think that children living with HIV should not go to school with HIV negative children"
# hk_atd_shop_notbuy	"Would not buy fresh vegetables from a shopkeeper who has HIV"
# hk_atd_discriminat	"Have discriminatory attitudes towards people living with HIV"

# hk_disclos_hiv	    "Self-reported as HIV positive and disclosed their HIV status to anyone" --- NEW indicator in DHS8
# hk_asham_hiv	      "Self-reported as HIV positive and felt ashamed of their HIV status" --- NEW indicator in DHS8
# hk_tlkbad_hiv	      "Self-reported as HIV positive and report that people talk badly about them because of HIV status in the past 12mos." --- NEW indicator in DHS8
# hk_othr_disclos_hiv	"Self-reported as HIV positive and report that someone else disclosed their status without their permission in the past 12mos." --- NEW indicator in DHS8
# hk_harass_hiv	      "Self-reported as HIV positive and report being verbally insulted/harassed/threatened due to HIV status in the past 12mos." --- NEW indicator in DHS8
# hk_stigma_hiv	      "Self-reported as HIV positive and report experiencing stigma in a community setting in the past 12mos." --- NEW indicator in DHS8
# hk_hlthwrk_tlkbad_hiv	"Self-reported as HIV positive and report that healthcare workers talked badly due to HIV status in the past 12mos." --- NEW indicator in DHS8
# hk_hlthwrk_vrbabuse_hiv	"Self-reported as HIV positive and report that healthcare workers verbally abused them due to HIV status in the past 12mos." --- NEW indicator in DHS8

# ***the indicators below were updated to the denominator of youth age 15-24 
# hk_knw_risk_cond		"Know you can reduce HIV risk by using condoms at every sex"
# hk_knw_risk_sex			"Know you can reduce HIV risk by limiting to one uninfected sexual partner who has no other partners"
# hk_knw_hiv_hlth			"Know a healthy looking person can have HIV"
# hk_knw_hiv_mosq			"Know HIV cannot be transmitted by mosquito bites"
# hk_knw_hiv_food			"Know cannot become infected by sharing food with a person who has HIV"
# hk_knw_all	        “Have all reported knowledge about HIV prevention among youth age 15-24" --- NEW indicator in DHS8

# NOTES ------------------------------------------------------------------------
# In DHS8 knowledge of HIV prevention indicators are only reported among women and men age 15-24. Updates to these indicators include replacing as missing values women and men age 25 or older.
# Several indicators have also been discontinued in DHS8. Please check the excel indicator list for these indicators. 
# 
# 12 new indicators in DHS8, see below

# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)



# HIV RELATED KNOWLEDGE (WOMEN) ------------------------------------------------

# // Ever heard of HIV/AIDS
  IRdata <- IRdata %>% mutate(hk_ever_heard = v751)  %>%
    set_value_labels(hk_ever_heard = yesno) %>%
    set_variable_labels(hk_ever_heard = "Have ever heard of HIV or AIDS")

# // Heard of ARVs that treat HIV --- NEW indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_knw_arv = case_when(
    v837==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_arv = yesno) %>%
    set_variable_labels(hk_knw_arv = "Heard of ARVs that treat HIV")

#  // Know risk of HIV MTCT can be reduced by meds
  IRdata <- IRdata %>% mutate(hk_knw_mtct_meds = case_when(
    v824==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_mtct_meds = yesno) %>%
    set_variable_labels(hk_knw_mtct_meds = "Know risk of HIV mother to child transmission can be reduced by the mother taking special drugs")

#  // Heard of PrEP - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_knw_PrEP = case_when(
    v859 %in% 1:3 ~ 1,
    TRUE ~ 0 )) %>%
    set_value_labels(hk_knw_PrEP = yesno) %>%
    set_variable_labels(hk_knw_PrEP = "Heard of PrEP")
  
#  // Heard of PrEP and approve of people who take PrEP - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_aprov_PrEP = case_when(
    v859==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_aprov_PrEP = yesno) %>%
    set_variable_labels(hk_aprov_PrEP = "Heard of PrEP and approve of people who take PrEP to reduce their risk of HIV")


  
  
# ATTITUDES  (WOMEN)------------------------------------------------------------
    
# // Think that children with HIV should not go to school with HIV negative children
  IRdata <- IRdata %>% mutate(hk_atd_child_nosch = case_when(
          v857a==0 ~ 1,
          v751==0 ~ NA_real_,
          TRUE ~ 0)) %>% 
    set_value_labels(hk_atd_child_nosch = yesno) %>%	
    set_variable_labels(hk_atd_child_nosch = "Think that children living with HIV should not go to school with HIV negative children")
  
  #  //  Would not buy fresh vegetabels from a shopkeeper who has HIV
  IRdata <- IRdata %>% mutate(hk_atd_shop_notbuy = case_when( 
                              v825==0 ~ 1,
                              v751==0 ~ NA_real_,
                              TRUE ~ 0)) %>% 
  set_value_labels(hk_atd_shop_notbuy = yesno) %>%
  set_variable_labels(hk_atd_shop_notbuy = "Would not buy fresh vegetables from a shopkeeper who has HIV")
  
  #  // Have discriminatory attitudes towards people living with HIV-AIDS
  IRdata <- IRdata %>% mutate(hk_atd_discriminat = case_when(
    (v857a==0 | v825==0) ~ 1,
    v751==0 ~ NA_real_,
    TRUE ~ 0)) %>%
    set_value_labels(hk_atd_discriminat = yesno) %>%
    set_variable_labels(hk_atd_discriminat = "Have discriminatory attitudes towards people living with HIV")
  
# DISCLOSURE AND STIGMA  (WOMEN)------------------------------------------------
#  //Disclosed their HIV status to anyone - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_disclos_hiv= case_when(
    v865==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_disclos_hiv = yesno) %>%
    set_variable_labels(hk_disclos_hiv = "Self-reported as HIV positive and disclosed their HIV status to anyone")
  
#  //Felt ashamed of their HIV status - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_asham_hiv= case_when(
    v866==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_asham_hiv = yesno) %>%
    set_variable_labels(hk_asham_hiv = "Self-reported as HIV positive and felt ashamed of their HIV status") 
  
#  //Report that people talk badly about them because of HIV status  - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_tlkbad_hiv= case_when(
    v867a==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_tlkbad_hiv = yesno) %>%
    set_variable_labels(hk_tlkbad_hiv = "Self-reported as HIV positive and report that people talk badly about them because of HIV status in the past 12 months")
  
#  //Report that someone else disclosed their status without their permission  - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_othr_disclos_hiv= case_when(
    v867b==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_othr_disclos_hiv = yesno) %>%
  set_variable_labels(hk_othr_disclos_hiv = "Self-reported as HIV positive and report that someone else disclosed their status without their permission in the past 12 months") 
  
#  //Report being verbally insulted/harassed/theratened because of their HIV status  - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_harass_hiv=  case_when(
    v867c==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_harass_hiv = yesno) %>%
  set_variable_labels(hk_harass_hiv = "Self-reported as HIV positive and report being verbally insulted/harassed/theratened because of their HIV status in the past 12 months") 
  
#  //Report experiencing stigma in a community setting  - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_stigma_hiv= case_when(
    (v867a==1|v867b==1|v867c==1) & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_stigma_hiv = yesno) %>%
  set_variable_labels(hk_stigma_hiv = "Self-reported as HIV positive and report experiencing stigma in a community setting in the past 12 months")
  
#  //Report that healthcare workers talked badly because of HIV status  - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_hlthwrk_tlkbad_hiv= case_when(
    v867d==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_hlthwrk_tlkbad_hiv = yesno) %>%
  set_variable_labels(hk_hlthwrk_tlkbad_hiv = "Self-reported as HIV positive and report that healthcare workers talked badly because of HIV status in the past 12 months")
  
#  //Report that healthcare workers verbally abused them because of HIV status  - NEW Indicator in DHS8
  IRdata <- IRdata %>% mutate(hk_hlthwrk_vrbabuse_hiv= case_when(
    v867e==1 & v861==1 ~ 1,
    v861==1 ~ 0)) %>%
    set_value_labels(hk_hlthwrk_vrbabuse_hiv = yesno) %>%
  set_variable_labels(hk_hlthwrk_vrbabuse_hiv = "Self-reported as HIV positive and report that healthcare workers verbally abused them because of HIV status in the past 12 months") 
  


# INDICATORS ONLY AMONG YOUTH AGE 15-24 (WOMEN) --------------------------------
#  // Know reduce risk - use condoms
  IRdata <- IRdata %>% mutate(hk_knw_risk_cond = case_when(
    v754cp==1 & v012<25 ~ 1,
    v012<25 ~ 0)) %>%
    set_value_labels(hk_knw_risk_cond = yesno) %>%
    set_variable_labels(hk_knw_risk_cond = "Know you can reduce HIV risk by using condoms")
  
#  // Know reduce risk - limit to one partner
  IRdata <- IRdata %>% mutate(hk_knw_risk_sex = case_when(
    v754dp==1 & v012<25 ~ 1,
    v012<25 ~ 0)) %>%
    set_value_labels(hk_knw_risk_sex = yesno) %>%
    set_variable_labels(hk_knw_risk_sex = "Know you can reduce HIV risk by limiting to one uninfected sexual partner who has no other partners")
  
#  //Know healthy person can have HIV
  IRdata <- IRdata %>% mutate(hk_knw_hiv_hlth = case_when(
    v756==1 & v012<25 ~ 1,
    v012<25 ~ 0)) %>%
    set_value_labels(hk_knw_hiv_hlth = yesno) %>%
    set_variable_labels(hk_knw_hiv_hlth = "Know a healthy looking person can have HIV")
  
#  //Know HIV cannot be transmitted by mosquito bites
  IRdata <- IRdata %>% mutate(hk_knw_hiv_mosq = case_when(
    v754jp==0 & v012<25 ~ 1,
    v012<25 ~ 0)) %>%
    set_value_labels(hk_knw_hiv_mosq = yesno) %>%
    set_variable_labels(hk_knw_hiv_mosq = "Know HIV cannot be transmitted by mosquito bites")
  
  
#  //Know HIV cannot be transmitted by sharing food with HIV infected person
  IRdata <- IRdata %>% mutate(hk_knw_hiv_food= case_when(
    v754wp==0 & v012<25 ~ 1,
    v012<25 ~ 0)) %>%
    set_value_labels(hk_knw_hiv_food = yesno) %>%
    set_variable_labels(hk_knw_hiv_food = "Know cannot become infected by sharing food with a person who has HIV")
  
  
#  //Have all reported knowledge about HIV prevention among youth age 15-24
  IRdata <- IRdata %>% mutate(hk_knw_all= case_when(
    v754cp==1 & v754dp==1 & v756==1 & v754jp==0 & v754wp==0 & v012<25 ~ 1,
    v012<25 ~ 0)) %>%
    set_value_labels(hk_knw_all = yesno) %>%
    set_variable_labels(hk_knw_all = "Have all reported knowledge about HIV prevention among youth age 15-24")
  
  
  
# HIV RELATED KNOWLEDGE (MEN) ------------------------------------------------

# // Ever heard of HIV/AIDS
MRdata <- MRdata %>% mutate(hk_ever_heard = case_when(
  mv751==1 ~ 1,
  TRUE ~ 0))  %>%
  set_value_labels(hk_ever_heard = yesno) %>%
  set_variable_labels(hk_ever_heard = "Have ever heard of HIV or AIDS")

# // Heard of ARVs that treat HIV --- NEW indicator in DHS8
  MRdata <- MRdata %>% mutate(hk_knw_arv = case_when(
  mv837==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_arv = yesno) %>%
  set_variable_labels(hk_knw_arv = "Heard of ARVs that treat HIV")
  
#  //Know risk of HIV MTCT can be reduced by meds
MRdata <- MRdata %>% mutate(hk_knw_mtct_meds= case_when(
  mv824==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_mtct_meds = yesno) %>%
  set_variable_labels(hk_knw_mtct_meds = "Know risk of HIV mother to child transmission can be reduced by the mother taking special drugs")

#  // Heard of PrEP - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_knw_PrEP = case_when(
  mv859 %in% 1:3 ~ 1,
  TRUE ~ 0 )) %>%
  set_value_labels(hk_knw_PrEP = yesno) %>%
  set_variable_labels(hk_knw_PrEP = "Heard of PrEP")

#  // Heard of PrEP and approve of people who take PrEP - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_aprov_PrEP = case_when(
  mv859==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_aprov_PrEP = yesno) %>%
  set_variable_labels(hk_aprov_PrEP = "Heard of PrEP and approve of people who take PrEP to reduce their risk of HIV")


                            
                            
# ATTITUDES (MEN)---------------------------------------------------------------

# // Think that children with HIV should not go to school with HIV negative children
MRdata <- MRdata %>% mutate(hk_atd_child_nosch = case_when(
  mv857a==0 ~ 1,
  mv751==0 ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_atd_child_nosch = yesno) %>%	
  set_variable_labels(hk_atd_child_nosch = "Think that children living with HIV should not go to school with HIV negative children")

#  //  Would not buy fresh vegetabels from a shopkeeper who has HIV
MRdata <- MRdata %>% mutate(hk_atd_shop_notbuy= case_when(
  mv825==0 ~ 1,
  mv751==0 ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_atd_shop_notbuy = yesno) %>%
  set_variable_labels(hk_atd_shop_notbuy = "Would not buy fresh vegetables from a shopkeeper who has HIV")

#  // Have discriminatory attitudes towards people living with HIV-AIDS
MRdata <- MRdata %>% mutate(hk_atd_discriminat = case_when(
  (mv857a==0 | mv825==0) ~ 1,
  mv751==0 ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_atd_discriminat = yesno) %>%
  set_variable_labels(hk_atd_discriminat = "Have discriminatory attitudes towards people living with HIV")


# DISCLOSURE AND STIGMA  (MEN)------------------------------------------------
#  //Disclosed their HIV status to anyone - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_disclos_hiv= case_when(
  mv865==1 & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_disclos_hiv = yesno) %>%
  set_variable_labels(hk_disclos_hiv = "Self-reported as HIV positive and disclosed their HIV status to anyone")

#  //Felt ashamed of their HIV status - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_asham_hiv= case_when(
  mv866==1 & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_asham_hiv = yesno) %>%
  set_variable_labels(hk_asham_hiv = "Self-reported as HIV positive and felt ashamed of their HIV status") 

#  //Report that people talk badly about them because of HIV status  - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_tlkbad_hiv= case_when(
  mv867a==1 & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_tlkbad_hiv = yesno) %>%
  set_variable_labels(hk_tlkbad_hiv = "Self-reported as HIV positive and report that people talk badly about them because of HIV status in the past 12 months")

#  //Report that someone else disclosed their status without their permission  - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_othr_disclos_hiv= case_when(
  mv867b==1 & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_othr_disclos_hiv = yesno) %>%
  set_variable_labels(hk_othr_disclos_hiv = "Self-reported as HIV positive and report that someone else disclosed their status without their permission in the past 12 months") 

#  //Report being verbally insulted/harassed/theratened because of their HIV status  - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_harass_hiv=  case_when(
  mv867c==1 & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_harass_hiv = yesno) %>%
  set_variable_labels(hk_harass_hiv = "Self-reported as HIV positive and report being verbally insulted/harassed/theratened because of their HIV status in the past 12 months") 

#  //Report experiencing stigma in a community setting  - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_stigma_hiv= case_when(
  (mv867a==1|mv867b==1|mv867c==1) & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_stigma_hiv = yesno) %>%
  set_variable_labels(hk_stigma_hiv = "Self-reported as HIV positive and report experiencing stigma in a community setting in the past 12 months")

#  //Report that healthcare workers talked badly because of HIV status  - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_hlthwrk_tlkbad_hiv= case_when(
  mv867d==1 & mv861==1 ~ 1,
  mv861==1 ~ 0)) %>%
  set_value_labels(hk_hlthwrk_tlkbad_hiv = yesno) %>%
  set_variable_labels(hk_hlthwrk_tlkbad_hiv = "Self-reported as HIV positive and report that healthcare workers talked badly because of HIV status in the past 12 months")

#  //Report that healthcare workers verbally abused them because of HIV status  - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_hlthwrk_vrbabuse_hiv= case_when(
  mv867e==1 & mv861==1 ~ 1,
  mv861==0 ~ 0)) %>%
  set_value_labels(hk_hlthwrk_vrbabuse_hiv = yesno) %>%
  set_variable_labels(hk_hlthwrk_vrbabuse_hiv = "Self-reported as HIV positive and report that healthcare workers verbally abused them because of HIV status in the past 12 months") 



# INDICATORS ONLY AMONG YOUTH AGE 15-24  (MEN)----------------------------------

#  // Know reduce risk - use condoms
MRdata <- MRdata %>% mutate(hk_knw_risk_cond = case_when(
  mv754cp==1 & mv012<25 ~ 1,
  mv012 <25 ~ 0)) %>%
  set_value_labels(hk_knw_risk_cond = yesno) %>%
  set_variable_labels(hk_knw_risk_cond = "Know you can reduce HIV risk by using condoms")

#  // Know reduce risk - limit to one partner
MRdata <- MRdata %>% mutate(hk_knw_risk_sex = case_when(
  mv754dp==1 & mv012<25 ~ 1,
  mv012 <25 ~ 0)) %>%
  set_value_labels(hk_knw_risk_sex = yesno) %>%
  set_variable_labels(hk_knw_risk_sex = "Know you can reduce HIV risk by limiting to one uninfected sexual partner who has no other partners")

#  //Know healthy person can have HIV
MRdata <- MRdata %>% mutate(hk_knw_hiv_hlth = case_when(
  mv756==1 & mv012<25 ~ 1,
  mv012 <25 ~ 0)) %>%
  set_value_labels(hk_knw_hiv_hlth = yesno) %>%
  set_variable_labels(hk_knw_hiv_hlth = "Know a healthy looking person can have HIV")

#  //Know HIV cannot be transmitted by mosquito bites
MRdata <- MRdata %>% mutate(hk_knw_hiv_mosq = case_when(
  mv754jp==0 & mv012<25 ~ 1,
  mv012 <25 ~ 0)) %>% 
  set_value_labels(hk_knw_hiv_mosq = yesno) %>%
  set_variable_labels(hk_knw_hiv_mosq = "Know HIV cannot be transmitted by mosquito bites")

#  //Know HIV cannot be transmitted by sharing food with HIV infected person
MRdata <- MRdata %>% mutate(hk_knw_hiv_food= case_when(
  mv754wp==0 & mv012<25 ~ 1,
  mv012 <25 ~ 0)) %>%
  set_value_labels(hk_knw_hiv_food = yesno) %>%
  set_variable_labels(hk_knw_hiv_food = "Know cannot become infected by sharing food with a person who has HIV")

# //Knowledge of all reported HIV prevention items - NEW Indicator in DHS8
MRdata <- MRdata %>% mutate(hk_knw_all= case_when(
  mv754cp==1 & mv754dp==1 & mv756==1 & mv754jp==0 & mv754wp==0 & mv012<25 ~ 1,
  mv012 <25 ~ 0)) %>%
  set_value_labels(hk_knw_all = yesno) %>%
  set_variable_labels(hk_knw_all = "Have all reported knowledge about HIV prevention among youth age 15-24")
