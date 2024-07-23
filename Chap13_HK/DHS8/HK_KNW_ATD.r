# ******************************************************************************
# Program: 			  HK_KNW_ATD.R
# Purpose: 			  Code to compute HIV-AIDS related knowledge and attitude indicators 
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# ******************************************************************************

# Variables created in this file ------------------------------------------------
#   
# hk_ever_heard			  "Have ever heard of HIV or AIDS"
# hk_knw_risk_cond		"Know you can reduce HIV risk by using condoms at every sex"
# hk_knw_risk_sex			"Know you can reduce HIV risk by limiting to one uninfected sexual partner who has no other partners"
# hk_knw_risk_condsex	"Know you can reduce HIV risk by using condoms at every sex and limiting to one uninfected partner with no other partner"
# hk_knw_hiv_hlth			"Know a healthy looking person can have HIV"
# hk_knw_hiv_mosq			"Know HIV cannot be transmitted by mosquito bites"
# hk_knw_hiv_supernat	"Know HIV cannot be transmitted by supernatural means"
# hk_knw_hiv_food			"Know cannot become infected by sharing food with a person who has HIV"
# hk_knw_hiv_hlth_2miscp	"Know a healthy looking person can have HIV and reject the two most common local misconceptions"
# hk_knw_comphsv			"Have comprehensive knowledge about HIV"
# 
# hk_knw_mtct_preg		"Know HIV mother to child transmission can occur during pregnancy"
# hk_knw_mtct_deliv		"Know HIV mother to child transmission can occur during delivery"
# hk_knw_mtct_brfeed	"Know HIV mother to child transmission can occur by breastfeeding"
# hk_knw_mtct_all3		"Know HIV mother to child transmission can occur during pregnancy, delivery, and by breastfeeding"
# hk_knw_mtct_meds		"Know risk of HIV mother to child transmission can be reduced by the mother taking special drugs"
# 
# hk_atd_child_nosch	"Think that children living with HIV should not go to school with HIV negative children"
# hk_atd_shop_notbuy	"Would not buy fresh vegetables from a shopkeeper who has HIV"
# hk_atd_discriminat	"Have discriminatory attitudes towards people living with HIV"


# NOTES ------------------------------------------------------------------------
# Indicator hk_knw_hiv_hlth_2miscp (line 85) is country specific, please check the final report for the two most common misconceptions.
# Currently coded as rejecting that HIV can be transmitted by mosquito bites and supernatural means.


  
# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)



# HIV RELATED KNOWLEDGE (WOMEN) ------------------------------------------------


# // Ever heard of HIV/AIDS
  IRdata <- IRdata %>% mutate(hk_ever_heard = v751)  %>%
    set_value_labels(hk_ever_heard = yesno) %>%
    set_variable_labels(hk_ever_heard = "Have ever heard of HIV or AIDS")
  
#  // Know reduce risk - use condoms
  IRdata <- IRdata %>% mutate(hk_knw_risk_cond = case_when(
    v754cp==1 ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(hk_knw_risk_cond = yesno) %>%
    set_variable_labels(hk_knw_risk_cond = "Know you can reduce HIV risk by using condoms")
  
#  // Know reduce risk - limit to one partner
  IRdata <- IRdata %>% mutate(hk_knw_risk_sex = case_when(
    v754dp==1 ~ 1, 
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_risk_sex = yesno) %>%
    set_variable_labels(hk_knw_risk_sex = "Know you can reduce HIV risk by limiting to one uninfected sexual partner who has no other partners")
  
#  // Know reduce risk - use condoms and limit to one partner
  IRdata <- IRdata %>% mutate(hk_knw_risk_condsex = case_when(
    v754cp==1 & v754dp==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_risk_condsex = yesno) %>%
    set_variable_labels(hk_knw_risk_condsex = "Know you can reduce HIV risk by using condoms and limiting to one uninfected partner")
  
#  //Know healthy person can have HIV
  IRdata <- IRdata %>% mutate(hk_knw_hiv_hlth = case_when(
    v756==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_hiv_hlth = yesno) %>%
    set_variable_labels(hk_knw_hiv_hlth = "Know a healthy looking person can have HIV")
  
#  //Know HIV cannot be transmitted by mosquito bites
  IRdata <- IRdata %>% mutate(hk_knw_hiv_mosq = case_when(
    v754jp==0 ~ 1,
    TRUE ~ 0 )) %>% 
    set_value_labels(hk_knw_hiv_mosq = yesno) %>%
    set_variable_labels(hk_knw_hiv_mosq = "Know HIV cannot be transmitted by mosquito bites")
  
#  //Know HIV cannot be transmitted by supernatural means
  IRdata <- IRdata %>% mutate(hk_knw_hiv_supernat = case_when(
    v823==0 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_hiv_supernat = yesno) %>%
    set_variable_labels(hk_knw_hiv_supernat = "Know HIV cannot be transmitted by supernatural means")
  
#  //Know HIV cannot be transmitted by sharing food with HIV infected person
  IRdata <- IRdata %>% mutate(hk_knw_hiv_food= case_when(
    v754wp==0 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_hiv_food = yesno) %>%
    set_variable_labels(hk_knw_hiv_food = "Know cannot become infected by sharing food with a person who has HIV")
  
#  //Know healthy person can have HIV and reject two common local misconceptions
  # ***NOTE*** - the two most common misconceptions are country-specific and this must be adapted
  IRdata <- IRdata %>% mutate(hk_knw_hiv_hlth_2miscp= case_when(
    v756==1 & v754jp==0 & v754wp==0 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_hiv_hlth_2miscp = yesno) %>%
    set_variable_labels(hk_knw_hiv_hlth_2miscp = "Know a healthy looking person can have HIV and reject the two most common local misconceptions")
  
#  //HIV comprehensive knowledge
  IRdata <- IRdata %>% mutate(hk_knw_comphsv= case_when(
    hk_knw_risk_condsex==1 & hk_knw_hiv_hlth==1 & hk_knw_hiv_hlth_2miscp==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_comphsv =yesno) %>%
    set_variable_labels(hk_knw_comphsv = "Have comprehensive knowledge about HIV")
  
#  //Know HIV MTCT can occur during pregnancy
  IRdata <- IRdata %>% mutate(hk_knw_mtct_preg= case_when(
    v774a==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_mtct_preg = yesno) %>%
    set_variable_labels(hk_knw_mtct_preg = "Know HIV mother to child transmission can occur during pregnancy")
  
#  //Know HIV MTCT can occur during delivery
  IRdata <- IRdata %>% mutate(hk_knw_mtct_deliv = case_when(
    v774b==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_mtct_deliv = yesno) %>%
    set_variable_labels(hk_knw_mtct_deliv = "Know HIV mother to child transmission can occur during delivery")
  
#  //Know HIV MTCT can occur during breastfeeding
  IRdata <- IRdata %>% mutate(hk_knw_mtct_brfeed= case_when(
    v774c==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_mtct_brfeed = yesno) %>%
    set_variable_labels(hk_knw_mtct_brfeed = "Know HIV mother to child transmission can occur by breastfeeding")
  
#  //Know all three HIV MTCT
  IRdata <- IRdata %>% mutate(hk_knw_mtct_all3= case_when(
    v774a==1 & v774b==1 & v774c==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_mtct_all3 = yesno) %>%
    set_variable_labels(hk_knw_mtct_all3 = "Know HIV mother to child transmission can occur during pregnancy, delivery, and by breastfeeding")
  
#  //Know risk of HIV MTCT can be reduced by meds
  IRdata <- IRdata %>% mutate(hk_knw_mtct_meds = case_when(
    v824==1 ~ 1,
    TRUE ~ 0)) %>%
    set_value_labels(hk_knw_mtct_meds = yesno) %>%
    set_variable_labels(hk_knw_mtct_meds = "Know risk of HIV mother to child transmission can be reduced by the mother taking special drugs")
  

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
  


  
  
  

# HIV RELATED KNOWLEDGE (MEN) ------------------------------------------------


# // Ever heard of HIV/AIDS
MRdata <- MRdata %>% mutate(hk_ever_heard = case_when(
  mv751==1 ~ 1,
  TRUE ~ 0))  %>%
  set_value_labels(hk_ever_heard = yesno) %>%
  set_variable_labels(hk_ever_heard = "Have ever heard of HIV or AIDS")

#  // Know reduce risk - use condoms
MRdata <- MRdata %>% mutate(hk_knw_risk_cond = case_when(
  mv754cp==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_risk_cond = yesno) %>%
  set_variable_labels(hk_knw_risk_cond = "Know you can reduce HIV risk by using condoms")

#  // Know reduce risk - limit to one partner
MRdata <- MRdata %>% mutate(hk_knw_risk_sex = case_when(
  mv754dp==1 ~ 1, 
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_risk_sex = yesno) %>%
  set_variable_labels(hk_knw_risk_sex = "Know you can reduce HIV risk by limiting to one uninfected sexual partner who has no other partners")

#  // Know reduce risk - use condoms and limit to one partner
MRdata <- MRdata %>% mutate(hk_knw_risk_condsex = case_when(
  mv754cp==1 & mv754dp==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_risk_sex = yesno) %>%
  set_variable_labels(hk_knw_risk_condsex = "Know you can reduce HIV risk by using condoms and limiting to one uninfected partner")

#  //Know healthy person can have HIV
MRdata <- MRdata %>% mutate(hk_knw_hiv_hlth = case_when(
  mv756==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_hiv_hlth = yesno) %>%
  set_variable_labels(hk_knw_hiv_hlth = "Know a healthy looking person can have HIV")

#  //Know HIV cannot be transmitted by mosquito bites
MRdata <- MRdata %>% mutate(hk_knw_hiv_mosq = case_when(
  mv754jp==0 ~ 1,
  TRUE ~ 0)) %>% 
  set_value_labels(hk_knw_hiv_mosq = yesno) %>%
  set_variable_labels(hk_knw_hiv_mosq = "Know HIV cannot be transmitted by mosquito bites")

#  //Know HIV cannot be transmitted by supernatural means
MRdata <- MRdata %>% mutate(hk_knw_hiv_supernat = case_when(
  mv823==0 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_hiv_supernat = yesno) %>%
  set_variable_labels(hk_knw_hiv_supernat = "Know HIV cannot be transmitted by supernatural means")

#  //Know HIV cannot be transmitted by sharing food with HIV infected person
MRdata <- MRdata %>% mutate(hk_knw_hiv_food= case_when(
  mv754wp==0 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_hiv_food = yesno) %>%
  set_variable_labels(hk_knw_hiv_food = "Know cannot become infected by sharing food with a person who has HIV")

#  //Know healthy person can have HIV and reject two common local misconceptions
MRdata <- MRdata %>% mutate(hk_knw_hiv_hlth_2miscp= case_when(
  mv756==1 & mv754jp==0 & mv823==0 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_hiv_hlth_2miscp = yesno) %>%
  set_variable_labels(hk_knw_hiv_hlth_2miscp = "Know a healthy looking person can have HIV and reject the two most common local misconceptions")

#  //HIV comprehensive knowledge
MRdata <- MRdata %>% mutate(hk_knw_comphsv= case_when(
  mv754cp==1 & mv754dp==1 & mv756==1 & mv754jp==0 & mv823==0 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_comphsv =yesno) %>%
  set_variable_labels(hk_knw_comphsv = "Have comprehensive knowledge about HIV")

#  //Know HIV MTCT can occur during pregnancy
MRdata <- MRdata %>% mutate(hk_knw_mtct_preg= case_when(
  mv774a==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_mtct_preg = yesno) %>%
  set_variable_labels(hk_knw_mtct_preg = "Know HIV mother to child transmission can occur during pregnancy")

#  //Know HIV MTCT can occur during delivery
MRdata <- MRdata %>% mutate(hk_knw_mtct_deliv = case_when(
  mv774b==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_mtct_deliv = yesno) %>%
  set_variable_labels(hk_knw_mtct_deliv = "Know HIV mother to child transmission can occur during delivery")

#  //Know HIV MTCT can occur during breastfeeding
MRdata <- MRdata %>% mutate(hk_knw_mtct_brfeed= case_when(
  mv774c==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_mtct_brfeed = yesno) %>%
  set_variable_labels(hk_knw_mtct_brfeed = "Know HIV mother to child transmission can occur by breastfeeding")

#  //Know all three HIV MTCT
MRdata <- MRdata %>% mutate(hk_knw_mtct_all3= case_when(
  mv774a==1 & mv774b==1 & mv774c==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_mtct_all3 = yesno) %>%
  set_variable_labels(hk_knw_mtct_all3 = "Know HIV mother to child transmission can occur during pregnancy, delivery, and by breastfeeding")

#  //Know risk of HIV MTCT can be reduced by meds
MRdata <- MRdata %>% mutate(hk_knw_mtct_meds= case_when(
  mv824==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_knw_mtct_meds = yesno) %>%
  set_variable_labels(hk_knw_mtct_meds = "Know risk of HIV mother to child transmission can be reduced by the mother taking special drugs")
                            
                            
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
