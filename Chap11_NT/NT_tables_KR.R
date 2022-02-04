# /*****************************************************************************************************
# Program: 			NT_tables_KR.R
# Purpose: 			produce tables for indicators from KR file
# Author:				Shireen Assaf
# Date last modified: December 6, 2021 by Shireen Assaf
# 
# *This do file will produce the following tables in excel:
# 1. 	Tables_bf:	Contains the tables for breastfeeding indicators
# 2.	Tables_IYCF:		Contains the tables for IYCF indicators in children
# 3. 	Tables_nut_ch.xls:	Contains the tables for micronutrient intake in children

# *****************************************************************************************************/

# indicators from KR file
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# **************************************************************************************************
# background variables

# //Age in months
KRdata <- KRdata %>%
  mutate(agemonths = case_when(age<6~ 1, age%in%c(6,7,8)~ 2, age%in%c(9,10,11)~ 3, age>=12&age<=17~ 4, 
                               age>=18&age<=23~ 5, age>=24&age<=35~ 6, age>=36&age<=47~ 7, age>=48&age<=59~ 8)) %>%
  set_value_labels(agemonths = c("<6"=1, "6-8"=2, "9-11"=3, "12-17"=4, "18-23"=5, "24-35"=6, "36-47"=7, "48-59"=8 )) %>%
  set_variable_labels(agemonths = "Age of child months categories: 0-59")

# //Age categories for children 0-23
KRdata <- KRdata %>%
  mutate(agecats = case_when(age<2~ 1, age%in%c(2,3)~ 2, age%in%c(4,5)~ 3, age%in%c(6,7,8)~ 4, 
                             age%in%c(9,10,11)~ 5, age>=12&age<=17~ 6, age>=18&age<=23~ 7, age>23~99)) %>%
  replace_with_na(replace = list(agecats = c(99))) %>%
  set_value_labels(agecats = c("0-1"=1, "2-3"=2, "4-5"=3, "6-8"=4, "9-11"=5, "12-17"=6, "18-23"=7 )) %>%
  set_variable_labels(agecats = "Age of child months categories: 0-23")

# //Place of delivery
KRdata <- KRdata %>%
  mutate(del_place = case_when(inrange(m15,20,39)~ 1, inrange(m15,10,19)~ 2, inrange(m15,40,99)~ 3)) %>%
  set_value_labels(del_place = c("Health facility"=1, "Home"=2, "Other/Missing"=3 )) %>%
  set_variable_labels(del_place = "Place of delivery")

# //Assistance during delivery
# **Note: Assistance during delivery are country specific indicators. Check final report to know if these are coded correctly. 
KRdata <- KRdata %>%
  mutate(del_pv =
           case_when(
             m3a == 1 | m3b == 1 |  m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1 ~ 1 ,
             m3g == 1 ~ 2 ,
             m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 3 ,
             m3n ==1 ~ 4,
             m3a ==8 | m3a==9 | age>=60 ~ 99)) %>%
  replace_with_na(replace = list(del_pv = c(99))) %>%
  set_value_labels(del_pv = c("Health personnel" = 1, "Traditional birth attendant"=2, "Other"=3, "No one"=4  )) %>%
  set_variable_labels(del_pv = "Assistance during delivery")

# //Mother's age 
KRdata <- KRdata %>%
  mutate(agem = case_when(v013==1  ~ 1, v013 %in% c(2,3) ~ 2, v013 %in% c(4,5) ~ 3, v013%in% c(6,7) ~ 4, v013>7 ~ 5 )) %>%
  set_value_labels(agem = c("15-19"=1, "20-29"=2, "30-39"=3, "40-49"=4, "50+"=5)) %>%
  set_variable_labels(agem = "Mother's age categories")

# //Wasting status
KRdata <- KRdata %>%
  mutate(waste =
           case_when(
             hw72 < -300  ~ 1 ,
             hw72 >= -300  & hw72 < -200 ~ 2 ,
             hw72 >= -200  & hw72 <9996 ~ 3 ,
             hw72 >= 9996 ~ 99)) %>%
  replace_with_na(replace = list(waste = c(99))) %>%
  set_value_labels(waste = c("Severe acute malnutrition" = 1, "Moderate acute malnutrition"=2, "Not wasted"=3  )) %>%
  set_variable_labels(waste = "Wasted child under 5 years")

# **************************************************************************************************
# * Initial breastfeeding 
# **************************************************************************************************
table_temp <-  KRdata %>% 
  calc_cro_rpct(
    cell_vars = list(b4, del_pv, del_place, v025, v024, v106, v190, total()),
    col_vars = list(nt_bf_ever, nt_bf_start_1hr, nt_bf_start_1day, nt_bf_prelac),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Breast feeding indicators")
write.xlsx(table_temp, "Chap11_NT/Tables_bf.xls", sheetName = "initialbf", append=TRUE)

# **************************************************************************************************
# * Micronutrient intake
# **************************************************************************************************
table_temp <-  KRdata %>% 
  calc_cro_rpct(
    cell_vars = list(agemonths, b4, agem, v025, v024, v106, v190, total()),
    col_vars = list(nt_ch_micro_mp, nt_ch_micro_iron, nt_ch_micro_vas, nt_ch_micro_dwm, nt_ch_micro_iod, nt_ch_food_supp),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Micronutrient intake")
write.xlsx(table_temp, "Chap11_NT/Tables_nut_ch.xls", sheetName = "micro_allchild", append=TRUE)

# ****************************************************
# //Bottle feeding
table_temp <-  KRdata %>% 
  calc_cro_rpct(
    cell_vars = list(agecats, total()),
    col_vars = list(nt_bottle),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Bottle feeding")
write.xlsx(table_temp, "Chap11_NT/Tables_bf.xls", sheetName = "bottlefd", append=TRUE)

# **************************************************************************************************

# Indicators from KR file restricted to youngest child under 2 years living with the mother (KRiycf subset data)
KRiycf <- KRiycf %>%
  mutate(wt = v005/1000000)

# **************************************************************************************************
# background variables

# //Age categories for children 0-23
KRiycf <- KRiycf %>%
  mutate(agecats = case_when(age<2~ 1, age%in%c(2,3)~ 2, age%in%c(4,5)~ 3, age%in%c(6,7,8)~ 4, 
                             age%in%c(9,10,11)~ 5, age>=12&age<=17~ 6, age>=18&age<=23~ 7, age>23~99)) %>%
  replace_with_na(replace = list(agecats = c(99))) %>%
  set_value_labels(agecats = c("0-1"=1, "2-3"=2, "4-5"=3, "6-8"=4, "9-11"=5, "12-17"=6, "18-23"=7 )) %>%
  set_variable_labels(agecats = "Age of child months categories: 0-23")

# //Mother's age 
KRiycf <- KRiycf %>%
  mutate(agem = case_when(v013==1  ~ 1, v013 %in% c(2,3) ~ 2, v013 %in% c(4,5) ~ 3, v013%in% c(6,7) ~ 4, v013>7 ~ 5 )) %>%
  set_value_labels(agem = c("15-19"=1, "20-29"=2, "30-39"=3, "40-49"=4, "50+"=5)) %>%
  set_variable_labels(agem = "Mother's age categories")

# **************************************************************************************************
# * Breastfeeding status 
# **************************************************************************************************
# //Breastfeeding status
table_temp <-  KRiycf %>% 
  calc_cro_rpct(
    cell_vars = list(agecats, total()),
    col_vars = list(nt_bf_status, nt_bf_curr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Breast feeding status")
write.xlsx(table_temp, "Chap11_NT/Tables_bf.xls", sheetName = "bfstatus", append=TRUE)

# **************************************************************************************************
# * IYCF indicators
# **************************************************************************************************
# //Breastfeeding status
table_temp <-  KRiycf %>% 
  calc_cro_rpct(
    cell_vars = list(total()),
    col_vars = list(nt_ebf, nt_bf_cont_1yr, nt_bf_cont_2yr,nt_food_bf, nt_ageapp_bf, nt_predo_bf),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("IYCF indicators on breastfeeding status")
write.xlsx(table_temp, "Chap11_NT/Tables_IYCF.xls", sheetName = "IYCFbf", append=TRUE)

# **************************************************************************************************
# * Foods and liquids consumed
# **************************************************************************************************
# 
# *** Among breastfeeding children ***
table_temp <-  KRiycf %>% 
  filter(nt_bf_curr==1) %>%
  calc_cro_rpct(
    cell_vars = list(agecats,total()),
    col_vars = list(nt_formula, nt_milk, nt_liquids, nt_bbyfood, nt_grains, nt_vita, nt_frtveg, nt_root, 
                    nt_nuts, nt_meatfish, nt_eggs, nt_dairy, nt_solids),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Food and liquids consumed - bf children")
write.xlsx(table_temp, "Chap11_NT/Tables_IYCF.xls", sheetName = "foods_bfchild", append=TRUE)

# *** Among non-breastfeeding children ***
table_temp <-  KRiycf %>% 
  filter(nt_bf_curr==0) %>%
  calc_cro_rpct(
    cell_vars = list(agecats,total()),
    col_vars = list(nt_formula, nt_milk, nt_liquids, nt_bbyfood, nt_grains, nt_vita, nt_frtveg, nt_root, 
                    nt_nuts, nt_meatfish, nt_eggs, nt_dairy, nt_solids),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Food and liquids consumed - non-bf children")
write.xlsx(table_temp, "Chap11_NT/Tables_IYCF.xls", sheetName = "foods_nonbfchild", append=TRUE)

# **************************************************************************************************
# * Minimum acceptable diet
# **************************************************************************************************

# *** Among breastfeeding children ***
table_temp <-  KRiycf %>% 
  filter(nt_bf_curr==1) %>%
  calc_cro_rpct(
    cell_vars = list(agecats,total()),
    col_vars = list(nt_mdd, nt_mmf, nt_mad ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Minimum acceptable diet - bf children")
write.xlsx(table_temp, "Chap11_NT/Tables_IYCF.xls", sheetName = "mindiet_bfchild", append=TRUE)

# *** Among non-breastfeeding children ***
table_temp <-  KRiycf %>% 
  filter(nt_bf_curr==0) %>%
  calc_cro_rpct(
    cell_vars = list(agecats,total()),
    col_vars = list(nt_fed_milk, nt_mdd, nt_mmf, nt_mad ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Minimum acceptable diet - non-bf children")
write.xlsx(table_temp, "Chap11_NT/Tables_IYCF.xls", sheetName = "mindiet_nonbfchild", append=TRUE)

# *** Among all children ***
table_temp <-  KRiycf %>% 
  calc_cro_rpct(
    cell_vars = list(agecats,total()),
    col_vars = list(nt_fed_milk, nt_mdd, nt_mmf, nt_mad ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Minimum acceptable diet - all children")
write.xlsx(table_temp, "Chap11_NT/Tables_IYCF.xls", sheetName = "mindiet_allchild", append=TRUE)

# **************************************************************************************************
# * Micronutrient intake
# **************************************************************************************************
table_temp <-  KRiycf %>% 
  calc_cro_rpct(
    cell_vars = list(agecats,b4,nt_bf_curr,agem, v025, v024, v106,v190, total()),
    col_vars = list(nt_ch_micro_vaf, nt_ch_micro_irf),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Micronutrient intake - youngest children age 6-23 living with the mother")
write.xlsx(table_temp, "Chap11_NT/Tables_nut_ch.xls.xls", sheetName = "micro_youngestchild", append=TRUE)

# **************************************************************************************************
