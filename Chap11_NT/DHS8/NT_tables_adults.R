# /*****************************************************************************************************
# Program: 			NT_tables_adults.R
# Purpose: 			produce tables for indicators from IR and MR files
# Author:				Shireen Assaf
# Date last modified: December 6, 2021 by Shireen Assaf
# 
# *This do file will produce the following tables in excel:
# 1. Tables_nut_wm:	Contains the tables for nutritional status indicators, anemia, and  micronutrient intake for women
# 2. Tables_nut_mn:		Contains the tables for nutritional status indicators, anemia, and  micronutrient intake for men

# The indicators are filtered for age 15-49. This can be changed if required for all women/men. 
# *****************************************************************************************************/

# indicators from IR file
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# **************************************************************************************************
# background variables

# //Age 
IRdata <- IRdata %>%
  mutate(agecat = case_when(v013==1  ~ 1, v013 %in% c(2,3) ~ 2, v013 %in% c(4,5) ~ 3, v013%in% c(6,7) ~ 4, v013>7 ~ 5 )) %>%
  set_value_labels(agecat = c("15-19"=1, "20-29"=2, "30-39"=3, "40-49"=4, "50+"=5)) %>%
  set_variable_labels(agecat = "Women age categories")

# //Number of children ever born
IRdata <- IRdata %>%
  mutate(ceb = case_when(v201==0  ~ 0, v201==1 ~ 1, v201 %in% c(2,3) ~ 2, v201 %in% c(4,5) ~ 3, v201>=6 ~ 4)) %>%
  set_value_labels(ceb = c("0"=0, "1"=1, "2-3"=2, "4-5"=3, "6+"=4)) %>%
  set_variable_labels(ceb = "Number of children ever born")

# //IUD use
IRdata <- IRdata %>%
  mutate(iud = if_else(v312==2,1,0)) %>%
  set_value_labels(iud = c("No"=0, "Yes"=1)) %>%
  set_variable_labels(iud = "IUD use")

# //Maternity status
IRdata <- IRdata %>%
  mutate(mstat = case_when( v213==1 ~ 1, v404==1 & v213!=1 ~ 2, TRUE  ~ 3)) %>%
  set_value_labels(mstat = c("Pregnant"=1, "Breastfeeding"=2, "Neither"=3)) %>%
  set_variable_labels(mstat = "Maternity status")

# **************************************************************************************************
# * Nutritional status of women
# **************************************************************************************************
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(agecat, v025, v024, v106, v190, total()),
    col_vars = list(nt_wm_ht, nt_wm_bmi_mean, nt_wm_norm, nt_wm_thin, nt_wm_mthin, nt_wm_modsevthin, nt_wm_ovobese, nt_wm_ovwt, nt_wm_obese),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Women's nutritional status indicators")
#Note the mean bmi is computed for the total and not by each background variable. 
write.xlsx(table_temp, "Chap11_NT/Tables_nut_wm.xls", sheetName = "women_nut", append=TRUE)

# **************************************************************************************************
# * Anemia in women
# **************************************************************************************************
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(agecat, ceb, mstat, iud, v463a, v025, v024, v106, v190, total()),
    col_vars = list(nt_wm_any_anem, nt_wm_mild_anem, nt_wm_mod_anem, nt_wm_sev_anem),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Women's anemia status")
write.xlsx(table_temp, "Chap11_NT/Tables_nut_wm.xls", sheetName = "women_anemia", append=TRUE)

# **************************************************************************************************
# * Micronutrient intake among women
# **************************************************************************************************
# //Number of days women took iron tablets or syrup during pregnancy 
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(agecat, ceb, mstat, iud, v463a, v025, v024, v106, v190, total()),
    col_vars = list(nt_wm_micro_iron, nt_wm_micro_dwm, nt_wm_micro_iod),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Women's micronutrient intake")
write.xlsx(table_temp, "Chap11_NT/Tables_nut_wm.xls", sheetName = "women_micronut", append=TRUE)


# **************************************************************************************************
# **************************************************************************************************

# * indicators from MR file
MRdata <- MRdata %>%
  mutate(wt = mv005/1000000)

# **************************************************************************************************
# background variables
# 
# //Age 
MRdata <- MRdata %>%
  mutate(agecat = case_when(mv013==1  ~ 1, mv013 %in% c(2,3) ~ 2, mv013 %in% c(4,5) ~ 3, mv013%in% c(6,7) ~ 4, mv013>7 ~ 5 )) %>%
  set_value_labels(agecat = c("15-19"=1, "20-29"=2, "30-39"=3, "40-49"=4, "50+"=5)) %>%
  set_variable_labels(agecat = "Men age categories")

# //Smokes cigarettes
MRdata <- MRdata %>%
  mutate(smoke = if_else(inrange(mv464a,1,888) | inrange(mv464b,1,888) | inrange(mv464c,1,888) | inrange(mv484a,1,888) | 
                           inrange(mv484b,1,888) | inrange(mv484c,1,888)  , 1, 0 )) %>%
  set_value_labels(smoke = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(smoke = "Smokes cigarettes")

# **************************************************************************************************
# * Nutritional status of men
# **************************************************************************************************
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(agecat, mv025, mv024, mv106, mv190, total()),
    col_vars = list(nt_mn_bmi_mean, nt_mn_norm, nt_mn_thin, nt_mn_mthin, nt_mn_modsevthin, nt_mn_ovobese, nt_mn_ovwt, nt_mn_obese),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Women's nutritional status indicators")
#Note the mean bmi is computed for the total and not by each background variable. 
write.xlsx(table_temp, "Chap11_NT/Tables_nut_mn.xls", sheetName = "men_nut", append=TRUE)

# **************************************************************************************************
# * Anemia in men
# **************************************************************************************************
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(agecat, smoke, mv025, mv024, mv106, mv190, total()),
    col_vars = list(nt_mn_any_anem),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Women's anemia status")
write.xlsx(table_temp, "Chap11_NT/Tables_nut_mn.xls", sheetName = "men_anemia", append=TRUE)
