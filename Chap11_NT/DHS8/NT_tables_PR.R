# /*****************************************************************************************************
# Program: 			NT_tables_PR.R
# Purpose: 			produce tables for indicators from PR file
# Author:				Shireen Assaf
# Date last modified: December 6, 2021 by Shireen Assaf
# 
# This do file will produce the following tables in excel:
# Tables_nut_ch:		Contains the tables for nutritional status and anemia indicators for children
# *****************************************************************************************************/

# * indicators from PR file
PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)
# 
# **************************************************************************************************
# * background variables
# 
# //Age in months
PRdata <- PRdata %>%
  mutate(agemonths = case_when(hc1<6~ 1, hc1%in%c(6,7,8)~ 2, hc1%in%c(9,10,11)~ 3, hc1>=12&hc1<=17~ 4, 
                               hc1>=18&hc1<=23~ 5, hc1>=24&hc1<=35~ 6, hc1>=36&hc1<=47~ 7, hc1>=48&hc1<=59~ 8)) %>%
  set_value_labels(agemonths = c("<6"=1, "6-8"=2, "9-11"=3, "12-17"=4, "18-23"=5, "24-35"=6, "36-47"=7, "48-59"=8 )) %>%
  set_variable_labels(agemonths = "Age of child months categories")

# Note: other background variables such as size at birth (from KR file) and mother's BMI (from IR) are found in other files and need to be merged with PR file
# These tables are only for background variables available in the PR file
# 
# **************************************************************************************************
# * Anthropometric indicators for children under age 5
# **************************************************************************************************
table_temp <-  PRdata %>% 
  calc_cro_rpct(
    cell_vars = list(agemonths, hc27, hv025, hv024, hv270, total()),
    col_vars = list(nt_ch_sev_stunt, nt_ch_stunt, nt_ch_mean_haz, nt_ch_sev_wast, nt_ch_wast, 
                    nt_ch_ovwt_ht, nt_ch_mean_whz, nt_ch_sev_underwt, nt_ch_underwt, nt_ch_ovwt_age, nt_ch_mean_waz),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Child's anthropometric indicators")
#Note the mean haz, whz, and waz are computed for the total and not by each background variable. 
write.xlsx(table_temp, "Chap11_NT/Tables_nut_ch.xls", sheetName = "child_anthro", append=TRUE)

# **************************************************************************************************
# * Anemia in children 6-59 months
# **************************************************************************************************
table_temp <-  PRdata %>% 
  calc_cro_rpct(
    cell_vars = list(agemonths, hc27, hv025, hv024, hv270, total()),
    col_vars = list(nt_ch_any_anem,nt_ch_mild_anem,nt_ch_mod_anem,nt_ch_sev_anem),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Child's anemia indicators")
write.xlsx(table_temp, "Chap11_NT/Tables_nut_ch.xls", sheetName = "child_anemia", append=TRUE)



