# /*****************************************************************************************************
# Program: 			WE_tables.R
# Purpose: 			produce tables for indicators
# Author:				Shireen Assaf
# Date last modified: November 22, 2021 by Shireen Assaf 
# 
# *This do file will produce the following tables in excel:
# 1. 	Tables_emply:	  Contains the tables for employment and earning indicators for women and men
# 2. 	Tables_assets:	Contains the tables for asset ownership indicators for women and men
# 3. 	Tables_empw:		Contains the tables for empowerment indicators, justification of wife beating, and decision making for women and men
# 		
# For women and men the indicators are filtered for age 15-49. This can be changed if required for all women/men. 
# *****************************************************************************************************/
# 
# sample weight 
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

MRdata <- MRdata %>%
  mutate(wt = mv005/1000000)


# Number of living children (variable needed for tables)
IRdata <- IRdata %>%
  mutate(numch = case_when(v218==0  ~ 0, v218 %in% c(1,2) ~ 1, v218 %in% c(3,4) ~ 2, v218>4 ~ 3)) %>%
  set_value_labels(numch = c("0"=0, "1-2"=1, "3-4"=2, "5+"=3  )) %>%
  set_variable_labels(numch = "Number of living children")

MRdata <- MRdata %>%
  mutate(numch = case_when(mv218==0  ~ 0, mv218 %in% c(1,2) ~ 1, mv218 %in% c(3,4) ~ 2, mv218>4 ~ 3)) %>%
  set_value_labels(numch = c("0"=0, "1-2"=1, "3-4"=2, "5+"=3  )) %>%
  set_variable_labels(numch = "Number of living children")

# **************************************************************************************************
# * Employment and earnings
# **************************************************************************************************
# //Employment in the last 12 months
# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, total()),
    col_vars = list(we_empl, we_empl_earn),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Women's employment and type of earning by age")
write.xlsx(table_temp, "Chap15_WE/Tables_emply.xls", sheetName = "employ_earn_women", append=TRUE)

# For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, total()),
    col_vars = list(we_empl, we_empl_earn),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Men's employment and type of earning by age")
write.xlsx(table_temp, "Chap15_WE/Tables_emply.xls", sheetName = "employ_earn_men", append=TRUE)

# ****************************************************
# //Decision on wife's cash earnings

table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, numch, v025, v024, v190, total()),
    col_vars = list(we_earn_wm_decide, we_earn_wm_compare),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Control over women's cash earning and comparison of women to husband cash earnings")
write.xlsx(table_temp, "Chap15_WE/Tables_emply.xls", sheetName = "cash_earn_women", append=TRUE)

# //Decision on men's cash earnings
# From men's response
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, numch, mv025, mv024, mv190, total()),
    col_vars = list(we_earn_mn_decide),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Control over men's cash earnings - from men's response")
write.xlsx(table_temp, "Chap15_WE/Tables_emply.xls", sheetName = "cash_earn_men", append=TRUE)

# From women's response
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, numch, v025, v024, v190, total()),
    col_vars = list(we_earn_hs_decide),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Control over men's cash earnings - from women's response")
write.xlsx(table_temp, "Chap15_WE/Tables_emply.xls", sheetName = "cash_earn_men_wm", append=TRUE)

# ****************************************************
# //Decision on wife or husband's cash earnings by comparison of wife to husband's earnings
# first construct a new variable that includes women that do not earn cash or not working
IRdata <- IRdata %>%
  mutate(we_earn_wm_compare2 =
           case_when(
             we_earn_wm_compare == 1   ~ 1 ,
             we_earn_wm_compare == 2   ~ 2 ,
             we_earn_wm_compare == 3   ~ 3 ,
             we_earn_wm_compare == 4   ~ 4 ,
             we_empl_earn==0 | we_empl_earn==3 ~ 5,
             we_empl==0 ~ 6, 
             we_earn_wm_compare>=8 ~ 8)) %>%
  set_value_labels(we_earn_wm_compare2 = c("More than husband" = 1, "Less than husband"=2,"Same as husband"=3, 
                               "Husband has no cash earnings or not working"=4, "Woman worked but has no cash earnings"=5,
                               "Woman did not work"=6, "Don't know/missing"=8)) %>%
  set_variable_labels(we_earn_wm_compare2 = "Comparison of women's earnings to her husband")

table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(we_earn_wm_compare2, total()),
    col_vars = list(we_earn_wm_decide, we_earn_hs_decide),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Control over earnings by comparison of earnings with husband")
write.xlsx(table_temp, "Chap15_WE/Tables_emply.xls", sheetName = "control_earn_wm", append=TRUE)

# **************************************************************************************************
# * Ownership of assets
# **************************************************************************************************
# //Own a house
# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v106, v190, total()),
    col_vars = list(we_own_house, we_own_land),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Ownership of house or land")
write.xlsx(table_temp, "Chap15_WE/Tables_assets.xls", sheetName = "house_land_wm", append=TRUE)

#For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, mv025, mv024, mv106, mv190, total()),
    col_vars = list(we_own_house, we_own_land),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Ownership of house or land")
write.xlsx(table_temp, "Chap15_WE/Tables_assets.xls", sheetName = "house_land_mn", append=TRUE)

# ****************************************************
# //Title or deed ownership for house
# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v106, v190, total()),
    col_vars = list(we_house_deed, we_land_deed),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Ownership of house or land deeds")
write.xlsx(table_temp, "Chap15_WE/Tables_assets.xls", sheetName = "house_land_deeds_wm", append=TRUE)

#For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, mv025, mv024, mv106, mv190, total()),
    col_vars = list(we_house_deed, we_land_deed),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Ownership of house or land deeds")
write.xlsx(table_temp, "Chap15_WE/Tables_assets.xls", sheetName = "house_land_deeds_mn", append=TRUE)

# ****************************************************
# //Have a bank account, mobile phone, use phone for finances
# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v106, v190, total()),
    col_vars = list(we_bank, we_mobile, we_mobile_finance),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Ownership of house or land deeds")
write.xlsx(table_temp, "Chap15_WE/Tables_assets.xls", sheetName = "bank_phone_wm", append=TRUE)

# For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, mv025, mv024, mv106, mv190, total()),
    col_vars = list(we_bank, we_mobile, we_mobile_finance),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Ownership of house or land deeds")
write.xlsx(table_temp, "Chap15_WE/Tables_assets.xls", sheetName = "bank_phone_mn", append=TRUE)


# **************************************************************************************************
# * Decision making indicators
# **************************************************************************************************
# //Decision making indicators

# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(total()),
    col_vars = list(we_decide_health, we_decide_hhpurch, we_decide_visits),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Decision making women")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "decision_wm", append=TRUE)

# For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(total()),
    col_vars = list(we_decide_health, we_decide_hhpurch),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Decision making men")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "decision_mn", append=TRUE)

# 
# ****************************************************
# //Decide on own health care either alone or jointly with partner

# For women
# First need to construct variable of employment by cash earning for the table 
IRdata <- IRdata %>%
  mutate(emply =
           case_when(
             v502==1 & v731 == 0   ~ 0 ,
             v502==1 & v731>0 & v731<8 & (v741==1 | v741==2) ~ 1,
             v502==1 & v731>0 & v731<8 & (v741==0 | v741==3) ~ 2,
             v502==1 & v731 >=8 ~ 99)) %>%
  replace_with_na(replace = list(emply = c(99))) %>%
  set_value_labels(emply = c("Employed not for cash" =2, "Employed for cash" = 1, "Not employed"=0  )) %>%
  set_variable_labels(emply = "Employment in the last 12 months by cash earnings")

table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, emply, numch, v025, v024, v106, v190, total()),
    col_vars = list(we_decide_health_self, we_decide_hhpurch_self, we_decide_visits_self, we_decide_all, we_decide_none ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Decision making women by background vars")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "decision_vars_wm", append=TRUE)

# For men
# First need to construct variable of employment by cash earning for the table 
MRdata <- MRdata %>%
  mutate(emply =
           case_when(
             mv502==1 & mv731 == 0   ~ 0 ,
             mv502==1 & mv731>0 & mv731<8 & (mv741==1 | mv741==2) ~ 1,
             mv502==1 & mv731>0 & mv731<8 & (mv741==0 | mv741==3) ~ 2,
             mv502==1 & mv731 >=8 ~ 99)) %>%
  replace_with_na(replace = list(emply = c(99))) %>%
  set_value_labels(emply = c("Employed not for cash" =2, "Employed for cash" = 1, "Not employed"=0  )) %>%
  set_variable_labels(emply = "Employment in the last 12 months by cash earnings")

table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, emply, numch, mv025, mv024, mv106, mv190, total()),
    col_vars = list(we_decide_health_self, we_decide_hhpurch_self, we_decide_all, we_decide_none ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Decision making men by background vars")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "decision_vars_mn", append=TRUE)

# **************************************************************************************************
# * Justification of violence
# **************************************************************************************************
# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, emply, numch, v502, v025, v024, v106, v190, total()),
    col_vars = list(we_dvjustify_burn, we_dvjustify_argue, we_dvjustify_goout, we_dvjustify_neglect, we_dvjustify_refusesex, we_dvjustify_onereas ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Justification for violence - women")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "justify_viol_wm", append=TRUE)

# For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, emply, numch, mv502, mv025, mv024, mv106, mv190, total()),
    col_vars = list(we_dvjustify_burn, we_dvjustify_argue, we_dvjustify_goout, we_dvjustify_neglect, we_dvjustify_refusesex, we_dvjustify_onereas ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Justification for violence - men")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "justify_viol_mn", append=TRUE)


# ****************************************************
# //Justify having no sex if husband is having sex with another woman
# For women
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(v013, v502, v025, v024, v106, v190, total()),
    col_vars = list(we_justify_refusesex, we_justify_cond ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Justification for not having sex - women")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "justify_nosex_wm", append=TRUE)

# For men
table_temp <-  MRdata %>% 
  filter(mv012>14 & mv012<50) %>%
  calc_cro_rpct(
    cell_vars = list(mv013, mv502, mv025, mv024, mv106, mv190, total()),
    col_vars = list(we_justify_refusesex, we_justify_cond ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Justification for not having sex - women")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "justify_nosex_mn", append=TRUE)

# ****************************************************
# //Number of decisions by those who disagree with all reasons that justify wife beating (only first column) AND
# Number of reasons that justify wife beating by those who participate in all decision making (only last column)
table_temp <-  IRdata %>% 
  filter(v012>14 & v012<50) %>%
  calc_cro_rpct(
    cell_vars = list(we_num_justifydv, we_num_decide, total()),
    col_vars = list(we_num_justifydv, we_num_decide ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Justification of wife beating by number of decisions women can participate")
write.xlsx(table_temp, "Chap15_WE/Tables_empw.xls", sheetName = "decisions_justify_viol", append=TRUE)
# 
# /* ****************************************************
# Note:
# The women empowerment indicators we_num_decide and we_num_justifydv may also be tabulated by current contraceptive use and unment need (chapter 7), ideal number of children (chapter 6),
# and reproductive health indicators (chapter 9). Please check these chapters to create the indicators of interest that you would like to include in the tabulations. 
# *****************************************************/
# 
