# /*****************************************************************************
# Program: 			HK_tables_WM.R - DHS8 update                                         
# Purpose: 			produce tables for HIV knowledge, attitudes, and behavior indicators
# Author:				Courtney Allen
# Date last modified: September 4 2024 by Courtney Allen
#
# Note this do file will produce the following tables in excel:
# 1. 	Tables_HK_wm:		Contains the tables for HIV knowledge, attitudes, and behavior indicators for women
#
# NOTES ------------------------------------------------------------------------
#  
# the total will show on the last row of each table.
# comment out the tables or indicator section you do not want
# to add additional crosstab variables, modify the line that begins with "cell_vars = list(v013...etc)"
# ******************************************************************************

# set expss package options to show one decimal place
expss_digits(digits=1)

## create workbook
wb = createWorkbook()


## create weight
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)
## create age groups for young people for specific tables
IRdata_young <- IRdata %>% filter(v013 %in% 1:2)

# TABLES FOR WOMEN -------------------------------------------------------------

# TABLES FOR HIV KNOWLEDGE INDICATORS ---------

## create table of ever hear of HIV ---------------------------------------------
# NOTE: The standard Woman's and Man's QREs include an optional question (WQ1001 and MQ701): Have you ever heard of HIV or AIDS? If the survey did NOT include these questions, then this table will not be in the final report. 
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_ever_heard,   total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Ever heard of HIV")

# save to workbook
sh = addWorksheet(wb, "Heard HIV")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of knowledge of medicines to treat HIV-------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v502, v025, v106),
    col_vars = list(hk_knw_arv, hk_knw_mtct_meds, hk_knw_PrEP, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Knowledge of medicines to treat HIV")

# save to workbook
sh = addWorksheet(wb, "Heard medicine")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of attitudes toward medicines to treat HIV-------------
table_temp <- IRdata %>% 
  cross_rpct(
    subgroup = hk_knw_PrEP==1,
    cell_vars = list(v013, v502, v025, v106),
    col_vars = list(hk_aprov_PrEP, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women heard of PrEP"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Attitudes towards medicines to treat HIV")

# save to workbook
sh = addWorksheet(wb, "Attitude medicine")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of discriminatory attitudes toward people living with HIV--------
# NOTE: The standard Woman's and Man's QREs include an optional question (WQ1001 and MQ701): Have you ever heard of HIV or AIDS? The denominator in this table is women and men who have heard of HIV or AIDS. If the survey did NOT include these questions, then this table should include everyone. Both versions are created below

table_temp <- IRdata %>% 
  cross_rpct(
    subgroup = hk_ever_heard==1,
    cell_vars = list(v013, v502, v025, v106, v190),
    col_vars = list(hk_atd_child_nosch, hk_atd_shop_notbuy, hk_atd_discriminat, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Discriminatory attitudes toward people living with HIV among women who have heard of HIV")

# save to workbook
sh = addWorksheet(wb, "Discrim know HIV")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v502, v025, v106, v190),
    col_vars = list(hk_atd_child_nosch, hk_atd_shop_notbuy, hk_atd_discriminat, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Discriminatory attitudes toward people living with HIV")

# save to workbook
sh = addWorksheet(wb, "Discrim")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

# TABLES FOR MULTIPLE SEX PARTNERS AND CONDOM USE INDICATORS ---------

## create table of mult sex partners and condom use------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_sex_2plus, hk_sex_notprtnr, hk_cond_2plus, hk_cond_notprtnr, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Mult sex partners and higer risk sexual intercourse")

# save to workbook
sh = addWorksheet(wb, "HIV risk")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)


## create worksheet for mean sex partners by age---------------------------------
sh = addWorksheet(wb, "Mean prtnr age")
writeData(wb, sheet = "Mean prtnr age", x = sexprtnr_mean_age, colNames = TRUE)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create worksheet for mean sex partners by marital status
sh = addWorksheet(wb, "Mean prtnr mar")
writeData(wb, sheet = "Mean prtnr mar", x = sexprtnr_mean_mar, colNames = TRUE)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create worksheet for mean sex partners by residence
sh = addWorksheet(wb, "Mean prtnr res")
writeData(wb, sheet = "Mean prtnr res", x = sexprtnr_mean_res, colNames = TRUE)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create worksheet for mean sex partners by education
sh = addWorksheet(wb, "Mean prtnr edu")
writeData(wb, sheet = "Mean prtnr edu", x = sexprtnr_mean_edu, colNames = TRUE)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create worksheet for mean sex partners by wealth
sh = addWorksheet(wb, "Mean prtnr wealth")
writeData(wb, sheet = "Mean prtnr wealth", x = sexprtnr_mean_wealth, colNames = TRUE)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)



# TABLES FOR HIV TESTING INDICATORS ---------

## create table of pregnant women tested for HIV---------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v502, v025, v106, v190),
    col_vars = list(hk_test_anc_result, hk_test_anc_noresult, hk_test_anclbr_result, hk_test_anclbr_noresult, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Pregnant women tested for HIV")

# save to workbook
sh = addWorksheet(wb, "Pregnant test")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)


## create table of coverage of prior HIV testing---------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v502, v025, v106, v190),
    col_vars = list(hk_test_prior, hk_test_ever, hk_test_12m, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Prior HIV testing")

# save to workbook
sh = addWorksheet(wb, "HIV test")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of number of times tested for HIV in lifetime--------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013),
    col_vars = list(hk_test_life, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Number of times tested for HIV in lifetime")

# save to workbook
sh = addWorksheet(wb, "HIV test life")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of knowledge of self-testing for HIV-----------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_hiv_selftest_heard, hk_hiv_selftest_use, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Knowledge and coverage of self-testing for HIV")

# save to workbook
sh = addWorksheet(wb, "Self test")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)



## TABLES FOR DISCLOSURE AND STIGMA INDICATORS ---------

## create table of disclosure and stigma for HIV-----------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_disclos_hiv, hk_asham_hiv, hk_tlkbad_hiv, hk_othr_disclos_hiv, hk_harass_hiv, hk_stigma_hiv, hk_hlthwrk_tlkbad_hiv, hk_hlthwrk_vrbabuse_hiv, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Disclosure, shame, and stigma experienced by people living with HIV")

# save to workbook
sh = addWorksheet(wb, "Stigma")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)



# TABLES FOR STI INDICATORS ----------------

## create table of self reported STIs--------------------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_sti, hk_gent_disch, hk_gent_sore, hk_sti_symp, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Self reported STIs")

# save to workbook
sh = addWorksheet(wb, "STIs")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)



# TABLE FOR KNOWLEDGE AMONG YOUNG PEOPLE INDICATORS ----------

## create table of HIV knowledge of prevention methods---------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    subgroup = v012<25,
    cell_vars = list(v013, v502, v025, v106, v190),
    col_vars = list(hk_knw_risk_cond, hk_knw_risk_sex, hk_knw_hiv_hlth, hk_knw_hiv_mosq, hk_knw_hiv_food,  hk_knw_all, total()),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Knowledge of HIV prevention methods among young people")

# save to workbook
sh = addWorksheet(wb, "Young HIV comp")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)



# TABLES FOR SEXUAL ACTIVITY AND HIV TESTING AMONG YOUNG PEOPLE INDICATORS ----------

## create table of age at first sex among young people --------------------------
  table_temp <- IRdata %>% 
  cross_rpct(
    subgroup = v012<25,
    cell_vars = list(v013, v025, v106),
    col_vars = list(hk_sex_15, hk_sex_18, total()),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Age at first sex among young people")

# save to workbook
sh = addWorksheet(wb, "Young sex age")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of premarital among young people --------------------------------
  table_temp <- IRdata %>% 
  cross_rpct(
    subgroup = v012<25,
    cell_vars = list(v013, v025, v106, total()),
    col_vars = list(hk_nosex_youth),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Premarital among young people")

# save to workbook
sh = addWorksheet(wb, "Young premarital")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of mult sex partners and condom use among young people ----------
table_temp <- IRdata_young %>% 
  cross_rpct(
    subgroup = v012<25,
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_sex_2plus, hk_sex_notprtnr, hk_cond_2plus, hk_cond_notprtnr, total()),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Mult sex partners and higer risk sexual intercourse")

# save to workbook
sh = addWorksheet(wb, "Young HIV risk")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)

## create table of recent HIV tests among young people --------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v502),
    col_vars = list(hk_sex_youth_test, total()),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Recent HIV tests among young people")

# save to workbook
sh = addWorksheet(wb, "Young HIV test")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_HK_wm.xlsx"), overwrite = TRUE)
