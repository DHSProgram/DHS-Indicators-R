# /*****************************************************************************************************
# Program: 			HK_tables                                                                     _WM.R
# Purpose: 			produce tables for HIV knowledge, attitudes, and behavior indicators
# Author:				Courtney Allen
# Date last modified: August 2022 by Courtney Allen
#
# Note this do file will produce the following tables in excel:
# 1. 	Tables_HK_wm:		Contains the tables for HIV knowledge, attitudes, and behavior indicators for women
#
# Notes: 
#  
# the total will show on the last row of each table.
# comment out the tables or indicator section you do not want
# to add additional crosstab variables, modify the line that begins with "cell_vars = list(v013...etc)"
# ******************************************************************************

# to remove workbook from global environment to start this code over:
# rm(list=deparse(substitute(wb)),envir=.GlobalEnv)

# set expss package options to show one decimal place
expss_digits(digits=1)

# create workbook
wb = createWorkbook()


# TABLES FOR WOMEN -------------------------------------------------------------

# create weight
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# create age groups for young people for specific tables
IRdata_young <- IRdata %>% filter(v013 %in% 1:2)

# create table of ever hear of HIV ---------------------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_ever_heard, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Ever heard of HIV")

# save to workbook
sh = addWorksheet(wb, "Heard HIV")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of HIV knowledge of prevention methods---------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_knw_risk_cond, hk_knw_risk_sex, hk_knw_risk_condsex, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Knowledge of HIV prevention methods")

# save to workbook
sh = addWorksheet(wb, "HIV know prevent")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of comprehensive HIV knowledge ----------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013),
    col_vars = list(hk_knw_hiv_hlth, hk_knw_hiv_mosq, hk_knw_hiv_supernat, hk_knw_hiv_food, hk_knw_hiv_hlth_2miscp, hk_knw_comphsv, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Comprehensive HIV knowledge")

# save to workbook
sh = addWorksheet(wb, "HIV know comp")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of knowledge of prevention of mother to child transmission of HIV----
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013),
    col_vars = list(hk_knw_mtct_preg, hk_knw_mtct_deliv, hk_knw_mtct_brfeed, hk_knw_mtct_all3, hk_knw_mtct_meds, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Knowledge of prevention of mother-to-child transmission of HIV")

# save to workbook
sh = addWorksheet(wb, "MTCT")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of discriminatory attitudes toward people living with HIV--------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
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
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of mult sex partners and condom use------------------------------
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
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create worksheet for mean sex partners by age---------------------------------
sh = addWorksheet(wb, "Mean prtnr age")
writeData(wb, sheet = "Mean prtnr age", x = sexprtnr_mean_age, colNames = TRUE)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)

# create worksheet for mean sex partners by marital status
sh = addWorksheet(wb, "Mean prtnr mar")
writeData(wb, sheet = "Mean prtnr mar", x = sexprtnr_mean_mar, colNames = TRUE)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)

# create worksheet for mean sex partners by residence
sh = addWorksheet(wb, "Mean prtnr res")
writeData(wb, sheet = "Mean prtnr res", x = sexprtnr_mean_res, colNames = TRUE)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)

# create worksheet for mean sex partners by education
sh = addWorksheet(wb, "Mean prtnr edu")
writeData(wb, sheet = "Mean prtnr edu", x = sexprtnr_mean_edu, colNames = TRUE)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)

# create worksheet for mean sex partners by wealth
sh = addWorksheet(wb, "Mean prtnr wealth")
writeData(wb, sheet = "Mean prtnr wealth", x = sexprtnr_mean_wealth, colNames = TRUE)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of prior HIV testing---------------------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_test_where,hk_test_prior, hk_test_ever, hk_test_12m, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Prior HIV testing")

# save to workbook
sh = addWorksheet(wb, "HIV test")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of HIV counseling------------------------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_hiv_consl_anc,hk_test_consl_anc, hk_test_noconsl_anc, 
                    hk_test_noresult_anc, hk_hiv_receivedall_anc, hk_test_anclbr_result,
                    hk_test_anclbr_noresult, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Coverage of HIV counseling")

# save to workbook
sh = addWorksheet(wb, "HIV counsel")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)



# create table of knowledge of self-testing for HIV-----------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106, v190),
    col_vars = list(hk_hiv_selftest_heard, hk_hiv_selftest_use, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Self-testing for HIV")

# save to workbook
sh = addWorksheet(wb, "Self test")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)




# create table of self reported STIs--------------------------------------------
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
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)



# create table of STI treatment-------------------------------------------------
table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013),
    col_vars = list(hk_sti_trt_doc, hk_sti_trt_pharm, hk_sti_trt_other,
                    hk_sti_notrt, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Treatment for STIs")

# save to workbook--------------------------------------------------------------
sh = addWorksheet(wb, "STIs treat")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of comprehensive knowledge of HIV among young people ------------
table_temp <- IRdata_young %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106),
    col_vars = list(hk_knw_comphsv, total()),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Comprehensive HIV knowledge among young people")

# save to workbook
sh = addWorksheet(wb, "Young HIV comp")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of age at first sex among young people --------------------------
  table_temp <- IRdata %>% 
  cross_rpct(
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
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of premarital among young people --------------------------------
  table_temp <- IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v106),
    col_vars = list(hk_nosex_youth, total()),
    weight = wt, 
    total_label = c("Total 15-24", "Number of women"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Premarital among young people")

# save to workbook
sh = addWorksheet(wb, "Young premarital")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)



# create table of mult sex partners and condom use among young people ----------
table_temp <- IRdata_young %>% 
  cross_rpct(
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
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)


# create table of recent HIV tests among young people --------------------------
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
saveWorkbook(wb, "Chap13_HK/Tables_HK_wm.xlsx", overwrite = TRUE)
