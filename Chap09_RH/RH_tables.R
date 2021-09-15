# /*****************************************************************************************************
# Program: 			RH_tables.R
# Purpose: 			produce tables for indicators
# Author:				Shireen Assaf
# Date last modified: September 14 2021 by Shireen Assaf 
# 
# *This do file will produce the following tables in excel:
# 1. 	Tables_ANC:	  Contains the tables for ANC provider, number of visits, timing of ANC visit and ANC components by background variables
# 2.	Tables_Probs: Contains the tables for problems accessing health care
# 3.	Tables_PNC:   Contains the tables for the PNC indicators for women and newborns
# 4.	Tables_Deliv: Contains the tables for the delivery indicators
# 	
# Notes: 	The indicators are outputted for women age 15-49 in line 27. This can be commented out if the indicators are required for all women.	
# *****************************************************************************************************/
# 
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# set expss package options to show one decimal place
expss_digits(digits=1)

# ****************************************************
# * indicators from IR file
# if file=="IR" {
# * limiting to women age 15-49
# drop if v012<15 | v012>49

# *********************************************************************************
# * ANC Indicators: excel file Tables_ANC will be produced
# *********************************************************************************

# //Person providing assistance during ANC and skilled assistance during ANC
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_anc_pv,rh_anc_pvskill),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Person providing assistance during ANC")

write.xlsx(table_temp, "Chap09_RH/Tables_ANC.xlsx", sheetName = "Provider", append=TRUE)

# ****************************************************
# // Number of ANC visits in categories and number of months pregnant at first ANC
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, total()),
    col_vars = list(rh_anc_numvs,rh_anc_moprg),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Number and timing of ANC visits")

write.xlsx(table_temp, "Chap09_RH/Tables_ANC.xlsx", sheetName = "Number and timing", append=TRUE)

library(pollster)
median <-topline(df=IRdata,variable=rh_anc_median,weight=wt)

write.xlsx(median, "Chap09_RH/Tables_ANC.xlsx", sheetName = "Median months preg at 1st ANC", append=TRUE)


# * Indicators involving ANC components 
# **************************************
# //took iron or parasite drugs during pregnancy
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_anc_iron,rh_anc_parast),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("ANC components - took iron or parasite drugs during pregnancy")

write.xlsx(table_temp, "Chap09_RH/Tables_ANC.xlsx", sheetName = "Took iron or parasite drugs", append=TRUE)


# ****************************************************
# //told of pregnancy complications, blood pressure, urine sample, blood sample taken during ANC visit
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_anc_prgcomp,rh_anc_bldpres, rh_anc_urine, rh_anc_bldsamp),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("ANC components during visit")

write.xlsx(table_temp, "Chap09_RH/Tables_ANC.xlsx", sheetName = "ANC components during visit", append=TRUE)

# */
# ****************************************************

# ****************************************************
# //had 2+ tetanus injections and neonatal tetanus
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_anc_toxinj,rh_anc_neotet),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Tetanus toxoid injections")

write.xlsx(table_temp, "Chap09_RH/Tables_ANC.xlsx", sheetName = "Tetanus toxoid", append=TRUE)

# *************************************************************************************
# * Indicators for PNC indicators: excel file Tables_PNC will be produced
# *************************************************************************************
# //PNC timing and PNC within 2 days for mother	
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_pnc_wm_timing,rh_pnc_wm_2days),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("PNC timing for mother")

write.xlsx(table_temp, "Chap09_RH/Tables_PNC.xlsx", sheetName = "Timing mother", append=TRUE)

# ****************************************************
# //PNC provider for mother
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_pnc_wm_pv),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("PNC provider")

write.xlsx(table_temp, "Chap09_RH/Tables_PNC.xlsx", sheetName = "Provider mother", append=TRUE)

# ****************************************************
# //PNC timing and PNC within 2 days for newborn
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_pnc_nb_timing,rh_pnc_nb_2days),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("PNC timing for mother")

write.xlsx(table_temp, "Chap09_RH/Tables_PNC.xlsx", sheetName = "Timing newborn", append=TRUE)

# ****************************************************
# //PNC provider for newborn
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_pnc_nb_pv),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("PNC provider")

write.xlsx(table_temp, "Chap09_RH/Tables_PNC.xlsx", sheetName = "Provider newborn", append=TRUE)

# ********************************************************************************************
# * Indicators for problems accessing health care: excel file Tables_Probs will be produced
# ********************************************************************************************
# //each problem and minimum one problem
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_prob_permit,rh_prob_money,rh_prob_dist,rh_prob_alone,rh_prob_minone),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Problems accessing health care")

write.xlsx(table_temp, "Chap09_RH/Tables_Probs.xlsx")

# ****************************************************************************
# ****************************************************************************
# 
# Indicators from BR file: will produce the Tables_Deliv excel file for the delivery indicators

BRdata <- BRdata %>%
  mutate(wt = v005/1000000)

# ****************************************************
# //place of delivery
table_temp <-  BRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = rh_del_pltype,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Place of delivery")

write.xlsx(table_temp, "Chap09_RH/Tables_Deliv.xlsx", sheetName = "Place of delivery", append=TRUE)

# ****************************************************
# //type of provider and skilled provider
table_temp <-  BRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_del_pv,rh_del_pvskill),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Provider")

write.xlsx(table_temp, "Chap09_RH/Tables_Deliv.xlsx", sheetName = "Provider", append=TRUE)

# ****************************************************
# //C-section delivery and timing for C-section
table_temp <-  BRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(rh_del_ces,rh_del_cestime),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("C-section")

write.xlsx(table_temp, "Chap09_RH/Tables_Deliv.xlsx", sheetName = "C-section", append=TRUE)

# ****************************************************
# //duration of stay after delivery
table_temp <-  BRdata %>% 
  calc_cro_rpct(
    cell_vars = list(rh_del_ces, total()),
    col_vars = rh_del_stay,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Duration of stay")

write.xlsx(table_temp, "Chap09_RH/Tables_Deliv.xlsx", sheetName = "Duration of stay", append=TRUE)
# ****************************************************
# 
