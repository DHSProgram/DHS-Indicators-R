# ************************************************************************
# Program: 			PH_tables.do
# Purpose: 			produce tables for household characteristics , WASH, and hand-washing indicators
# Author:				Shireen Assaf
# Date last modified: August 4, 2023 by Courtney Allen
 
# This do file will produce the following tables in excel:
#   Tables_HH:		Contains the tables for household characteristic, household possessions, hand-washing indicators, and WASH
 
# Notes: 			[WASH to be added soon]		 						
# ************************************************************************

# indicators from HR file ------------------------------------------------
HRdata <- HRdata %>%
  mutate(wt = hv005/1000000)

HRWASHdata <- HRWASHdata %>% 
  mutate(wt = hv005/1000000)

# Household drinking water -----------------------------------------------
table_temp <-  HRWASHdata %>% 
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_wtr_source, ph_wtr_improve, ph_wtr_time, ph_wtr_basic),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household drinking water")
table_temp
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_wtr_source", append=TRUE)

# Treatment of household drinking water ----------------------------------------
table_temp <-  HRWASHdata %>% 
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_wtr_trt_boil, ph_wtr_trt_chlor, ph_wtr_trt_cloth, ph_wtr_trt_filt, ph_wtr_trt_solar, ph_wtr_trt_stand, ph_wtr_trt_other, ph_wtr_trt_none, ph_wtr_trt_appr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Treatment of household drinking water")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_wtr_treat", append=TRUE)

# Household sanitation facilities ----------------------------------------------
table_temp <-  HRWASHdata %>% 
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_sani_type, ph_sani_improve, ph_sani_location, ph_sani_basic),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household sanitation facilities")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_sani", append=TRUE)

# ******************************************************************************


# indicators from PR file ---------------

# create weights
PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)

PRWASHdata <- PRWASHdata %>%
  mutate(wt = hv005/1000000)

# most tables below select for the de jure population (hv102==1) before creating the table

# Population source of drinking water -----------------------------------------------
table_temp <-  PRWASHdata %>% 
  filter(hv102==1) %>%
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_wtr_source, ph_wtr_improve, ph_wtr_time, ph_wtr_basic),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Population drinking water source")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "pop_wtr_source", append=TRUE)

# Population using treatment of household drinking water -----------------------
table_temp <-  PRWASHdata %>% 
  filter(hv102==1) %>%
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_wtr_trt_boil, ph_wtr_trt_chlor, ph_wtr_trt_cloth, ph_wtr_trt_filt, ph_wtr_trt_solar, ph_wtr_trt_stand, ph_wtr_trt_other, ph_wtr_trt_none, ph_wtr_trt_appr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Treatment of household drinking water")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "pop_wtr_treat", append=TRUE)

# Population by type of sanitation facilities ----------------------------------------------
table_temp <-  PRWASHdata %>% 
  filter(hv102==1) %>%
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_sani_type, ph_sani_improve, ph_sani_location, ph_sani_basic),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Population by type of sanitation facilities")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "pop_sani", append=TRUE)

# Hand-washing -----------------------------------------------------------------
table_temp <-  PRdata %>% 
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ph_hndwsh_place_fxd, ph_hndwsh_place_mob, ph_hndwsh_place_any, ph_hndwsh_water, 
                    ph_hndwsh_soap, ph_hndwsh_clnsagnt, ph_hndwsh_basic, ph_hndwsh_limited ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household handwashing")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_handwash", append=TRUE)

