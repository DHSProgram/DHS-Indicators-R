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

# Household characteristics ----------------------------------------------------
table_temp <-  HRdata %>% 
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_electric, ph_floor, ph_rooms_sleep, ph_cook_place, ph_cook_fuel,	ph_cook_solid, 
                    ph_cook_clean, ph_smoke),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household characteristics")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_characteristics", append=TRUE)

# Household possessions --------------------------------------------------
table_temp <-  HRdata %>% 
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_radio, ph_tv, ph_mobile, ph_tel, ph_comp, ph_frig, ph_bike, ph_cart,	ph_moto, ph_car, 
                    ph_boat, ph_agriland, ph_animals),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household possessions")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_possessions", append=TRUE)


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


# Household population ---------------------------------------------------------
table_temp <-  PRdata %>% 
  cross_cpct(
    col_vars = list(hv025, hv104, total()),
    cell_vars = list(ph_pop_age, ph_pop_depend, ph_pop_cld_adlt, ph_pop_adols),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household population")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_pop", append=TRUE)


# Household composition-----------------------------------------------------------
table_temp <-  temp_children %>% 
  filter(hv102==1) %>%
  cross_cpct(
    col_vars = list(hv025, total()),
    cell_vars = list(ph_hhhead_sex, ph_num_members, ph_orph_double, ph_orph_single, ph_orph_foster),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household composition")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "hh_comp", append=TRUE)



# Child's living arrangements----------------------------------------------------------
table_temp <-  PR_temp_children %>% 
  filter(hv102==1) %>%
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ph_chld_liv_arrang, ph_chld_liv_noprnt, ph_chld_orph),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Child's living arrangments")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "chld_liv", append=TRUE)


# Birth registration----------------------------------------------------------
table_temp <-  PRdata %>% 
  filter(hv102==1) %>%
  cross_rpct(
    cell_vars = list(hv104, hv025, hv024, hv270, total()),
    col_vars = list(ph_birthreg, ph_birthreg_nocert, ph_birthreg_cert),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Birth registration of children under 5")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "birth_reg", append=TRUE)


# Educational attainment of female pop----------------------------------------------------------
table_temp <-  PRdata %>% 
  filter(hv104==2) %>%
  cross_rpct(
    cell_vars = list(ph_pop_age, hv025, hv024, hv270, total()),
    col_vars = list(ph_highest_edu),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Educational attainment of female population")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "edu_attainment_wm", append=TRUE)


# Educational attainment of male pop----------------------------------------------------------
table_temp <-  PRdata %>% 
  filter(hv104==1) %>%
  cross_rpct(
    cell_vars = list(ph_pop_age, hv025, hv024, hv270, total()),
    col_vars = list(ph_highest_edu),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Educational attainment of male population")
write.xlsx(table_temp, "Chap02_PH/Tables_PH.xls", sheetName = "edu_attainment_mn", append=TRUE)


# School attendance ratios----------------------------------------------------------
write.xlsx(ph_sch_nar_prim, "Chap02_PH/Tables_PH.xls", sheetName = "schol_nar_prim",append=TRUE)
write.xlsx(ph_sch_nar_sec, "Chap02_PH/Tables_PH.xls", sheetName = "schol_nar_sec",append=TRUE)
write.xlsx(ph_sch_gar_prim, "Chap02_PH/Tables_PH.xls", sheetName = "schol_gar_prim",append=TRUE)
write.xlsx(ph_sch_gar_sec, "Chap02_PH/Tables_PH.xls", sheetName = "schol_gar_sec",append=TRUE)

