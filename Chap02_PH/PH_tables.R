# /*****************************************************************************************************
# Program: 			PH_tables.do
# Purpose: 			produce tables for household characteristics , WASH, and hand-washing indicators
# Author:				Shireen Assaf
# Date last modified: Feb 4, 2022 by Shireen Assaf
# 
# *This do file will produce the following tables in excel:
# Tables_HH:		Contains the tables for household characteristic, household possessions, hand-washing indicators, and WASH
# 
# Notes: 			[WASH to be added soon]		 						
# *****************************************************************************************************/
# 
# indicators from HR file
HRdata <- HRdata %>%
  mutate(wt = hv005/1000000)

# **************************************************************************************************
# Household characteristics
# **************************************************************************************************
table_temp <-  HRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, total()),
    col_vars = list(ph_electric, ph_floor, ph_rooms_sleep, ph_cook_place, ph_cook_fuel,	ph_cook_solid, 
                    ph_cook_clean, ph_smoke),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household characteristics")
write.xlsx(table_temp, "Chap02_PH/Tables_HH.xls", sheetName = "HHcharacteristics", append=TRUE)

# **************************************************************************************************
# Household possessions
# **************************************************************************************************
table_temp <-  HRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, total()),
    col_vars = list(ph_radio, ph_tv, ph_mobile, ph_tel, ph_comp, ph_frig, ph_bike, ph_cart,	ph_moto, ph_car, 
                    ph_boat, ph_agriland, ph_animals),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household possessions")
write.xlsx(table_temp, "Chap02_PH/Tables_HH.xls", sheetName = "HHpossessions", append=TRUE)


 
# **************************************************************************************************
# **************************************************************************************************

# indicators from HR file
PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)
# 
# **************************************************************************************************
# Hand-washing
# **************************************************************************************************
table_temp <-  PRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ph_hndwsh_place_fxd, ph_hndwsh_place_mob, ph_hndwsh_place_any, ph_hndwsh_water, 
                    ph_hndwsh_soap, ph_hndwsh_clnsagnt, ph_hndwsh_basic, ph_hndwsh_limited ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Household handwashing")
write.xlsx(table_temp, "Chap02_PH/Tables_HH.xls", sheetName = "HHhandwash", append=TRUE)

# **************************************************************************************************

