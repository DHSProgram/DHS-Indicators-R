# ******************************************************************************
# Program: 			  FF_tables.R
# Purpose: 		    produce tables for indicators
# Data inputs: 		IR dataset
# Data outputs:		tables on screen and in excel sheets
# Author:				  Mahmoud Elkasabi
# Date last modified: September 07 2021 by Mahmoud Elkasabi
# ******************************************************************************

# Fertility preferences according to number of living children
table_temp = IRdata %>%
  calc_cro_cpct(
    cell_vars = list(ff_want_type),
    col_vars = list(numch,  total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Fertility preferences according to number of living children")

table_temp 

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_want_type",append=TRUE)

# Percentage of currently married women age 15-49 who want no more children
table_temp = IRdata %>%
  calc_cro_cpct(
    cell_vars = list(ff_want_nomore),
    row_vars = list(v025, v024, v106, v190,  total()),
    col_vars = list(numch,  total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Desire to limit childbearing: Women")

table_temp 

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_want_nomore",append=TRUE)

# Ideal number of children according to number of living children
table_temp = IRdata %>%
  calc_cro_cpct(
    cell_vars = list(ff_ideal_num),
    col_vars = list(numch,  total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Ideal number of children according to number of living children")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_num",append=TRUE)

table_temp = IRdata %>%
  filter(v613<95) %>% 
  tab_cells(v613) %>%
  tab_rows(numch,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children:" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children for women 15-49")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean",append=TRUE)

table_temp = IRdata %>%
  filter(v613<95 & v502==1) %>% 
  tab_cells(v613) %>%
  tab_rows(numch,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children: Currently married women" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children for married women 15-49")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean_married",append=TRUE)

table_temp = IRdata %>%
  filter(v613<95) %>% 
  tab_cells(v613) %>%
  tab_rows(v013, v025, v024, v106, v190, total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children according to background characteristics")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean_background",append=TRUE)
