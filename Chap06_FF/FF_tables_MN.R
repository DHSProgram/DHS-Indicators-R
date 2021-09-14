# ******************************************************************************
# Program: 			  FF_tables.R
# Purpose: 		    produce tables for indicators
# Data inputs: 		IR dataset
# Data outputs:		tables on screen and in excel sheets
# Author:				  Mahmoud Elkasabi
# Date last modified: September 07 2021 by Mahmoud Elkasabi
# ******************************************************************************

# Fertility preferences according to number of living children
table_temp = MRdata %>%
  filter(mv012 <= 49) %>%
  calc_cro_cpct(
    cell_vars = list(ff_want_type),
    col_vars = list(numch,  total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Fertility preferences according to number of living children")

table_temp 

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_want_type_MN",append=TRUE)

# Men 15+
table_temp = MRdata %>%
  calc_cro_cpct(
    cell_vars = list(ff_want_type),
    col_vars = list(total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Fertility preferences according to number of living children")

table_temp 

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_want_type_MN_15+",append=TRUE)


# Percentage of currently married men who want no more children
table_temp = MRdata %>%
  filter(mv012 <= 49) %>%
  calc_cro_cpct(
    cell_vars = list(ff_want_nomore),
    row_vars = list(mv025, mv024, mv106, mv190,  total()),
    col_vars = list(numch,  total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Desire to limit childbearing: men")

table_temp 

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_want_nomore_MN",append=TRUE)

# Ideal number of children according to number of living children
table_temp = MRdata %>%
  filter(mv012 <= 49) %>%
  calc_cro_cpct(
    cell_vars = list(ff_ideal_num),
    col_vars = list(numch,  total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Ideal number of children according to number of living children")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_num_MN",append=TRUE)

table_temp = MRdata %>%
  filter(mv012 <= 49) %>%
  filter(mv613<95) %>% 
  tab_cells(mv613) %>%
  tab_rows(numch,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children: men" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children for men")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean__MN",append=TRUE)

# 15+
table_temp = MRdata %>%
  filter(mv613<95) %>% 
  tab_cells(mv613) %>%
  tab_rows(numch,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children: men" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children for men")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean_MN_15+",append=TRUE)


table_temp = MRdata %>%
  filter(mv012 <= 49) %>%
  filter(mv613<95 & mv502==1) %>% 
  tab_cells(mv613) %>%
  tab_rows(numch,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children: Currently married men" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children for married men")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean_married_MN",append=TRUE)

# 15+
table_temp = MRdata %>%
  filter(mv613<95 & mv502==1) %>% 
  tab_cells(mv613) %>%
  tab_rows(numch,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children: Currently married men" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children for married men")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean_married_MN_15+",append=TRUE)


table_temp = MRdata %>%
  filter(mv012 <= 49) %>%
  filter(mv613<95) %>% 
  tab_cells(mv613) %>%
  tab_rows(mv013, mv025, mv024, mv106, mv190, total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("Mean ideal number of children" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean ideal number of children according to background characteristics")

table_temp

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_ideal_mean_background_MN",append=TRUE)



