# ******************************************************************************
# Program: 			  ML_tables_HR.R - No changes in DHS8 update
# Purpose: 		    produce tables for indicators
# Data outputs:		tables in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: August 02, 2024 by Courtney Allen
# ******************************************************************************
# Note this do file will produce the following tables in excel:
#1. Tables_HH_ITN:		Contains the tables for houeshold possession of ITNs 
# ******************************************************************************
# set expss package options to show one decimal place
expss_digits(digits=1)
wb = createWorkbook()

HRdata <- HRdata %>%
  mutate(wt = hv005/1000000)
    
# Household ownership of one mosquito net
table_temp <-  HRdata %>% 
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_mosquitonet),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Household ownership of one+ mosquito net")

# save to workbook
sh = addWorksheet(wb, "hh_net")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Household ownership of ITNs
table_temp <-  HRdata %>% 
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_itnhh),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Household ownership of ITNs")

# save to workbook
sh = addWorksheet(wb, "hh_ITN")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Average number of mosquito nets per household
table_temp = HRdata %>%
  tab_cells(ml_numnets) %>%
  tab_rows(hv025, hv024, hv270,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun(Mean = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Average number of mosquito nets per household")

# save to workbook
sh = addWorksheet(wb, "hh_net_average")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Average number of ITNs per household
table_temp = HRdata %>%
  tab_cells(ml_numitnhh) %>%
  tab_rows(hv025, hv024, hv270,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun(Mean = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Average number of ITNs per household")

# save to workbook
sh = addWorksheet(wb, "hh_ITN_average")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Households with at least one mosquito net for every 2 persons 
table_temp <-  HRdata %>% 
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_mosnethhaccess),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Households with at least one mosquito net for every 2 persons")

# save to workbook
sh = addWorksheet(wb, "hh_net_for2")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Households with at least one ITN for every 2 persons 
table_temp <-  HRdata %>% 
  filter(hv013>=1) %>%
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_hhaccess),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Households with at least one ITN for every 2 persons")

# save to workbook
sh = addWorksheet(wb, "hh_ITN_for2")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)
