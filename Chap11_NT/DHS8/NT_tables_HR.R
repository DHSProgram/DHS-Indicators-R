# /*****************************************************************************************************
# Program: 			NT_tables_HR.R
# Purpose: 			produce tables for indicators from HR file
# Author:				Shireen Assaf
# Date last modified: December 6, 2021 by Shireen Assaf
# 
# *This do file will produce the following tables in excel:
# Tables_salt_hh:		Contains the tables for salt testing and iodized salt in households

# *****************************************************************************************************/

# indicators from HR file
HRdata <- HRdata %>%
  mutate(wt = hv005/1000000)

table_temp <-  HRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(nt_salt_any, nt_salt_iod),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Salt in household")
write.xlsx(table_temp, "Chap11_NT/Tables_salt_hh.xls", sheetName = "salt_hh", append=TRUE)

