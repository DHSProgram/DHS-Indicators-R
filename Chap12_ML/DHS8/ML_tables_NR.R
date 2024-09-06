# ******************************************************************************
# Program: 			  ML_tables_IR.R - No changes in DHS8
# Purpose: 		    produce tables for indicators
# Data outputs:		tables on screen and in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: August 02, 2024 by Courtney Allen
# ******************************************************************************
# Note this do file will produce the following tables in excel:
# 5. 	Tables_IPTP:		Contains tables on IPTp uptake
# ******************************************************************************

NRdata <- NRdata %>%
  mutate(wt = v005/1000000)

#  Percentage who received one or more doses of SP/Fansidar
table_temp <-  NRdata %>% 
cross_rpct(
  subgroup = m80==1, #only for livebirths
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(ml_one_iptp, ml_two_iptp, ml_three_iptp,
 total()),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Percentage who received one or more doses of SP/Fansidar")

# save to workbook
sh = addWorksheet(wb, "iptp_live")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

#  Percentage who received doses of SP/Fansidar by type of live/stillbirth
table_temp <-  NRdata %>% 
  cross_rpct(
    cell_vars = list(m80, total()),
    col_vars = list(ml_one_iptp, ml_two_iptp, ml_three_iptp, total()),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Percentage who received one or more doses of SP/Fansidar by birth type")

# save to workbook
sh = addWorksheet(wb, "iptp_all")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)
