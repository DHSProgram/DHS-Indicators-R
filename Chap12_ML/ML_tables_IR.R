# ******************************************************************************
# Program: 			  ML_tables_IR.R
# Purpose: 		    produce tables for indicators
# Data outputs:		tables on screen and in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: January 12, 2022 by Mahmoud Elkasabi
# ******************************************************************************
# Note this do file will produce the following tables in excel:
# 5. 	Tables_IPTP:		Contains tables on IPTp uptake
# ******************************************************************************

IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

#  Percentage who received one or more doses of SP/Fansidar
table_temp <-  IRdata %>% 
calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(ml_one_iptp),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Percentage who received one or more doses of SP/Fansidar")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "ir_sp1",append=TRUE)

#  Percentage who received two or more doses of SP/Fansidar
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(ml_two_iptp),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Percentage who received two or more doses of SP/Fansidar")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "ir_sp2",append=TRUE)

#  Percentage who received three or more doses of SP/Fansidar
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(ml_three_iptp),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Percentage who received three or more doses of SP/Fansidar")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "ir_sp3",append=TRUE)
