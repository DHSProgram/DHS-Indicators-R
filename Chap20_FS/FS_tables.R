# /*****************************************************************************************************
# Program: 			FS_tables.R
# Purpose: 			produce tables for indicators
# Author:				Shireen Assaf
# Date last modified: September 28, 2022 by Shireen Assaf 
# 
# This file will produce the following table in excel:
# Tables_FIST: Contains the tables for fistula indicators

# *****************************************************************************************************/
# //Heard of fistula and ever experienced fistula
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list( v013, v025, v024, v106, v190, total()),
    col_vars = list(fs_heard, fs_ever_exp),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Knowledge and ever experienced fistula")
write.xlsx(table_temp, here(chap,"Tables_FIST.xls"), sheetName = "Know_Prev", append=TRUE)


# *************************************************************************************************
# //Reported cause of fistula and number of days since symptoms began
 
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list(total()),
    col_vars = list(fs_cause, fs_days_symp),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Cause of fistula and number of days since symptoms began")
write.xlsx(table_temp, here(chap,"Tables_FIST.xls"), sheetName = "cause_days", append=TRUE)

# *************************************************************************************************

# //Provider type for fistula treatment, outcome of treatment, and whether women had an operation
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list(v025, v024, v106, v190, total()),
    col_vars = list(fs_trt_provid, fs_trt_outcome, fs_trt_operat),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Provider type for treatment, outcome of treatment, and operation")
write.xlsx(table_temp, here(chap,"Tables_FIST.xls"), sheetName = "provider_treat", append=TRUE)

# *************************************************************************************************
# //Reason for not seeking treatment

table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list(total()),
    col_vars = list(fs_notrt_reason),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Reason for not seeking treatment")
write.xlsx(table_temp, here(chap,"Tables_FIST.xls"), sheetName = "reason_notreat", append=TRUE)
