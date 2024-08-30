# /*****************************************************************************************************
# Program: 			MS_tables_MN.R - DHS8 update
# Purpose: 			produce tables for indicators
# Author:				Courtney Allen
# Date last modified: August 20 2024 by Courtney Allen
#
# Note this do file will produce the following tables in excel:
# 1. 	Tables_MS_mn:		Contains the tables for martial and sexual activity indicators for men
#
# Notes: 
#  
# the total will show on the last row of each table.
# comment out the tables or indicator section you do not want
# to add additional crosstab variables, modify the line that begins with "cell_vars = list(v013...etc)"
# ******************************************************************************



# set expss package options to show one decimal place
expss_digits(digits=1)

wb = createWorkbook()



# TABLES FOR MEN ---------------------------------------------------------------

# create weight
MRdata <- MRdata %>%
  mutate(wt = mv005/1000000)


# create table of marital status by age
table_temp <- MRdata %>% 
  cross_rpct(
    cell_vars = list(mv013),
    col_vars = list(ms_mar_stat,total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of men"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Current marital status")

# save to workbook
sh = addWorksheet(wb, "Mar status")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)

# create table of currently married by age
table_temp <- MRdata %>% 
  cross_rpct(
    cell_vars = list(mv013),
    col_vars = list(ms_mar_union,total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of men"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Currently in union")

# save to workbook
sh = addWorksheet(wb, "Current mar")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)


# create table number of wives by age, residence, education, wealth
table_temp <- MRdata %>% 
  cross_rpct(
    subgroup = mv502==1,
    cell_vars = list(mv013, mv025, mv106, mv190),
    col_vars = list(ms_wives_num,total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of men"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Number of wives")


# save to workbook
sh = addWorksheet(wb, "Wives")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)


# create table of age at marriage by age
# NOTE - values for age groups are only valid if the youngest age in the age group
# is older than the first-marriage age being reported (e.g. married by age 18 
# cannot be reported for age group 15-19)
table_temp <- MRdata %>% 
  cross_rpct(
    cell_vars = list(mv013),
    col_vars = list(ms_afm_15, ms_afm_18, ms_afm_20, ms_afm_22, ms_afm_25, ms_mar_never, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of men"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Age at marriage")

# save to workbook
sh = addWorksheet(wb, "Mar age")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)


# create worksheet for median age at first marriage results
sh = addWorksheet(wb, "Median mar age")
writeData(wb, sheet = "Median mar age", x = median_mar_men, colNames = TRUE)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)

# create worksheet for median age at first marriage by subgroup results
sh = addWorksheet(wb, "Median mar age subgroup")
writeData(wb, sheet = "Median mar age subgroup", x = median_mar_subgroup_men, colNames = TRUE)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)


# create table of age at first sex
table_temp <- MRdata %>% 
  cross_rpct(
    cell_vars = list(mv013),
    col_vars = list(ms_afs_15, ms_afs_18, ms_afs_20, ms_afs_22, ms_afs_25, ms_sex_never, total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of men"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Age at first sex")

# save to workbook
sh = addWorksheet(wb, "Sex age")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)


# create worksheet for median age at first sex results
sh = addWorksheet(wb, "Median sex age")
writeData(wb, sheet = "Median sex age", x = median_sex_men, colNames = TRUE)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)

# create worksheet for median age at first sex by subgroup results
sh = addWorksheet(wb, "Median sex age subgroup")
writeData(wb, sheet = "Median sex age subgroup", x = median_sex_subgroup_men, colNames = TRUE)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)


# create table for recent sexual activity by age, residence, education, and wealth
table_temp <- MRdata %>% 
  cross_rpct(
    cell_vars = list(mv013, mv025, mv106, mv190),
    col_vars = list(ms_sex_recent,total()),
    weight = wt, 
    total_label = c("Total 15-49", "Number of men"),
    total_statistic = c("w_rpct", "w_cases"),
    total_row_position = c("below"),
    expss_digits(digits=1)) %>% 
  set_caption("Timing of recent sexual activity")


# save to workbook
sh = addWorksheet(wb, "Sexual activity")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap,"Tables_MS_mn.xlsx"), overwrite = TRUE)

