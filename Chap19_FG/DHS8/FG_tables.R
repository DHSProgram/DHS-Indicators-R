# /*****************************************************************************************************
# Program: 			FG_tables.R
# Purpose: 			produce tables for indicators
# Author:				Shireen Assaf
# Date last modified: September 22, 2022 by Shireen Assaf 
# 
# *This do file will produce the following tables in excel in the Tables_Circum.xls excel file:
# 1. 	Know_Prev_Type_wm:	Contains the tables for heard of female circumcision and prevalence and type of female circumcision among women age 15-49 
# 2. 	AgeFC_wm:	          Contains the table for age of circumcision among women age 15-49
# 3.  Person_wm:          Contains the table for who performed the circumcision among women age 15-49
# 4.	Opinions_wm:		    Contains the tables for opinions related to female circumcision among women age 15-49
# 5.  Know_mn:            Contains the tables for heard of female circumcision among men age 15-49 
# 6.	Opinions_mn:		    Contains the tables for opinions related to female circumcision among men age 15-49

# Notes: 	

# Tables are produced among women and men age 15-49. 
# If tables are needed for all men (for some surveys this can be over age 49) the filter can be changed in the code. 

# Tables for circumcision among girls are produced in the FG_GIRLS.R or FG_GIRLS_merge.R files.
# *****************************************************************************************************

# Tables among women

# //Heard of female circumcision and prevalence and type of female circumcision among women age 15-49
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list( v013, v025, v024, v106, v190, total()),
    col_vars = list(fg_heard, fg_fcircum_wm, fg_type_wm),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Knowledge, prevalence, and type of FC among women age 15-49")
write.xlsx(table_temp, "Chap18_FG/Tables_Circum.xls", sheetName = "Know_Prev_Type_wm", append=TRUE)

# ****************************************************
# //Age at circumcision
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list( v013, v025, v024, v106, v190, total()),
    col_vars = list(fg_age_wm),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Age at FC among women age 15-49")
write.xlsx(table_temp, "Chap18_FG/Tables_Circum.xls", sheetName = "AgeFC_wm", append=TRUE)

# ****************************************************
# //Person performing the circumcision among women age 15-49
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list( total()),
    col_vars = list(fg_who_wm),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Person who performed FC")
write.xlsx(table_temp, "Chap18_FG/Tables_Circum.xls", sheetName = "Person_wm", append=TRUE)

# ****************************************************
# //Opinion on whether female circumcision is required by their religion or should continue among women
table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list( fg_fcircum_wm, v013, v025, v024, v106, v190, total()),
    col_vars = list(fg_relig, fg_cont ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Opinions on FC among women age 15-49")
write.xlsx(table_temp, "Chap18_FG/Tables_Circum.xls", sheetName = "Opinions_wm", append=TRUE)


# ****************************************************************************
# Tables among men

# //Heard of female circumcision
table_temp <-  MRdata %>% 
  filter(mv012>=15 & mv012<=49) %>%
  cross_rpct(
    cell_vars = list( mv013, mv025, mv024, mv106, mv190, total()),
    col_vars = list(fg_heard),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Knowledge of FC among men age 15-49")
write.xlsx(table_temp, "Chap18_FG/Tables_Circum.xls", sheetName = "Know_mn", append=TRUE)

# ****************************************************

# //Opinion on whether female circumcision is required by their religion or should continue among men
table_temp <-  MRdata %>% 
  filter(mv012>=15 & mv012<=49) %>%
  cross_rpct(
    cell_vars = list( mv013, mv025, mv024, mv106, mv190, total()),
    col_vars = list(fg_relig, fg_cont ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Opinions on FC among men age 15-49")
write.xlsx(table_temp, "Chap18_FG/Tables_Circum.xls", sheetName = "Opinions_mn", append=TRUE)

