# ******************************************************************************
# Program: 			  ML_tables_KR.R - DHS8 update
# Purpose: 		    produce tables for indicators
# Data outputs:		tables on screen and in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: August 16, 2024 by Courtney Allen for DHS8 update
# ******************************************************************************
# Note this do file will produce the following tables in excel:
# 6. Tables_FEVER:	Contains tables on fever careseeking for children under 5 (fever, treatment seeking)
# 7. Tables_Antimal: Contains tables for antimalarial drugs
# ******************************************************************************

KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# Child's age as background variable for tables
KRdata <- KRdata %>% mutate(age = b19)

# recode age
KRdata <- KRdata %>%
  mutate(agecat = case_when(
    age %in%  (0:11) ~ 1,
    age %in%  (12:23) ~ 2,
    age %in%  (24:35) ~ 3,
    age %in%  (36:47) ~ 4,
    age %in%  (48:60) ~ 5)) %>%
  set_value_labels(agecat = c("<12"=1, "12-23"=2,"24-35"=3, "36-47"=4,"48-59"=5)) %>%
  set_variable_labels(agecat = "Age")

# Fever and care seeking for fever
# Children under age 5 years with fever in the 2 weeks preceding the survey
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_fever),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children under age 5 years with fever in the 2 weeks preceding the survey")
table_temp
# save to workbook
sh = addWorksheet(wb, "kr_fever")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey, percentage for whom advice or treatment was sought
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_fev_care),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("advice or treatment was sought")
table_temp
# save to workbook
sh = addWorksheet(wb, "kr_fev_care")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey, percentage for whom advice or treatment was sought the same or next day
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_fev_care_day),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("advice or treatment was sought the same or next day")
table_temp
# save to workbook
sh = addWorksheet(wb, "kr_fev_day")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Children under age 5 years with fever in the 2 weeks preceding the survey who had blood taken from a finger or heel for testing
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_stick),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("blood taken from a finger or heel for testing")

# save to workbook
sh = addWorksheet(wb, "kr_stick")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Diagnosed with malaria by a healthcare provider - NEW Indicator in DHS8
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_told_malaria),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Diagnosed with malaria by a healthcare provider")
table_temp

# save to workbook
sh = addWorksheet(wb, "kr_told")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Source of advice or treatment for fever symptoms
# only the following sources are computed, to get other sources that are country specific, please see the note on these indicators in the ML_FEVER.do file

# among children with fever symptoms
table_temp = KRdata %>%
  tab_cells(ml_fev_govh, ml_fev_govcent, ml_fev_phosp, ml_fev_pdoc, ml_fev_pharm) %>%
  tab_cols()  %>%
  tab_weight(wt) %>%
  tab_stat_cpct(total_statistic = "w_cases" ) %>% 
  tab_pivot() %>% 
  tab_caption("Source of advice or treatment among children with fever symptoms")


# save to workbook
sh = addWorksheet(wb, "kr_source")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# among children with fever symptoms whom advice or treatment was sought
table_temp = KRdata %>%
  tab_cells(ml_fev_govh_trt, ml_fev_govcent_trt, ml_fev_phosp_trt, ml_fev_pdoc_trt,ml_fev_pharm_trt, ml_fev_pharm) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct(total_statistic = "w_cases" ) %>% 
  tab_pivot() %>% 
  tab_caption("Source of advice or treatment among children with fever symptoms whom advice or treatment was sought")

# save to workbook
sh = addWorksheet(wb, "kr_source_sought")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

### Antimalarial drugs
# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took an ACT
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_act, ml_sp_fan, ml_chloro, ml_amodia, ml_quin_pill, ml_quin_inj, ml_artes_rec, ml_artes_inj, ml_antimal_other),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took ACT")
table_temp
# save to workbook
sh = addWorksheet(wb, "kr_antimal")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)
