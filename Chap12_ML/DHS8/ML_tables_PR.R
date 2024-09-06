# ******************************************************************************
# Program: 			  ML_tables_PR.R - No changes in DHS8
# Purpose: 		    produce tables for indicators
# Data outputs:		tables on screen and in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: August 02, 2024 by Courtney Allen
# ******************************************************************************
# Note this do file will produce the following tables in excel:
# 3. Tables_MAL_ANEMIA:	Contains the table for children 6–59 months old tested for anemia and tables for children with severe anemia (<8.0 g/dL)
# 4. Tables_MALARIA:		Contains the table for children 6-59 months old tested for malaria and children with malaria infection via RDT and microscopy
# ******************************************************************************

PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)

# recode age for adults (years)
PRdata <- PRdata %>%
  mutate(age = case_when(
    hml16 %in%  (0:4) ~ 1,
    hml16 %in%  (5:14) ~ 2,
    hml16 %in%  (15:34) ~ 3,
    hml16 %in%  (35:49) ~ 4,
    hml16 %in%  (50:97) ~ 5,
    hml16 %in%  (98:99) ~ 99)) %>%
      set_value_labels(age = c("0-4"=1, "5-14"=2,"15-34"=3, "35-49"=4,"50+"=5)) %>%
      set_variable_labels(age= "Age") %>%
  replace_with_na(replace = list(age = c(99)))

# recode age for children (months)
PRdata <- PRdata %>%
  mutate(agechild = case_when(
    hml16 ==0 ~ 1,
    hml16 ==1 ~ 2,
    hml16 ==2 ~ 3,
    hml16 ==3 ~ 4,
    hml16 ==4 ~ 5)) %>%
  set_value_labels(agechild = c("<12"=1, "12-23"=2,"24-35"=3, "36-47"=4,"48-59"=5)) %>%
  set_variable_labels(agechild = "Child's age in months") %>%
  replace_with_na(replace = list(agechild = c(99)))

# recode age for biomarker testing in children (months)
PRdata <- PRdata %>%
  mutate(agechild2 = case_when(
    hc1 %in% (6:8) ~ 1,
    hc1 %in% (9:11) ~ 2,
    hc1 %in% (12:17) ~ 3,
    hc1 %in% (18:23) ~ 4,
    hc1 %in% (24:35) ~ 5,
    hc1 %in% (36:47) ~ 6,
    hc1 %in% (48:59) ~ 7)) %>%
  set_value_labels(agechild2 = c("6-8"=1, "9-11"=2,"12-17"=3, "18-23"=4,"24-35"=5, "36-47"=6, "48-59"=7)) %>%
  set_variable_labels(agechild2 = "Child's age in months") %>%
  replace_with_na(replace = list(agechild2 = c(99)))

# Indicators for population who slept under a net or ITN the night before the survey

# De-facto household population who slept the night before the survey under a mosquito net (treated or untreated)
table_temp <-  PRdata %>% 
  filter(hv103==1) %>%
  cross_rpct(
    cell_vars = list(age,hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_net),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("household population who slept the night before the survey under a mosquito net")

# save to workbook
sh = addWorksheet(wb, "pr_slept_net")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# De-facto household population who slept the night before the survey under an ITN
table_temp <-  PRdata %>% 
  filter(hv103==1) %>%
  cross_rpct(
    cell_vars = list(age,hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("household population who slept the night before the survey under an ITN")

# save to workbook
sh = addWorksheet(wb, "pr_slept_itn")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Children under age 5 who slept the night before the survey under a mosquito net (treated or untreated)
table_temp <-  PRdata %>% 
  filter(hv103==1 & hml16<5) %>%
  cross_rpct(
    cell_vars = list(agechild, hv104, hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_net),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children under age 5 who slept the night before the survey under a mosquito net")

# save to workbook
sh = addWorksheet(wb, "pr_chld_net")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Children under age 5 who slept the night before the survey under an ITN
table_temp <-  PRdata %>% 
  filter(hv103==1 & hml16<5) %>%
  cross_rpct(
    cell_vars = list(agechild, hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children under age 5 who slept the night before the survey under an ITN")

# save to workbook
sh = addWorksheet(wb, "pr_chld_itn")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Pregnant women age 15-49 who slept the night before the survey under a mosquito net (treated or untreated)
table_temp <-  PRdata %>% 
  filter(hv103==1 & hv104==2 & hml18==1 & hml16>=15 & hml16<=49) %>%
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_net),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Pregnant women age 15-49 who slept the night before the survey under a mosquito net")

# save to workbook
sh = addWorksheet(wb, "pr_preg_net")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Pregnant women age 15-49 who slept the night before the survey under an ITN
table_temp <-  PRdata %>% 
  filter(hv103==1 & hv104==2 & hml18==1 & hml16>=15 & hml16<=49) %>%
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Pregnant women age 15-49 who slept the night before the survey under an ITN")

# save to workbook
sh = addWorksheet(wb, "pr_preg_itn")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Indicators for anemia and malaria testing and prevalence: 
# Note: Testing indicators are not weighted

# Tested for Anemia
table_temp <-  PRdata %>% 
  cross_rpct(
    subgroup = (hv103==1 & hv042==1),
    cell_vars = list(agechild2, hc27, hv025, hv024, hv270, total()),
    col_vars = list(ml_test_anemia),
    # weight = wt, table should not be weighted
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Tested for Anemia")

# save to workbook
sh = addWorksheet(wb, "pr_tested")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Testing of Parasitemia (via RDT) in children 6-59 months
table_temp <-  PRdata %>% 
  cross_rpct(
    cell_vars = list(agechild2, hc27, hv025, hv024, hv270, total()),
    col_vars = list(ml_test_rdtmal),
    # weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Testing of Parasitemia (via RDT) in children 6-59 months")

# save to workbook
sh = addWorksheet(wb, "pr_tstRDT_chld")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Testing of Parasitemia (via microscopy) in children 6-59 months
table_temp <-  PRdata %>% 
  cross_rpct(
    cell_vars = list(agechild2, hc27, hv025, hv024, hv270, total()),
    col_vars = list(ml_test_micmal),
   # weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Testing of Parasitemia (via microscopy) in children 6-59 months")

# save to workbook
sh = addWorksheet(wb, "pr_tstmicro_chld")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Children with hemoglobin lower than 8.0 g/dl
table_temp <-  PRdata %>% 
  cross_rpct(
    cell_vars = list(agechild2, hc27, hv025, hv024, hv270, total()),
    col_vars = list(ml_anemia),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children with hemoglobin lower than 8.0 g/dl")

# save to workbook
sh = addWorksheet(wb, "pr_tested_hemo")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)


# Parasitemia (via RDT) in children 6-59 months
table_temp <-  PRdata %>% 
  cross_rpct(
    cell_vars = list(agechild2, hc27,hv025, hv024, hv270, total()),
    col_vars = list(ml_rdtmalpos),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Malaria (via RDT) in children 6-59 months")

# save to workbook
sh = addWorksheet(wb, "pr_RDT_chld")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# Parasitemia (via microscopy) in children 6-59 months
table_temp <-  PRdata %>% 
  cross_rpct(
    cell_vars = list(agechild2, hc27,hv025, hv024, hv270, total()),
    col_vars = list(ml_micmalpos),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Malaria (via microscopy) in children 6-59 months")

# save to workbook
sh = addWorksheet(wb, "pr_micro_chld")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)


