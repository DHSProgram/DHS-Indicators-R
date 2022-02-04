# ******************************************************************************
# Program: 			  ML_tables_PR.R
# Purpose: 		    produce tables for indicators
# Data outputs:		tables on screen and in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: January 12, 2022 by Mahmoud Elkasabi
# ******************************************************************************
# Note this do file will produce the following tables in excel:
# 3. Tables_MAL_ANEMIA:	Contains the table for children 6–59 months old tested for anemia and tables for children with severe anemia (<8.0 g/dL)
# 4. Tables_MALARIA:		Contains the table for children 6-59 months old tested for malaria and children with malaria infection via RDT and microscopy
# ******************************************************************************

PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)

# recode age
PRdata <- PRdata %>%
  mutate(age = case_when(
    hv105 %in%  (0:4) ~ 1,
    hv105 %in%  (5:14) ~ 2,
    hv105 %in%  (15:34) ~ 3,
    hv105 %in%  (35:49) ~ 4,
    hv105 %in%  (50:95) ~ 5,
    hv105 %in%  (96:99) ~ 99),
    age = add_labels(age, labels = c("0-4"=1, "5-14"=2,"15-34"=3, "35-49"=4,"50+"=5)),
    age = set_label(age, label = "Age"))%>%
  replace_with_na(replace = list(age = c(99)))

# Indicators for population who slept under a net or ITN the night before the survey

# De-facto household population who slept the night before the survey under a mosquito net (treated or untreated)
table_temp <-  PRdata %>% 
  filter(hv103==1) %>%
  calc_cro_rpct(
    cell_vars = list(age,hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_net),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("household population who slept the night before the survey under a mosquito net")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_slept_net",append=TRUE)

# De-facto household population who slept the night before the survey under an ITN
table_temp <-  PRdata %>% 
  filter(hv103==1) %>%
  calc_cro_rpct(
    cell_vars = list(age,hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("household population who slept the night before the survey under an ITN")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_slept_itn",append=TRUE)

# Children under age 5 who slept the night before the survey under a mosquito net (treated or untreated)
table_temp <-  PRdata %>% 
  filter(hv103==1 & hml16<5) %>%
  calc_cro_rpct(
    cell_vars = list(hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_net),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children under age 5 who slept the night before the survey under a mosquito net")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_chld_net",append=TRUE)

# Children under age 5 who slept the night before the survey under an ITN
table_temp <-  PRdata %>% 
  filter(hv103==1 & hml16<5) %>%
  calc_cro_rpct(
    cell_vars = list(hv104,hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children under age 5 who slept the night before the survey under an ITN")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_chld_itn",append=TRUE)

# Pregnant women age 15-49 who slept the night before the survey under a mosquito net (treated or untreated)
table_temp <-  PRdata %>% 
  filter(hv103==1 & hv104==2 & hml18==1 & hml16>=15 & hml16<=49) %>%
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_net),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Pregnant women age 15-49 who slept the night before the survey under a mosquito net")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_preg_net",append=TRUE)

# Pregnant women age 15-49 who slept the night before the survey under an ITN
table_temp <-  PRdata %>% 
  filter(hv103==1 & hv104==2 & hml18==1 & hml16>=15 & hml16<=49) %>%
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Pregnant women age 15-49 who slept the night before the survey under an ITN")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_preg_itn",append=TRUE)

# Indicators for anemia and malaria testing and prevalence: 
# Note: Testing indicators are not weighted

# Tested for Anemia
table_temp <-  PRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hc27,hv025, hv024, hv270, total()),
    col_vars = list(ml_test_anemia),
    # weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Tested for Anemia")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_tested",append=TRUE)

# Children with hemoglobin lower than 8.0 g/dl
table_temp <-  PRdata %>% 
    calc_cro_rpct(
      cell_vars = list(hc27,hv025, hv024, hv270, total()),
      col_vars = list(ml_anemia),
      weight = wt,
      total_label = "N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Children with hemoglobin lower than 8.0 g/dl")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_tested_chld",append=TRUE)
 
# Testing of Parasitemia (via microscopy) in children 6-59 months
table_temp <-  PRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hc27,hv025, hv024, hv270, total()),
    col_vars = list(ml_test_micmal),
   # weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Testing of Parasitemia (via microscopy) in children 6-59 months")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_tstmicro_chld",append=TRUE)
 
# Parasitemia (via microscopy) in children 6-59 months
table_temp <-  PRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hc27,hv025, hv024, hv270, total()),
    col_vars = list(ml_micmalpos),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Parasitemia (via microscopy) in children 6-59 months")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_micro_chld",append=TRUE)

# Testing of Parasitemia (via RDT) in children 6-59 months
table_temp <-  PRdata %>% 
    calc_cro_rpct(
      cell_vars = list(hc27,hv025, hv024, hv270, total()),
      col_vars = list(ml_test_rdtmal),
     # weight = wt,
      total_label = "N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Testing of Parasitemia (via RDT) in children 6-59 months")
  
write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_tstRDT_chld",append=TRUE)
  
# Parasitemia (via RDT) in children 6-59 months
table_temp <-  PRdata %>% 
    calc_cro_rpct(
      cell_vars = list(hc27,hv025, hv024, hv270, total()),
      col_vars = list(ml_rdtmalpos),
      weight = wt,
      total_label = "N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Parasitemia (via RDT) in children 6-59 months")
  
write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_RDT_chld",append=TRUE)
