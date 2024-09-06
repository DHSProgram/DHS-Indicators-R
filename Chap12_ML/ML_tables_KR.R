# ******************************************************************************
# Program: 			  ML_tables_KR.R
# Purpose: 		    produce tables for indicators
# Data outputs:		tables on screen and in excel sheets
# Author:				  Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: September 5, 2024 by Courtney Allen
# ******************************************************************************
# Note this do file will produce the following tables in excel:
# 6. Tables_FEVER:	Contains tables on fever careseeking for children under 5 (fever, treatment seeking)
# 7. Tables_Antimal: Contains tables for antimalarial drugs
# ******************************************************************************

KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# Child's age as background variable for tables
# If b19 is not available in the data use v008 - b3
if ("TRUE" %in% (!("b19" %in% names(KRdata))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  KRdata <- KRdata %>%
    mutate(age = b19)
} else {
  KRdata <- KRdata %>%
    mutate(age = v008 - b3)
}

# recode age
KRdata <- KRdata %>%
  mutate(agecat = case_when(
    age %in%  (0:11) ~ 1,
    age %in%  (12:23) ~ 2,
    age %in%  (24:35) ~ 3,
    age %in%  (36:47) ~ 4,
    age %in%  (48:60) ~ 5),
    agecat = add_labels(agecat, labels = c("<12"=1, "12-23"=2,"24-35"=3, "36-47"=4,"48-59"=5)),
    agecat = set_label(agecat, label = "Age"))

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

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_fever",append=TRUE)

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

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_fev_care",append=TRUE)

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

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_fev_day",append=TRUE)

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

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_stick",append=TRUE)

# Source of advice or treatment for fever symptoms
# only the following sources are computed, to get other sources that are country specific, please see the note on these indicators in the ML_FEVER.do file

# among children with fever symptoms
table_temp = KRdata %>%
  tab_cells(ml_fev_govh, ml_fev_govcent, ml_fev_pclinc, ml_fev_pdoc, ml_fev_pharm) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Source of advice or treatment among children with fever symptoms")


write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_source",append=TRUE)

# among children with fever symptoms whom advice or treatment was sought
table_temp = KRdata %>%
  tab_cells(ml_fev_govh_trt, ml_fev_govcent_trt, ml_fev_pclinc_trt, ml_fev_pdoc_trt,ml_fev_pharm_trt, ml_fev_pharm) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Source of advice or treatment among children with fever symptoms whom advice or treatment was sought")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_source_sought",append=TRUE)

### Antimalarial drugs
# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took an ACT
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_act),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took ACT")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_ACT",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took SP/Fansider
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_sp_fan),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took SP/Fansider")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_SP",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took Chloroquine
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_chloro),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took Chloroquine")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_Chloroquine",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took Amodiaquine
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_amodia),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took Amodiaquine")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_Amodiaquine",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took Quinine pills
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_quin_pill),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took Quinine pills")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_Quinine",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took Quinine injection or IV
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_quin_inj),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took Quinine injection or IV")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_Quinine_inj",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took Artesunate rectal
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_artes_rec),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took Artesunate rectal")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_Artesunate",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took Artesunate injection or intravenous
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_artes_inj),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took Artesunate injection or intravenous")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_Artesunate_inj",append=TRUE)

# Among children under age 5 years with fever in the 2 weeks preceding the survey who took any antimalarial medication, percentage who took other antimalarial
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(agecat, b4, v025, v024, v106, v190, total()),
    col_vars = list(ml_antimal_other),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Children took other antimalarial")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "kr_antimalarial_other",append=TRUE)
