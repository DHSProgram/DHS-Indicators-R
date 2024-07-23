# ******************************************************************************
# Program: 			  HV_tables_AR.R
# Purpose: 		    produce tables for indicators
# Data outputs:		tables in excel sheets
# Author:				  Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# ******************************************************************************
#Note this do file will produce the following tables in excel:
#1. Tables_coverage:	Contains the tables for HIV testing coverage for women, men, and total. THESE TABLES ARE UNWEIGHTED
#Not all surveys have testing that distinguish between HIV-1 and HIV-2. 

# indicators from PR file
AR_PMRdata <- AR_PMRdata %>%
  mutate(age_wm = case_when( #compute age groups for women
    ha1>=15 & ha1<=19  ~ 1,
    ha1>=20 & ha1<=24  ~ 2,
    ha1>=25 & ha1<=29  ~ 3,
    ha1>=30 & ha1<=34  ~ 4,
    ha1>=35 & ha1<=39  ~ 5,
    ha1>=40 & ha1<=44  ~ 6,
    ha1>=45 & ha1<=49  ~ 7),
    age_wm = add_labels(age_wm, labels = c("15-19"=1, "20-24"=2, "25-29"=3, "30-34"=4, "35-39"=5, "40-44"=6, "45-49"=7))) %>%
  mutate(age_mn = case_when( #compute age groups for men
    hb1>=15 & hb1<=19  ~ 1,
    hb1>=20 & hb1<=24  ~ 2,
    hb1>=25 & hb1<=29  ~ 3,
    hb1>=30 & hb1<=34  ~ 4,
    hb1>=35 & hb1<=39  ~ 5,
    hb1>=40 & hb1<=44  ~ 6,
    hb1>=45 & hb1<=49  ~ 7),
    age_mn = add_labels(age_mn, labels = c("15-19"=1, "20-24"=2, "25-29"=3, "30-34"=4, "35-39"=5, "40-44"=6, "45-49"=7))) %>%
  mutate(edu_wm= #compute education for women
           case_when(
             ha66!=8 & ha66!=9 ~ ha66,
             ha66==8 | ha66==9  ~ 9)) %>%
  mutate(edu_mn= #compute education for men
           case_when(
             hb66!=8 & hb66!=9 ~ hb66,
             hb66==8 | hb66==9  ~ 9))

# Coverage of HIV testing
  # Testing status among women
table_temp <-  AR_PMRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, age_wm, edu_wm, hv270, total()),
    col_vars = list(hv_hiv_test_wm),
    #   weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Testing status among women")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_coverage_wm",append=TRUE)
  
# Testing status among men
table_temp <-  AR_PMRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, age_mn, edu_mn, hv270, total()),
    col_vars = list(hv_hiv_test_mn),
    #   weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Testing status among men")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_coverage_mn",append=TRUE)

# Testing status among all
table_temp <-  AR_PMRdata %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(hv_hiv_test_tot),
    #   weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Testing status among all")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_coverage_all",append=TRUE)

