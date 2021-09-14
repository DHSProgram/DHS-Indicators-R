# ******************************************************************************
# Program: 			  FP_tables.R
# Purpose: 		    produce tables for indicators  
# Author:				  Courtney Allen
# Date last modified: May 17 2021 by Courtney Allen
# ******************************************************************************
#   

# survey set the design with survey weights
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# set expss package options to show one decimal place
expss_digits(digits=1)

# create list of current use of family planning variables
fp_cruse_list <- IRdata %>% list(fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                fp_cruse_wthd, fp_cruse_other)

# create population groups that are shown in tables

# dummy var for all women
IRdata <- IRdata %>% mutate(fp_all = case_when(
  v007>0  ~ "all"),
  fp_all = set_label(fp_all, label = "all women"))

# only married women
IRdata <- IRdata %>% mutate(fp_married = case_when(
  v502==1  ~ "married"),
  fp_married = set_label(fp_married, label = "currently married women"))

# sexually active unmarried women (SAUW); sexually active if had sex in last 30 days
IRdata <- IRdata %>% mutate(fp_sauw = case_when(
  v502!=1 & v528<=30 ~ "SAUW"),
sauw = set_label(fp_sauw, label = "sexally active, unmarried women"))
  

# table: FP knowledge of any method, any modern method, any traditional method
table_temp <-  IRdata %>% 
  calc_cro_cpct(
    cell_vars = list(fp_know_any,   fp_know_mod,   fp_know_fster,   fp_know_mster, 
                     fp_know_pill, fp_know_iud,   fp_know_inj,     fp_know_imp, 
                     fp_know_mcond, fp_know_fcond,  fp_know_ec,  fp_know_sdm,  fp_know_lam, 
                     fp_know_omod, fp_know_trad, fp_know_rhy,  fp_know_wthd, fp_know_other),
    col_vars = list(fp_all,fp_married,fp_sauw),
    weight = wt,
    expss_digits(digits=1)) %>%   
  set_caption("Knowledge of family planning methods")

write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use")


# table: FP knowledge of any method, any modern method, any traditional method by background variables
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v106, v190),
    col_vars = list(fp_know_any,   fp_know_mod, total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Knowledge of family planning methods")

write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use")



# table: mean number of family planning methods known - this table needs updates
# table_temp <-  IRdata %>% 
#   calc_cro_cpct(
#     cell_vars = mean(),
#     weight = wt,
#     expss_digits(digits=1)) %>%   
#   set_caption("Mean number of family planning methods known")
# 
# write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use")


# table: current use by age
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = v013,
    col_vars = list(total(),fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                    fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                    fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                    fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                    fp_cruse_wthd, fp_cruse_other),
    weight = wt, 
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%  
  set_caption("Current use by age, all women")

write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use",append=TRUE)


# create table of current use by age, currently married
table_temp <-  IRdata %>% 
  filter(v502==1) %>%
    calc_cro_rpct(
    cell_vars = v013,
    col_vars = list(total(),fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                    fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                    fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                    fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                    fp_cruse_wthd, fp_cruse_other),
    weight = wt) %>% 
      set_caption("Current use by age, currently married women")

write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use",append=TRUE)


# create table of current use by number of living children, sexually active, unmarried women
table_temp <-  IRdata %>% 
  filter(v502!=1 & v528<=30) %>%
    calc_cro_rpct(
    cell_vars = v201,
    col_vars = list(total(),fp_cruse_any, fp_cruse_mod, fp_cruse_fster,
                    fp_cruse_mster, fp_cruse_pill, fp_cruse_iud, fp_cruse_inj,
                    fp_cruse_imp, fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph,
                    fp_cruse_lam, fp_cruse_ec, fp_cruse_omod, fp_cruse_trad,
                    fp_cruse_rhy, fp_cruse_wthd, fp_cruse_other),
    weight = wt) %>% 
  set_caption("Current use by age, sexually active, unmarried women")
  
write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use",append=TRUE)


# create table of current use by age, sexually active, unmarried women
table_temp <-  IRdata %>% 
  filter(v502==1) %>%
    calc_cro_rpct(
      cell_vars = v024, v025, v106, v190,
      col_vars = list(total(),fp_cruse_any, fp_cruse_mod, fp_cruse_fster,
                      fp_cruse_mster, fp_cruse_pill, fp_cruse_iud, fp_cruse_inj,
                      fp_cruse_imp, fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph,
                      fp_cruse_lam, fp_cruse_ec, fp_cruse_omod, fp_cruse_trad,
                      fp_cruse_rhy, fp_cruse_wthd, fp_cruse_other),
      weight = wt) %>% 
      set_caption("Current use by age, sexually active, unmarried women")


