# ******************************************************************************
# Program: 			  FP_tables.R
# Purpose: 		    produce tables for indicators  
# Author:				  Courtney Allen
# Date last modified: March 30 2021 by Courtney Allen
# ******************************************************************************
#   



# survey set the design with survey weights
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)


# create list of current use of family planning variables
fp_cruse_list <- c("fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                fp_cruse_wthd, fp_cruse_other")


# create table of current use by age
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = v013,
    col_vars = list(total(),fp_cruse_list),
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
    col_vars = list(total(),fp_cruse_list),
    weight = wt) %>% 
      set_caption("Current use by age, currently married women")

write.xlsx(table_temp, "Tables_FP_WM.xlsx", sheetName = "use",append=TRUE)


# create table of current use by age, sexually active, unmarried women
table_temp <-  IRdata %>% 
  filter(v502!=1 & v528<=30) %>%
    calc_cro_rpct(
    cell_vars = v013,
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


