# ******************************************************************************
# Program: 			  RC_tables_WM.R
# Purpose: 		    produce tables for indicators
# Data inputs: 		IR dataset
# Data outputs:		tables on screen and in excel sheets
# Author:				  Mahmoud Elkasabi, Ali Roghani, Courtney Allen
# Date last modified: August 23 2024 by Courtney Allen
# ******************************************************************************

wb = createWorkbook()

# Background characteristics of respondents - women
table_temp = IRdata %>%
  tab_cells(v013,  v025, v106, v024, v190) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Background characteristics of respondents - women")

table_temp

# save to workbook
sh = addWorksheet(wb, "background")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)

# TABLES FOR EDUCATION AND LITERACY INDICATORS-----------------  

# Educational attainment - women
table_temp = IRdata %>%
  cross_rpct(
    cell_vars = list(v013, v025, v024, v190,    total()),
    col_vars = rc_edu,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Educational attainment: Women")

table_temp 

# save to workbook
sh = addWorksheet(wb, "educ")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)

# Median years of education
sh = addWorksheet(wb, "Median edu")
writeData(wb, sheet = "Median edu", x = median_edu, colNames = TRUE)
saveWorkbook(wb, here(chap,"Tables_RC_wm.xlsx"), overwrite = TRUE)

# Literacy
table_temp = IRdata %>%
  cross_rpct(
    cell_vars = list(v013, v025, v024, v190,  v106,  total()),
    col_vars = list(rc_litr_cats, rc_litr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: Women")

table_temp 

# save to workbook
sh = addWorksheet(wb, "Literacy")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)


# Literacy - media exposure
table_temp = IRdata %>%
  cross_rpct(
    cell_vars = list(v013, v025, v024, v190,   v106,  total()),
    col_vars = list(rc_media_newsp, rc_media_tv, rc_media_radio, rc_media_allthree, rc_media_none ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: Women")

table_temp 

# save to workbook
sh = addWorksheet(wb, "Media")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)


#  Internet usage
if (!is.null(IRdata$v171a)){
  table_temp = IRdata %>%
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,   v106,  total()),
      col_vars = list(rc_intr_ever, rc_intr_use12mo, rc_intr_usefreq ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Internet usage: Women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Internet")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
}


# TABLES FOR EMPLOYMENT INDICATORS -----------------  
#  Employment status
  table_temp = IRdata %>%
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,    v106,  total()),
      col_vars = list(rc_empl),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Employment status: Women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Employment")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
# Occupation
table_temp = IRdata %>%
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,    v106,  total()),
      col_vars = list(rc_occup ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Occupation: Women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Occupation")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
  table_temp = IRdata %>%
    cross_cpct(
      subgroup = emp==1 , #employed in last 12 months
      cell_vars = list(rc_empl_earn, rc_empl_type, rc_empl_cont,  total()),
      col_vars = list(rc_agri, total()),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Type of employment: Women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Typeemployment")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
# TABLES FOR INSURANCE INDICATORS -----------------  
  ## Health insurance  
  table_temp = IRdata %>%
      cross_rpct(
        cell_vars = list(v013, v025, v024, v190,   v106,  total()),
        col_vars = list(rc_hins_ss, rc_hins_empl, rc_hins_comm, rc_hins_priv, rc_hins_other, rc_hins_any),
        weight = wt,
        total_label = "Weighted N",
        total_statistic = "w_cases",
        total_row_position = c("below")) %>%
      set_caption("Health insurance coverage: Women")
    
    table_temp 
    
    # save to workbook
    sh = addWorksheet(wb, "Hinsurance")
    xl_write(table_temp, wb, sh)
    saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
    
# TABLES FOR TOBACCO INDICATORS-----------------  
  ## Tobacco smoking  
  table_temp = IRdata %>%
      cross_rpct(
        cell_vars = list(v013, v025, v024, v190,  v106,  total()),
        col_vars = list(rc_tobc_cig, rc_tobc_other, rc_tobc_smk_any),
        weight = wt,
        total_label = "Weighted N",
        total_statistic = "w_cases",
        total_row_position = c("below")) %>%
      set_caption("Tobacco smoking: Women")
    
    table_temp 
    
    # save to workbook
    sh = addWorksheet(wb, "Tobacco")
    xl_write(table_temp, wb, sh)
    saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
    

    ## Smokeless Tobacco use  
    table_temp = IRdata %>%
      cross_cpct(
        cell_vars = list(rc_tobc_snuffm, rc_tobc_snuffn, rc_tobc_chew, rc_tobv_betel, rc_tobc_osmkless, rc_tobc_anysmkless, rc_tobc_any),
        col_vars = list(total()),
        weight = wt,
        total_label = "Weighted N",
        total_statistic = "w_cases",
        total_row_position = c("below")) %>%
      set_caption("Smokeless Tobacco use: women")
    
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Smokeless")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
 
  ## Tobacco use, any  
  table_temp = IRdata %>%
    cross_rpct(
      cell_vars = list(v013, v025, v190,  v106,  total()),
      col_vars = list(rc_tobc_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Any tobacco use: women")
  
  # save to workbook
  sh = addWorksheet(wb, "Tobacco any")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE) 

# TABLES FOR ALCOHOL INDICATORSV -----------------  
  ## Consumed any alcohol
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,    v106,  total()),
      col_vars = list(rc_alc_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Consumed any alcohol: women")
  
  # save to workbook
  sh = addWorksheet(wb, "Alcohol")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
  ## Frequency of drinking
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,   v106,  total()),
      col_vars = list(rc_alc_freq),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Frequency of drinking: women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Alcohol_freq")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
  ## Average number of drinks
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,  v106,  total()),
      col_vars = list(rc_alc_drinks ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Frequency of drinking: women")
  
  # save to workbook
  sh = addWorksheet(wb, "Alcohol_average")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
  
  # TABLES FOR MIGRATION-----------------  
  
  # Place of birth
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    filter(!is.na(rc_place_birth)) %>%
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,    v106,  total()),
      col_vars = list(rc_place_birth),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Place of birth : women")
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Place_birth")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
  # Migrated in the last 5 years
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,   total()),
      col_vars = list(rc_migrant_5yrs),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Migrated in the last 5 years : women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Migrated_5year")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
# Type of migration by age
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    cross_rpct(
      cell_vars = list(v013,    total()),
      col_vars = list(rc_migrant_type),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Type of migration by age : women")
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Type_migration")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
# Reason for migration
  
  
  table_temp = IRdata %>%
    filter(v012 <= 49) %>%   # comment out for age 15+
    cross_rpct(
      cell_vars = list(v013, v025, v024, v190,   v106,  total()),
      col_vars = list(rc_migrant_reason ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Reason for migration: women")
  
  
  table_temp 
  
  # save to workbook
  sh = addWorksheet(wb, "Reason_Migration")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_WM.xlsx"), overwrite = TRUE)
  
  

  
  