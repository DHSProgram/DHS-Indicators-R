# ******************************************************************************
# Program: 			  RC_tables_MN.R 
# Purpose: 		    produce tables for indicators
# Data inputs: 		IR survey list
# Data outputs:		tables on screen and in excel sheets
# Author:				  Mahmoud Elkasabi, Ali Roghani, Courtney Allen
# Date last modified: August 23 2024 by Courtney Allen
# ******************************************************************************

# create workbook for tables
wb = createWorkbook()

# Background characteristics of respondents - men
table_temp = MRdata %>%
  tab_subgroup(mv012<=49) %>%
  tab_cells(mv013,  mv025, mv106, mv024, mv190) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Background characteristics of respondents - men")

table_temp

# save to workbook
sh = addWorksheet(wb, "background_mn")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

# TABLES FOR EDUCATION AND LITERACY INDICATORS ---------------------
# Educational attainment - men
  table_temp = MRdata %>%
  cross_rpct(
    subgroup= mv012<=49,
    cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
    col_vars = rc_edu,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Educational attainment: men")

table_temp 

# save to workbook
sh = addWorksheet(wb, "educ_mn")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)


# Median years of education
sh = addWorksheet(wb, "Median edu")
writeData(wb, sheet = "Median edu", x = median_edu_men, colNames = TRUE)
saveWorkbook(wb, here(chap,"Tables_RC_mn.xlsx"), overwrite = TRUE)

# Literacy: men

table_temp = MRdata %>%
  cross_rpct(
    subgroup= mv012<=49,
    cell_vars = list(mv013, mv025, mv024, mv190,    mv106,  total()),
    col_vars = list(rc_litr_cats, rc_litr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: men")

table_temp 

# save to workbook
sh = addWorksheet(wb, "Literacy")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)


# Literacy: media exposure

table_temp = MRdata %>%
  cross_rpct(
    subgroup= mv012<=49,
    cell_vars = list(mv013, mv025, mv024, mv190,    mv106,  total()),
    col_vars = list(rc_media_newsp, rc_media_tv, rc_media_radio, rc_media_allthree, rc_media_none ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: men")

table_temp 

# save to workbook
sh = addWorksheet(wb, "Media")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

#  Internet usage
if (!is.null(MRdata$mv171a)){
  
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
      col_vars = list(rc_intr_ever, rc_intr_use12mo, rc_intr_usefreq ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Internet usage: men")
  
  table_temp 
  
# save to workbook
sh = addWorksheet(wb, "Internet")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

}

# TABLES FOR EMPLOYMENT INDICATORS---------------------

#  Employment status
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
      col_vars = list(rc_empl),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Employment status: men")
  
  # save to workbook
  sh = addWorksheet(wb, "Mar status")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap,"Tables_RC_MN.xlsx"), overwrite = TRUE)

# Occupation
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,  mv106,  total()),
      col_vars = list(rc_occup ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Occupation: men")
  
  # save to workbook
  sh = addWorksheet(wb, "Occupation")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)
  
  table_temp = MRdata %>%
    cross_cpct(
      subgroup =   emp==1 , #employed in last 12 months
      cell_vars = list(rc_empl_earn, rc_empl_cont,  total()),
      col_vars = list(rc_agri,total()),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Type of employment: men")
  
  table_temp 
  
# save to workbook
sh = addWorksheet(wb, "Typeemployment")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

# TABLES FOR HEALTH INSURANCE INDICATORS---------------------

## Health insurance  

table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
      col_vars = list(rc_hins_ss, rc_hins_empl, rc_hins_comm, rc_hins_priv, rc_hins_other, rc_hins_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Health insurance coverage: men")

# save to workbook
sh = addWorksheet(wb, "Hinsurance")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

# TABLES FOR TOBACCO INDICATORS ---------------------

## Tobacco smoking  
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,  mv106,  total()),
      col_vars = list(rc_tobc_cig, rc_tobc_other, rc_tobc_smk_any, rc_smk_freq),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Tobacco smoking: men")

# save to workbook
sh = addWorksheet(wb, "Tobacco")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

## Average number of cigarettes smoked daily 
table_temp = MRdata %>%
  cross_rpct(
    subgroup= mv012<=49 & !is.na(rc_cig_day),
    cell_vars = list(mv025, total()),
    col_vars = list(rc_cig_day),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Average number of cigarettes smoked daily: men")

# save to workbook
sh = addWorksheet(wb, "Smoke_average")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

## Smokeless Tobacco use  
  table_temp = MRdata %>%
    cross_cpct(
      subgroup= mv012<=49,
      cell_vars = list(rc_tobc_snuffm, rc_tobc_snuffn, rc_tobc_chew, rc_tobv_betel, rc_tobc_osmkless, rc_tobc_anysmkless, rc_tobc_any),
      col_vars = list(total()),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Smokeless Tobacco use: men")
  
t# save to workbook
sh = addWorksheet(wb, "Smokeless")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)


## Tobacco use, any  
table_temp = MRdata %>%
  cross_rpct(
    subgroup= mv012<=49,
    cell_vars = list(mv013, mv025, mv190,  mv106,  total()),
    col_vars = list(rc_tobc_any),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Any tobacco use: men")

# save to workbook
sh = addWorksheet(wb, "Tobacco any")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

# TABLES FOR ALCOHOL INDICATORS ---------------------

## Consumed any alcohol
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
      col_vars = list(rc_alc_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Consumed any alcohol: men")
  
  
# save to workbook
sh = addWorksheet(wb, "Alcohol")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

## Frequency of drinking
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,  mv106,  total()),
      col_vars = list(rc_alc_freq),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Frequency of drinking: men")
  
  
  # save to workbook
  sh = addWorksheet(wb, "Alcohol_freq")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

## Average number of drinks
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190, mv106,  total()),
      col_vars = list(rc_alc_drinks ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Frequency of drinking: men")
  
  # save to workbook
  sh = addWorksheet(wb, "Alcohol_average")
  xl_write(table_temp, wb, sh)
  saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

## TABLES FOR MIGRATION INDICATORS------
  
# Place of birth
  table_temp = MRdata %>%
    cross_rpct(
      subgroup= mv012<=49,
      cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
      col_vars = list(rc_place_birth),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Place of birth : men")
  
   # save to workbook
   sh = addWorksheet(wb, "Place_birth")
   xl_write(table_temp, wb, sh)
   saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

# Migrated in the last 5 years
 table_temp = MRdata %>%
   cross_rpct(
     subgroup = mv012<=49 & !is.na(rc_migrant_5yrs),
     cell_vars = list(mv013, mv025, mv024, mv190,   total()),
     col_vars = list(rc_migrant_5yrs, total()),
     weight = wt,
     total_label = "Weighted N",
     total_statistic = "w_cases",
     total_row_position = c("below")) %>%
   set_caption("Migrated in the last 5 years : men")
 
 # save to workbook
 sh = addWorksheet(wb, "Migrant_5year")
 xl_write(table_temp, wb, sh)
 saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)
 
# Type of migration by age
 table_temp = MRdata %>%
   cross_rpct(
     subgroup= mv012<=49,
     cell_vars = list(mv013,    total()),
     col_vars = list(rc_migrant_type),
     weight = wt,
     total_label = "Weighted N",
     total_statistic = "w_cases",
     total_row_position = c("below")) %>%
   set_caption("Type of migration by age : men")

 # save to workbook
 sh = addWorksheet(wb, "Migrant_type")
 xl_write(table_temp, wb, sh)
 saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)

# Reason for migration
  table_temp = MRdata %>%
   cross_rpct(
     subgroup= mv012<=49,
     cell_vars = list(mv013, mv025, mv024, mv190,   mv106,  total()),
     col_vars = list(rc_migrant_reason ),
     weight = wt,
     total_label = "Weighted N",
     total_statistic = "w_cases",
     total_row_position = c("below")) %>%
   set_caption("Reason for migration: men")

# save to workbook
 sh = addWorksheet(wb, "Migrant_reason")
 xl_write(table_temp, wb, sh)
 saveWorkbook(wb, here(chap, "/Tables_RC_MN.xlsx"), overwrite = TRUE)
