# ******************************************************************************
# Program: 			  RC_tables_WM.R
# Purpose: 		    produce tables for indicators
# Data inputs: 		IR survey list
# Data outputs:		tables on screen and in excel sheets
# Author:				  Mahmoud Elkasabi
# Date last modified: March 31 2021 by Mahmoud Elkasabi
# ******************************************************************************

# Background characteristics of respondents - women
table_temp = IRdata %>%
  tab_cells(v013, v501, v025, v106, v024, v190) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Background characteristics of respondents - women")

table_temp

write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "background_wm",append=FALSE) 

# Educational attainment - women
table_temp = IRdata %>%
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
    col_vars = rc_edu,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Educational attainment: Women")

table_temp 

write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "educ_wm",append=TRUE)

# National
IRdata2 = IRdata

dummy.median <- IRdata2 %>% 
  summarise(weighted_median = weightedMedian(eduyr, wt, na.rm=TRUE))
IRdata2$weighted_median <- dummy.median$weighted_median

IRdata2$dummy <- ifelse(IRdata2$eduyr<IRdata2$weighted_median , 1, 0)

dummy.mean <- IRdata2 %>%
  summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
IRdata2$sL <- dummy.mean$sL

IRdata2$dummy <- ifelse(IRdata2$eduyr<=IRdata2$weighted_median , 1, 0)

dummy.mean <- IRdata2 %>%
  summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
IRdata2$sU <- dummy.mean$sU

IRdata2 <- IRdata2 %>%
  mutate(smedian = round((weighted_median-1+(0.5-sL)/(sU-sL)),2))%>%
  mutate(smedian = set_label(smedian, label = "Median years of education"))

medians_total <- IRdata2 %>% 
  summarise(MEDIANS = mean(smedian, na.rm=TRUE))

# By subgroups
MEDIANFUN <- function(Class){
  
  IRdata2 <- IRdata
  
  IRdata2$class <- IRdata2[[Class]]
  
  dummy.median <- IRdata2 %>% 
    group_by(class) %>%
    summarise(weighted_median = weightedMedian(eduyr, wt, na.rm=TRUE))
  IRdata2 <- left_join(IRdata2, dummy.median, by = 'class')
  
  IRdata2$dummy <- ifelse(IRdata2$eduyr<IRdata2$weighted_median , 1, 0)
  
  dummy.mean <- IRdata2 %>% 
    group_by(class) %>%
    summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
  IRdata2 <- left_join(IRdata2, dummy.mean, by = 'class')
  
  IRdata2$dummy <- ifelse(IRdata2$eduyr<=IRdata2$weighted_median , 1, 0)
  
  dummy.mean <- IRdata2 %>% 
    group_by(class) %>%
    summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
  IRdata2 <- left_join(IRdata2, dummy.mean, by = 'class')
  
  IRdata2 = IRdata2 %>%
    mutate(smedian = round((weighted_median-1+(0.5-sL)/(sU-sL)),2)) 
  
  medians_subgroups <- IRdata2 %>% 
    group_by(class) %>%
    summarise(MEDIANS = mean(smedian, na.rm=TRUE))
  
  return(medians_subgroups)
  
}	

M1 <- MEDIANFUN(Class = "v013" )
M2 <- MEDIANFUN(Class = "v025" )
M3 <- MEDIANFUN(Class = "v024" )
M4 <- MEDIANFUN(Class = "v190" )

write.xlsx(medians_total, "Tables_RC_WM.xlsx", sheetName = "educ_medians_national",append=TRUE)
write.xlsx(M1, "Tables_RC_WM.xlsx", sheetName = "educ_medians_age",append=TRUE)
write.xlsx(M2, "Tables_RC_WM.xlsx", sheetName = "educ_medians_residence",append=TRUE)
write.xlsx(M3, "Tables_RC_WM.xlsx", sheetName = "educ_medians_region",append=TRUE)
write.xlsx(M4, "Tables_RC_WM.xlsx", sheetName = "educ_medians_wealth",append=TRUE)


# Literacy: Women

table_temp = IRdata %>%
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
    col_vars = list(rc_litr_cats, rc_litr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: Women")

table_temp 

write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Literacy_wm",append=TRUE)


# Literacy: media exposure
table_temp = IRdata %>%
  calc_cro_rpct(
    cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
    col_vars = list(rc_media_newsp, rc_media_tv, rc_media_radio, rc_media_allthree, rc_media_none ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: Women")

table_temp 

write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Media_wm",append=TRUE)


#  Internet usage
if (!is.null(IRdata$v171a)){
  
  table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
      col_vars = list(rc_intr_ever, rc_intr_use12mo, rc_intr_usefreq ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Internet usage: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Internet_wm",append=TRUE)
  
}


#  Employment status
  table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
      col_vars = list(rc_empl),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Employment status: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Employment_wm",append=TRUE)

# Occupation
table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
      col_vars = list(rc_occup ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Occupation: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Occupation_wm",append=TRUE)

  table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(rc_empl_type, rc_empl_earn, rc_empl_cont,  total()),
      col_vars = list(rc_agri),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Type of employment: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Typeemployment_wm",append=TRUE)
  
## Health insurance  
table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
      col_vars = list(rc_hins_any, rc_hins_ss, rc_hins_empl, rc_hins_comm, rc_hins_priv, rc_hins_other ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Health insurance coverage: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Hinsurance_wm",append=TRUE)
  
## Tobacco smoking  
table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
      col_vars = list(rc_tobc_cig, rc_tobc_other, rc_tobc_smk_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Tobacco smoking: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Tobacco_wm",append=TRUE)


  ## Smokeless Tobacco use  
  
  table_temp = IRdata %>%
    calc_cro_rpct(
      cell_vars = list(v013, v025, v024, v190,  v501,  v106,  total()),
      col_vars = list(rc_tobc_snuffm, rc_tobc_snuffn, rc_tobc_chew, rc_tobv_betel, rc_tobc_osmkless, rc_tobc_anysmkless, rc_tobc_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Smokeless Tobacco use: Women")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_WM.xlsx", sheetName = "Smokeless_wm",append=TRUE)
  