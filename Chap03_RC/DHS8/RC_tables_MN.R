# ******************************************************************************
# Program: 			  RC_tables_MN.R
# Purpose: 		    produce tables for indicators
# Data inputs: 		IR survey list
# Data outputs:		tables on screen and in excel sheets
# Author:				  Mahmoud Elkasabi
# Date last modified: March 31 2021 by Mahmoud Elkasabi
# ******************************************************************************

# Background characteristics of respondents - men
table_temp = MRdata %>%
  filter(mv012 <= 49) %>%   # comment out for age 15+
  tab_cells(mv013, mv501, mv025, mv106, mv024, mv190) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Background characteristics of respondents - men")

table_temp

write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "background_MN",append=FALSE) 

# Educational attainment - men

table_temp = MRdata %>%
  filter(mv012 <= 49) %>%   # comment out for age 15+
  calc_cro_rpct(
    cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
    col_vars = rc_edu,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Educational attainment: men")

table_temp 

write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "educ_MN",append=TRUE)

# National
MRdata2 = MRdata %>%
  filter(mv012 <= 49)   # comment out for age 15+

dummy.median <- MRdata2 %>% 
  summarise(weighted_median = weightedMedian(eduyr, wt, na.rm=TRUE))
MRdata2$weighted_median <- dummy.median$weighted_median

MRdata2$dummy <- ifelse(MRdata2$eduyr<MRdata2$weighted_median , 1, 0)

dummy.mean <- MRdata2 %>%
  summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
MRdata2$sL <- dummy.mean$sL

MRdata2$dummy <- ifelse(MRdata2$eduyr<=MRdata2$weighted_median , 1, 0)

dummy.mean <- MRdata2 %>%
  summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
MRdata2$sU <- dummy.mean$sU

MRdata2 <- MRdata2 %>%
  mutate(smedian = round((weighted_median-1+(0.5-sL)/(sU-sL)),2))%>%
  mutate(smedian = set_label(smedian, label = "Median years of education"))

medians_total <- MRdata2 %>% 
  summarise(MEDIANS = mean(smedian, na.rm=TRUE))


# By subgroups
MEDIANFUN <- function(Class){
  
  MRdata2 <- MRdata
  
  MRdata2$class <- MRdata2[[Class]]
  
  dummy.median <- MRdata2 %>% 
    group_by(class) %>%
    summarise(weighted_median = weightedMedian(eduyr, wt, na.rm=TRUE))
  MRdata2 <- left_join(MRdata2, dummy.median, by = 'class')
  
  MRdata2$dummy <- ifelse(MRdata2$eduyr<MRdata2$weighted_median , 1, 0)
  
  dummy.mean <- MRdata2 %>% 
    group_by(class) %>%
    summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
  MRdata2 <- left_join(MRdata2, dummy.mean, by = 'class')
  
  MRdata2$dummy <- ifelse(MRdata2$eduyr<=MRdata2$weighted_median , 1, 0)
  
  dummy.mean <- MRdata2 %>% 
    group_by(class) %>%
    summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
  MRdata2 <- left_join(MRdata2, dummy.mean, by = 'class')
  
  MRdata2 = MRdata2 %>%
    mutate(smedian = round((weighted_median-1+(0.5-sL)/(sU-sL)),2)) 
  
  medians_subgroups <- MRdata2 %>% 
    group_by(class) %>%
    summarise(MEDIANS = mean(smedian, na.rm=TRUE))
  
  return(medians_subgroups)
  
}	

M1 <- MEDIANFUN(Class = "mv013" )
M2 <- MEDIANFUN(Class = "mv025" )
M3 <- MEDIANFUN(Class = "mv024" )
M4 <- MEDIANFUN(Class = "mv190" )

write.xlsx(medians_total, "Tables_RC_MN.xlsx", sheetName = "educ_medians_national",append=TRUE)
write.xlsx(M1, "Tables_RC_MN.xlsx", sheetName = "educ_medians_age",append=TRUE)
write.xlsx(M2, "Tables_RC_MN.xlsx", sheetName = "educ_medians_residence",append=TRUE)
write.xlsx(M3, "Tables_RC_MN.xlsx", sheetName = "educ_medians_region",append=TRUE)
write.xlsx(M4, "Tables_RC_MN.xlsx", sheetName = "educ_medians_wealth",append=TRUE)


# Literacy: men

table_temp = MRdata %>%
  filter(mv012 <= 49) %>%   # comment out for age 15+
  calc_cro_rpct(
    cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
    col_vars = list(rc_litr_cats, rc_litr),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: men")

table_temp 

write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Literacy_MN",append=TRUE)


# Literacy: media exposure

table_temp = MRdata %>%
  filter(mv012 <= 49) %>%   # comment out for age 15+
  calc_cro_rpct(
    cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
    col_vars = list(rc_media_newsp, rc_media_tv, rc_media_radio, rc_media_allthree, rc_media_none ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Literacy: men")

table_temp 

write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Media_MN",append=TRUE)

#  Internet usage

if (!is.null(MRdata$mv171a)){
  
  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
      col_vars = list(rc_intr_ever, rc_intr_use12mo, rc_intr_usefreq ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Internet usage: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Internet_MN",append=TRUE)
  
}


#  Employment status
  
  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
      col_vars = list(rc_empl),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Employment status: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Employment_MN",append=TRUE)

# Occupation
  
  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
      col_vars = list(rc_occup ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Occupation: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Occupation_MN",append=TRUE)

  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(rc_empl_type, rc_empl_earn, rc_empl_cont,  total()),
      col_vars = list(rc_agri),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Type of employment: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Typeemployment_MN",append=TRUE)
  
## Health insurance  

table_temp = MRdata %>%
  filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
      col_vars = list(rc_hins_any, rc_hins_ss, rc_hins_empl, rc_hins_comm, rc_hins_priv, rc_hins_other ),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Health insurance coverage: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Hinsurance_MN",append=TRUE)
  
## Tobacco smoking  
  
  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
      col_vars = list(rc_tobc_cig, rc_tobc_other, rc_tobc_smk_any, rc_smk_freq),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Tobacco smoking: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Tobacco_MN",append=TRUE)


  ## Smokeless Tobacco use  
  
  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv013, mv025, mv024, mv190,  mv501,  mv106,  total()),
      col_vars = list(rc_tobc_snuffm, rc_tobc_snuffn, rc_tobc_chew, rc_tobv_betel, rc_tobc_osmkless, rc_tobc_anysmkless, rc_tobc_any),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Smokeless Tobacco use: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Smokeless_MN",append=TRUE)
  
  ## Average number of cigarettes smoked daily 
  
  table_temp = MRdata %>%
    filter(mv012 <= 49) %>%   # comment out for age 15+
    calc_cro_rpct(
      cell_vars = list(mv025, total()),
      col_vars = list(rc_cig_day),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      total_row_position = c("below")) %>%
    set_caption("Average number of cigarettes smoked daily: men")
  
  table_temp 
  
  write.xlsx(table_temp, "Tables_RC_MN.xlsx", sheetName = "Smoke_average_MN",append=TRUE)
  
  