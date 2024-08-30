# ******************************************************************************
# Program: 			  FE_FERT.R
# Purpose: 		    Code to compute current fertility indicators  
# Data inputs: 		IR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified: June 2024 Ali Roghani
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Variables created in this file:
#  //FERTILITY
#fe_preg			"Currently pregnant"
#fe_ceb_num		"Number of children ever born (CEB)"
#fe_ceb_mean		"Mean number of CEB"

#fe_ceb_comp  	"Completed fertility - Mean number of CEB to women age 40-49"
#fe_live_mean	"Mean number of living children"

#//MENOPAUSE
#fe_meno			"Menopausal"

#//TEEN PREGNANCY AND MOTHERHOOD
#fe_teen_birth	"Teens who have had a live birth"
#fe_teen_preg	"Teens pregnant with first child"
#fe_teen_beg		"Teens who have begun childbearing"

#//FIRST BIRTH
#fe_birth_never	"Never had a birth"
#fe_afb_15		"First birth by age 15"
#fe_afb_18		"First birth by age 18"
#fe_afb_20		"First birth by age 20"
#fe_afb_22		"First birth by age 22"
#fe_afb_25		"First birth by age 25"
#fe_mafb_25		"Median age at first birth among age 25-49"
# -----------------------------------------------------------------------------#
#

IRdata <- IRdata %>% 
  mutate(wt = v005/1000000) %>%
  mutate(residence = v025) %>% 
  mutate(region = v024) %>%
  mutate(wealth = v190) %>%  
  mutate(education = v106)

# Essential for tables with means - ideally labels should be assigned for regions too
IRdata <- expss::apply_labels(
  IRdata,
  residence = c("urban" = 1, "rural" = 2),
  education = c("No education" = 0, "Primary" = 1, "Secondary" = 2, "More than secondary" = 3),
  wealth = c("poorest" = 1, "poorer" = 2, "middle" = 3, "richer" = 4, "richest" = 5)
)


#############################################################################################################################################

# Currently pregnant by background variables

# TABLE 5.2

IRdata = IRdata %>%
  mutate(fe_preg = v213) %>%  # Currently Pregnant
  mutate(fe_preg = factor(fe_preg, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_preg = set_label(fe_preg, label = "Currently pregnant by background variables"))
  
table_temp = IRdata %>%
  calc_cro_rpct(
    cell_vars = list(residence, region, education, wealth, total()),
    col_vars = fe_preg,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Currently Pregnant among women 15-49")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_preg",append=TRUE)

# Number of children ever born (CEB) #################################
# TABLE 5.4

IRdata = IRdata %>%
  mutate(fe_ceb_num = case_when(
    v201 < 10 ~ v201,      
    v201 > 9 ~ 10)) %>%
  add_labels(fe_ceb_num, labels = c(`10+` = 10))
  
table_temp = IRdata %>%
  calc_cro_rpct(
    cell_vars = list(v013, total()),
    col_vars = fe_ceb_num ,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Number of children ever born (CEB)")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_ceb_num",append=TRUE)

# Mean number of CEB #################################

IRdata <- expss::apply_labels(
  IRdata,
  v013 = c("15-19" = 1, "20-24" = 2, "25-29" = 3, "30-34" = 4, "35-39" = 5, "40-44" = 6, "45-49" = 7),
  v201 = "|"
)

table_temp = IRdata %>%
  tab_cells(v201) %>%
  tab_rows(total(), v013) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("fe_ceb_mean" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean number of CEB")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_ceb_mean",append=TRUE)


# Among currently married women #############################################

# TABLE 5.4

table_temp = IRdata %>%
  filter(v502 == 1) %>%
  calc_cro_rpct(
    cell_vars = list(v013, total()),
    col_vars = fe_ceb_num,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Number of children ever born among currently married women")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "CM_fe_ceb_num",append=TRUE)

table_temp = IRdata %>%
  filter(v502 == 1) %>% 
  tab_cells(v201) %>%
  tab_rows(total(), v013) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("CM_fe_ceb_mean" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean number of children ever born (CEB) among currently married women ")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "CM_fe_ceb_mean",append=TRUE)

#Mean number of children ever born (CEB) ########################################################################

# TABLE 5.2

IRdata = IRdata %>%
  mutate(fe_ceb_comp = ifelse(v013 >= 6, v201, NA )) 

IRdata <- expss::apply_labels(
  IRdata,
  fe_ceb_comp = "|"
)

table_temp = IRdata %>%
  filter(v013 >= 6) %>%
  tab_cells(fe_ceb_comp) %>%
  tab_rows(total(), residence, region, education, wealth) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("fe_ceb_comp" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Completed fertility - Mean number of CEB to women age 40-49")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_ceb_comp",append=TRUE)

#Mean number of living children among all women #################################################################

# TABLE 5.4

IRdata = IRdata %>%
  mutate(fe_live_mean = v218) 

IRdata <- expss::apply_labels(
  IRdata,
  fe_live_mean = "|"
)

table_temp = IRdata %>%
  tab_cells(fe_live_mean) %>%
  tab_rows(total(), v013) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("fe_live_mean" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean number of living children")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_live_mean",append=TRUE)

#Mean number of living children among currently married women #################################################################

# TABLE 5.4

table_temp = IRdata %>%
  filter(v502 == 1) %>%
  tab_cells(fe_live_mean) %>%
  tab_rows(total(), v013) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun("CM_fe_live_mean" = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("Mean number of living children among currently married women")

table_temp

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "CM_fe_live_mean",append=TRUE)

# Experienced menopause by age ##################################################################################

# TABLE 5.8

IRdata = IRdata %>%
  mutate(fe_meno = case_when(
    v013 > 3 & (v226>5 & v226<997) & v213==0 & v405==0  ~ 1,
    TRUE ~ 0)) %>%
  mutate(fe_meno = factor(fe_meno, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_meno = set_label(fe_meno, label = "Experienced menopause")) %>%
  mutate( fe_meno_age = cut(v012, breaks= c(0,30,35,40,42,44,46,48,50),right = FALSE)) %>%  
  mutate(fe_meno_age = set_label(fe_meno_age, label = "Age groups for Menopause table"))

IRdata <- expss::apply_labels(
  IRdata,
  fe_meno = "|"
)

table_temp = IRdata %>%
  filter(v012 >= 30) %>%
  calc_cro_rpct(
    cell_vars = list(fe_meno_age, total()),
    col_vars = fe_meno,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Menopausal")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_meno",append=TRUE)

## // TEEN PREGNANCY AND MOTHERHOOD

## Teens (age 15-19) had a live birth by background variables

# TABLE 5.11

IRdata = IRdata %>%
  mutate(fe_teen_birth = case_when(
    v201 == 0 ~ 0,
    v201 > 0 ~ 1)) %>%
  mutate(fe_teen_birth = factor(fe_teen_birth, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_teen_birth = set_label(fe_teen_birth, label = "Teens who have had a live birth"))

table_temp = IRdata %>%
  filter(v013 == 1) %>% 
  calc_cro_rpct(
    cell_vars = list(v012, residence, region, education, wealth, total()),
    col_vars = fe_teen_birth,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Teens (age 15-19) had a live birth by background variables")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_teen_birth",append=TRUE)


#########################################################################################################################################
## Teens (age 15-19) currently pregnant by background variables

# TABLE 5.11

IRdata = IRdata %>%
  mutate(fe_teen_preg = case_when(
    (v201!=0 | v213!=1) ~ 0,
    v201==0 & v213==1 ~ 1)) %>%
  mutate(fe_teen_preg = factor(fe_teen_preg, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_teen_preg = set_label(fe_teen_preg, label = "Teens pregnant with first child"))

table_temp = IRdata %>%
  filter(v013 == 1) %>% 
  calc_cro_rpct(
    cell_vars = list(v012, residence, region, education, wealth, total()),
    col_vars = fe_teen_preg,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Teens pregnant with first child")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_teen_preg",append=TRUE)


#########################################################################################################################################
## Teens (women age 15-19) who have begun childbearing

# TABLE 5.11

IRdata = IRdata %>%
  mutate(fe_teen_beg = case_when(
    v013 == 1 & (v201 == 0 & v213!=1) ~ 0,
    v013 == 1 & (v201 > 0 | v213==1) ~ 1)) %>%
  mutate(fe_teen_beg = factor(fe_teen_beg, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_teen_beg = set_label(fe_teen_beg, label = "Teens who have begun childbearing"))

table_temp = IRdata %>%
  filter(v013 == 1) %>% 
  calc_cro_rpct(
    cell_vars = list(v012, residence, region, education, wealth, total()),
    col_vars = fe_teen_beg,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Teens (women age 15-19) who have begun childbearing")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "fe_teen_beg",append=TRUE)

###############################################################################################################################################################################

# First birth by specific ages #####################################################################

# TABLE 5.9

IRdata = IRdata %>%
  mutate(fe_afb_15 = case_when(
    is.na(v212) | (v212 >= 15 & v212 < 50) ~ 0,
    v212 >= 0 & v212 < 15 ~ 1)) %>%
  mutate(fe_afb_15 = factor(fe_afb_15, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_afb_15 = set_label(fe_afb_15, label = "First birth by age 15")) %>%
  mutate(fe_afb_18 = case_when(
    v012 >= 18 & (is.na(v212) | (v212 >= 18 & v212 < 50)) ~ 0,
    v012 >= 18 & (v212 >= 0 & v212 < 18) ~ 1)) %>%
  mutate(fe_afb_18 = factor(fe_afb_18, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_afb_18 = set_label(fe_afb_18, label = "First birth by age 18")) %>%
  mutate(fe_afb_20 = case_when(
    v012 >= 20 & (is.na(v212) | (v212 >= 20 & v212 < 50)) ~ 0,
    v012 >= 20 & (v212 >= 0 & v212 < 20) ~ 1)) %>%
  mutate(fe_afb_20 = factor(fe_afb_20, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_afb_20 = set_label(fe_afb_20, label = "First birth by age 20")) %>%
  mutate(fe_afb_22 = case_when(
    v012 >= 22 & (is.na(v212) | (v212 >= 22 & v212 < 50)) ~ 0,
    v012 >= 22 & (v212 >= 0 & v212 < 22) ~ 1)) %>%
  mutate(fe_afb_22 = factor(fe_afb_22, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_afb_22 = set_label(fe_afb_22, label = "First birth by age 22")) %>%
  mutate(fe_afb_25 = case_when(
    v012 >= 25 & (is.na(v212) | (v212 >= 25 & v212 < 50)) ~ 0,
    v012 >= 25 & (v212 >= 0 & v212 < 25) ~ 1)) %>%
  mutate(fe_afb_25 = factor(fe_afb_25, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_afb_25 = set_label(fe_afb_25, label = "First birth by age 25")) %>%
  mutate(fe_birth_never = case_when(
    v201 == 0 ~ 1,
    v201 != 0 ~ 0)) %>%
  mutate(fe_birth_never = factor(fe_birth_never, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(fe_birth_never = set_label(fe_birth_never, label = "Never had a birth"))

table_temp1 = IRdata %>%
  calc_cro_cpct(
    cell_vars = list(fe_afb_15,fe_afb_18,fe_afb_20,fe_afb_22,fe_afb_25,fe_birth_never),
    col_vars = list(v013, total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Percent had first birth by specific ages, by age group")


table_temp2 = IRdata %>%
  filter(v013 >= 2) %>% 
  tab_cells(fe_afb_15,fe_afb_18,fe_afb_20,fe_afb_22,fe_afb_25,fe_birth_never) %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Percent had first birth by specific ages, among 20-49 year olds")

table_temp3 = IRdata %>%
  filter(v013 >= 3) %>% 
  tab_cells(fe_afb_15,fe_afb_18,fe_afb_20,fe_afb_22,fe_afb_25,fe_birth_never) %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Percent had first birth by specific ages, among 25-49 year olds")

write.xlsx(table_temp1, "Tables_FE.xlsx", sheetName = "First_birth",append=TRUE) 
write.xlsx(table_temp2, "Tables_FE.xlsx", sheetName = "First_birth_20_49",append=TRUE) 
write.xlsx(table_temp3, "Tables_FE.xlsx", sheetName = "First_birth_25_49",append=TRUE) 


#Median age at first birth by background variables ##########################################################################
#############################################################################################################################

# TABLE 5.10

IRdata2 <- IRdata %>% 
  mutate(age = case_when(
    !is.na(v212) ~ v212,
    TRUE ~ 99)) 

Age_groups <- list(c(15, 19), c(20, 24), c(25, 29), c(30, 34), c(35, 39), c(40, 44), c(45, 49), c(20, 49), c(25, 49))
temp_national <- vector("list", 9)
temp_data <- vector("list", 9)

# NATIONAL MEDIANS 

for (i in seq_along(Age_groups)) {
  
  IRdata2_ages <- IRdata2 %>% 
    filter(v012 >= Age_groups[[i]][1] & v012 <= Age_groups[[i]][2])
  
  dummy.median <- IRdata2_ages %>% 
    summarise(weighted_median = weightedMedian(age, wt, na.rm=TRUE))
  IRdata2_ages$weighted_median <- dummy.median$weighted_median
  
  IRdata2_ages$dummy <- ifelse(IRdata2_ages$age<IRdata2_ages$weighted_median , 1, 0)
  
  dummy.mean <- IRdata2_ages %>%
    summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
  IRdata2_ages$sL <- dummy.mean$sL
  
  IRdata2_ages$dummy <- ifelse(IRdata2_ages$age<=IRdata2_ages$weighted_median , 1, 0)
  
  dummy.mean <- IRdata2_ages %>%
    summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
  IRdata2_ages$sU <- dummy.mean$sU
  
  IRdata2_ages <- IRdata2_ages %>%
    mutate(smedian = round((weighted_median+(0.5-sL)/(sU-sL)),2))
  
  temp_national[[i]] <- IRdata2_ages %>%
    summarise(MEDIANS = mean(smedian, na.rm=TRUE))
  
  temp_national[[i]]$class = 0
  temp_national[[i]] = apply_labels(temp_national[[i]],
                                    class = c("National" = 0))
  
  temp_national[[i]]$age_group = paste0(Age_groups[[i]][1], "-", Age_groups[[i]][2])
} 
temp_national


# SUBPOPULATIONS MEDIANS #####################################################################
MEDIANAGES <- function(Class) {
  
  IRdata2$class <- IRdata2[[Class]]
  
  for (i in seq_along(Age_groups)) {
    
    IRdata2_ages <- IRdata2 %>% 
      filter(v012 >= Age_groups[[i]][1] & v012 <= Age_groups[[i]][2])
    
    dummy.median <- IRdata2_ages %>% 
      group_by(class) %>%
      summarise(weighted_median = weightedMedian(age, wt, na.rm=TRUE))
    IRdata2_ages <- left_join(IRdata2_ages, dummy.median, by = 'class')
    
    IRdata2_ages$dummy <- ifelse(IRdata2_ages$age<IRdata2_ages$weighted_median , 1, 0)
    
    dummy.mean <- IRdata2_ages %>%
      group_by(class) %>%
      summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
    IRdata2_ages <- left_join(IRdata2_ages, dummy.mean, by = 'class')
    
    IRdata2_ages$dummy <- ifelse(IRdata2_ages$age<=IRdata2_ages$weighted_median , 1, 0)
    
    dummy.mean <- IRdata2_ages %>%
      group_by(class) %>%
      summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
    IRdata2_ages <- left_join(IRdata2_ages, dummy.mean, by = 'class')
    
    IRdata2_ages <- IRdata2_ages %>%
      mutate(smedian = round((weighted_median+(0.5-sL)/(sU-sL)),2))
    
    temp_data[[i]] <- IRdata2_ages %>%
      group_by(class) %>%
      summarise(MEDIANS = mean(smedian, na.rm=TRUE))
   
    temp_data[[i]]$age_group = paste0(Age_groups[[i]][1], "-", Age_groups[[i]][2])

  } 
  return(temp_data)
}

# change [[i]] for relevant age group: [[9]] for 25-49
M1 <- (MEDIANAGES(Class = "residence" )[[9]])
M2 <- (MEDIANAGES(Class = "region" )[[9]])
M3 <- (MEDIANAGES(Class = "education" )[[9]])
M4 <- (MEDIANAGES(Class = "wealth" )[[9]])

M0 <- (temp_national[[9]])

write.xlsx(M0, "Tables_FE.xlsx", sheetName = "fe_mafb_25_national",append=TRUE)
write.xlsx(M1, "Tables_FE.xlsx", sheetName = "fe_mafb_25_residence",append=TRUE)
write.xlsx(M2, "Tables_FE.xlsx", sheetName = "fe_mafb_25_region",append=TRUE)
write.xlsx(M3, "Tables_FE.xlsx", sheetName = "fe_mafb_25_education",append=TRUE)
write.xlsx(M4, "Tables_FE.xlsx", sheetName = "fe_mafb_25_wealth",append=TRUE)

#########################################################################################################################################
###############################################################################################################################################################################
