# ******************************************************************************
# Program: 			  FE_MEDIANS.R
# Purpose: 		    Creates the indicators for fe_amenorrhea, postpartum fe_abstinence, and pp fe_insusceptibility fertility  
# Data inputs: 		KR survey list
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified: March 16 2021 by Mahmoud Elkasabi
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Variables created in this file:
#
#fe_amen			"Births for which the mother is Amenorrhoeic"
#fe_abst		  "Births for which the mother is Abstaining"
#fe_insusc		"Births for which the mother is Insusceptible"

#################################################################################

krdata <- KRdata %>% 
  select(starts_with("v0"), starts_with("b"),v405, v406, v106, v190, wt)

if ("b19" %in% names(krdata)) {
  krdata$months_since_birth = krdata$b19
} else {
  krdata$months_since_birth = krdata$v008-krdata$b3 
}

krdata <- krdata %>% 
  mutate(group2 = ifelse(months_since_birth <= 35,  1+as.integer(months_since_birth/2), NA )) %>%
  filter(!is.na(group2)) %>%
  mutate(group2 = factor(group2, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), 
                         labels = c("0-1", "2-3", "4-5", "6-7", "8-9", "10-11", "12-13", "14-15", "16-17", "18-19",
                                    "20-21", "22-23", "24-25", "26-27", "28-29", "30-31", "32-33", "34-35")))

krdata2 <- krdata %>% 
  mutate(n_tuple = 1) %>%
  group_by(v001, v002, v003, months_since_birth) %>%
  summarise(n_tuple = sum(n_tuple, na.rm=TRUE))

krdata <- left_join(krdata, krdata2, by = c("v001", "v002", "v003", "months_since_birth"))

krdata <- krdata %>% 
  filter(b0 == 0 | b0 == n_tuple) %>%
  mutate(fe_amen = case_when(
    bidx==1 & v405==1 ~ 1,      
    TRUE ~ 0)) %>%
  mutate(fe_abst = case_when(
    bidx==1 & v406==1 ~ 1,      
    TRUE ~ 0)) %>%
  mutate(fe_insusc = case_when(
    fe_amen>0 | fe_abst>0 ~ 1,      
    TRUE ~ 0))


# TABLE 5.6

# Essential for tables with means - ideally labels should be assigned for regions too
krdata <- expss::apply_labels(
  krdata,
  fe_amen = c("No" = 0, "Yes" = 1),
  fe_abst = c("No" = 0, "Yes" = 1),
  fe_insusc = c("No" = 0, "Yes" = 1)
)

IRdata <- expss::apply_labels(
  IRdata,
  v013 = c("15-19" = 1, "20-24" = 2, "25-29" = 3, "30-34" = 4, "35-39" = 5, "40-44" = 6, "45-49" = 7),
  v201 = "|"
)

table_temp = krdata %>%
  calc_cro_rpct(
    cell_vars = list(group2, total()),
    col_vars = list(fe_amen,fe_abst,fe_insusc),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Postpartum fe_amenorrhoea, fe_abstinence, and fe_insusceptibility")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "Postpartum",append=TRUE)


# TABLE 5.6
# Postpartum Means

# Change DHS phase

WIDTH1 <- ifelse(DHS_PHASE <=6,2,1)

krdata_fe_amen <- krdata %>% 
  group_by(group2) %>%
  summarise(fe_amen = weighted.mean(fe_amen, wt, na.rm=TRUE)) %>% 
  mutate(width = case_when(
    DHS_PHASE<=6 & group2==1 ~ 0.75,      
    DHS_PHASE<=6 & group2==2 ~ 1.5,      
    DHS_PHASE<=6 & group2==3 ~ 1.75,      
    DHS_PHASE<=6 & (group2 != 1 & group2 != 2 & group2 != 3) ~ 2,
    DHS_PHASE>=7 ~ 2)) %>%
  mutate(MEAN = fe_amen * width)  %>%
  summarize(fe_amenorrhoeic = sum(MEAN) + WIDTH1)  

krdata_fe_abst <- krdata %>% 
  group_by(group2) %>%
  summarise(fe_abst = weighted.mean(fe_abst, wt, na.rm=TRUE)) %>% 
  mutate(width = case_when(
    DHS_PHASE<=6 & group2==1 ~ 0.75,      
    DHS_PHASE<=6 & group2==2 ~ 1.5,      
    DHS_PHASE<=6 & group2==3 ~ 1.75,      
    DHS_PHASE<=6 & (group2 != 1 & group2 != 2 & group2 != 3) ~ 2,
    DHS_PHASE>=7 ~ 2)) %>%
  mutate(MEAN = fe_abst * width)  %>%
  summarize(fe_abstaining = sum(MEAN) + WIDTH1)  

krdata_fe_insusc <- krdata %>% 
  mutate(fe_insusc = case_when(
    fe_amen>0 | fe_abst>0 ~ 1,      
    TRUE ~ 0)) %>%
  group_by(group2) %>%
  summarise(fe_insusc = weighted.mean(fe_insusc, wt, na.rm=TRUE)) %>% 
  mutate(width = case_when(
    DHS_PHASE<=6 & group2==1 ~ 0.75,      
    DHS_PHASE<=6 & group2==2 ~ 1.5,      
    DHS_PHASE<=6 & group2==3 ~ 1.75,      
    DHS_PHASE<=6 & (group2 != 1 & group2 != 2 & group2 != 3) ~ 2,
    DHS_PHASE>=7 ~ 2)) %>%
  mutate(MEAN = fe_insusc * width)  %>%
  summarize(fe_insusceptible = sum(MEAN) + WIDTH1)  

Postpartum_mean <- as.data.frame(cbind(krdata_fe_amen,krdata_fe_abst,krdata_fe_insusc))

write.xlsx(Postpartum_mean, "Tables_FE.xlsx", sheetName = "Postpartum_Means",append=TRUE)

################################################################################################################################################################
