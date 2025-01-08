# /*****************************************************************************************************
# Program: 			CH_tables.R
# Purpose: 			produce tables for chapter 10 - Child health
# Author:				Shireen Assaf
# Date last modified: August 17, 2022 by Shireen Assaf
# 
# *This do file will produce the following tables in excel:
#  1. Tables_Size:		  Contains the tables for child's size indicators
#  2.	Tables_ARI_FV.xls:Contains the tables for ARI and fever indicators
#  3.	Tables_DIAR.xls:  Contains the tables for diarrhea indicators, mother's knowledge of ORS, and stool disposal.
# 	Note: these tabouts do not include the watsan indicators (source of drinking water and type of 
# 	toilet facility). For these indicators please use the PH_SANI.do and the PH_WATER.do files from Chapter 2 
# 	which will produce the watsan indicators using a PR file. 
# 	The PR file then needs to be merged with the coded KR file with the diarrhea indicators to include them in the tabulations. 

#  4. Tables_VAC: Contains the tables for child's vaccination indicators        
  # Notes:	These tables will be produced for the age group selection in the CH_VAC.do file. 
  # The default section is children 12-23 months. If estimates are required for children 24-35 months, 
  # the CH_VAC.do file needs to be run again with that age group selection and then this do file to produce files
  # can be run again. 
# *****************************************************************************************************/
# 
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# age of child. If b19 is not available in the data use v008 - b3
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

# Non-standard background variable to add to tables

# Age in months
KRdata <- KRdata %>%
  mutate(agecats = case_when(age<6~ 1, age>=6 & age<=11~ 2, age>=12&age<=23~ 3, 
                               age>=24&age<=35~ 4, age>=36&age<=47~ 5, age>=48&age<=59~ 6)) %>%
  set_value_labels(agecats = c("<6"=1, "6-11"=2, "12-23"=3, "24-35"=4, "36-47"=5, "48-59"=6 )) %>%
  set_variable_labels(agecats = "Age of child in months categories: 0-59")

# Mother's age at birth (years)
KRdata <- KRdata  %>%
  mutate(months_age = b3-v011) %>%
  mutate(mo_age_at_birth =
           case_when(
             months_age < 20*12   ~ 1 ,
             months_age >= 20*12 & months_age < 35*12 ~ 2,
             months_age >= 35*12 &  months_age < 50*12 ~ 3)) %>%
  mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-34", "Mother's age at birth 35-49"))) %>%
  mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) 

# Birth order
KRdata <- KRdata  %>%
  mutate(birth_order =
           case_when(
             bord == 1  ~ 1,
             bord >= 2 & bord <= 3 ~ 2,
             bord >= 4 & bord <= 5 ~ 3,
             bord >= 6  ~ 4,
             bord == NA ~ 99)) %>%
  replace_with_na(replace = list(birth_order = c(99))) %>%
  mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-5","Birth order:6+"))) %>%
  mutate(birth_order = set_label(birth_order, label = "Birth order"))  

# Mother's smoking status
KRdata <- KRdata  %>%
  mutate(smoke =
           case_when(
             v463a==1 | v463b==1 | v463e==1 | v463f==1 | v463g==1 | v463j==1 | v463k==1 | v463l==1 | v463x==1 ~ 1,
             TRUE ~ 2)) %>%
  set_value_labels(smoke = c("Smokes cigarrettes/tobacco"=1, "Does not smoke"=2))

#Cooking fuel
KRdata <- KRdata  %>%
  mutate(fuel = 
           case_when(v161<=4 ~ 1, 
                     v161==5 ~ 2, 
                     v161==6 ~ 3, 
                     v161==7 ~ 4, 
                     v161%in%c(8,9,10) ~ 5,
                     v161==11 ~ 6,
                     v161==95 ~ 8,
                     v161==96 ~ 7, 
                     v161>=97 ~ 99)) %>%
  replace_with_na(replace = list(fuel = c(99))) %>%
  set_value_labels(fuel = c("electricity/gas" = 1, "kerosene"=2, "coal"=3, "charcoal"=4, "wood/straw/grass/crop"=5,
                            "animal dung"=6, "other"=7, "no food cooked in house"=8)) %>%
  set_variable_labels(fuel = "cooking fuel")


# **************************************************************************************************
# Tables for child's size variables
# **************************************************************************************************
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list( mo_age_at_birth, birth_order, smoke, v025, v106, v024, v190, total()),
    col_vars = list(ch_size_birth, ch_report_bw, ch_below_2p5),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Child's size tables")
write.xlsx(table_temp, "Chap10_CH/Tables_Size.xls", sheetName = "Size_birthweight", append=TRUE)

# **************************************************************************************************
# Tables for ARI and fever indicators
# **************************************************************************************************
# Note: Care and soure of treatment depend on the country specific definitions. Check the 
# footnote in the final report for these tables and make adjustments in the CH_AR_FV file. 

table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list( agecats, b4, smoke, fuel, v025, v106, v024, v190, total()),
    col_vars = list(ch_ari, ch_ari_care, ch_ari_care_day),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("ARI and fever tables")
write.xlsx(table_temp, "Chap10_CH/Tables_ARI_FV.xls", sheetName = "ARI", append=TRUE)

table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(total()),
    col_vars = list(ch_ari_govh, ch_ari_govh_trt, ch_ari_govcent,ch_ari_govcent_trt, ch_ari_pclinc, ch_ari_pclinc_trt,
                    ch_ari_pdoc, ch_ari_pdoc_trt, ch_ari_pharm, ch_ari_pharm_trt),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("ARI and fever tables")
write.xlsx(table_temp, "Chap10_CH/Tables_ARI_FV.xls", sheetName = "ARI_source_treatment", append=TRUE)

table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list( agecats, b4, v025, v106, v024, v190, total()),
    col_vars = list(ch_fever, ch_fev_care, ch_fev_care_day, ch_fev_antib),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("ARI and fever tables")
write.xlsx(table_temp, "Chap10_CH/Tables_ARI_FV.xls", sheetName = "Fever", append=TRUE)


# **************************************************************************************************
# Tables for diarrhea indicators
# **************************************************************************************************
table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list( agecats, b4, v025, v106, v024, v190, total()),
    col_vars = list(ch_diar, ch_diar_care),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Diarrhea tables")
write.xlsx(table_temp, "Chap10_CH/Tables_DIAR.xls", sheetName = "Diarrhea", append=TRUE)

table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list( agecats, b4, v025, v106, v024, v190, total()),
    col_vars = list(ch_diar_liq, ch_diar_food),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Diarrhea tables")
write.xlsx(table_temp, "Chap10_CH/Tables_DIAR.xls", sheetName = "Feeding", append=TRUE)

table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list( agecats, b4, v025, v106, v024, v190, total()),
    col_vars = list(ch_diar_ors, ch_diar_rhf, ch_diar_ors_rhf, ch_diar_zinc, ch_diar_zinc_ors, ch_diar_ors_fluid,
                    ch_diar_ort, ch_diar_ort_feed, ch_diar_antib, ch_diar_antim, ch_diar_intra, ch_diar_other, ch_diar_notrt),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Diarrhea tables")
write.xlsx(table_temp, "Chap10_CH/Tables_DIAR.xls", sheetName = "ORS", append=TRUE)

table_temp <-  KRdata %>% 
  cross_rpct(
    cell_vars = list(total()),
    col_vars = list(ch_diar_govh, ch_diar_govh_trt, ch_diar_govh_ors, 
                    ch_diar_govcent,ch_diar_govcent_trt,ch_diar_govcent_ors, 
                    ch_diar_pclinc, ch_diar_pclinc_trt, ch_diar_pclinc_ors, 
                    ch_diar_pdoc, ch_diar_pdoc_trt, ch_diar_pdoc_ors, 
                    ch_diar_pharm, ch_diar_pharm_trt, ch_diar_pharm_ors),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Diarrhea tables")
write.xlsx(table_temp, "Chap10_CH/Tables_DIAR.xls", sheetName = "Source_treatment", append=TRUE)

table_temp <-  IRdata %>% 
  cross_rpct(
    cell_vars = list(v013, v025, v024, v106, v190, total()),
    col_vars = list(ch_know_ors),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Diarrhea tables")
write.xlsx(table_temp, "Chap10_CH/Tables_DIAR.xls", sheetName = "mothers_know_ors", append=TRUE)

table_temp <-  KRstool %>% 
  cross_rpct(
    cell_vars = list( agecats, v025, v106, v024, v190, total()),
    col_vars = list(ch_stool_dispose, ch_stool_safe),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Diarrhea tables")
write.xlsx(table_temp, "Chap10_CH/Tables_DIAR.xls", sheetName = "Disposal_stool", append=TRUE)

# **************************************************************************************************
#  Vaccinations tables
# **************************************************************************************************

# Birth order
KRvac <- KRvac  %>%
  mutate(birth_order =
           case_when(
             bord == 1  ~ 1,
             bord >= 2 & bord <= 3 ~ 2,
             bord >= 4 & bord <= 5 ~ 3,
             bord >= 6  ~ 4,
             bord == NA ~ 99)) %>%
  replace_with_na(replace = list(birth_order = c(99))) %>%
  mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-5","Birth order:6+"))) %>%
  mutate(birth_order = set_label(birth_order, label = "Birth order"))  

# vaccines by source
table_temp <-  KRvac %>% 
  cross_rpct(
    cell_vars = list(total()),
    col_vars = list(ch_bcg_card, ch_bcg_moth, ch_bcg_either, ch_pent1_card, ch_pent1_moth, ch_pent1_either,
                      ch_pent2_card, ch_pent2_moth,	ch_pent2_either,	ch_pent3_card, ch_pent3_moth, ch_pent3_either	,
                      ch_polio0_card, ch_polio0_moth, ch_polio0_either, ch_polio1_card, ch_polio1_moth, ch_polio1_either,
                      ch_polio2_card, ch_polio2_moth, ch_polio2_either, ch_polio3_card, ch_polio3_moth, ch_polio3_either ,
                      ch_pneumo1_card, ch_pneumo1_moth, ch_pneumo1_either, ch_pneumo2_card, ch_pneumo2_moth, ch_pneumo2_either ,
                      ch_pneumo3_card, ch_pneumo3_moth, ch_pneumo3_either, ch_rotav1_card, ch_rotav1_moth, ch_rotav1_either ,
                      ch_rotav2_card, ch_rotav2_moth, ch_rotav2_either, ch_rotav3_card, ch_rotav3_moth, ch_rotav3_either ,
                      ch_meas_card, ch_meas_moth, ch_meas_either, ch_allvac_card, ch_allvac_moth ,ch_allvac_either ,
                      ch_novac_card, ch_novac_moth, ch_novac_either),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("vaccination tables")
write.xlsx(table_temp, "Chap10_CH/Tables_VAC.xls", sheetName = "vaccine_source", append=TRUE)

table_temp <-  KRvac %>% 
  cross_rpct(
    cell_vars = list( b4, birth_order, ch_card_seen, v025, v024,v106, v190, total()),
    col_vars = list(ch_bcg_either, ch_pent1_either, ch_pent2_either, ch_pent3_either,
                    ch_polio0_either, ch_polio1_either, ch_polio2_either, ch_polio3_either,
                    ch_pneumo1_either, ch_pneumo2_either, ch_pneumo3_either,
                    ch_rotav1_either, ch_rotav2_either, ch_rotav3_either,
                    ch_meas_either, ch_allvac_either, ch_novac_either ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("vaccination tables")
write.xlsx(table_temp, "Chap10_CH/Tables_VAC.xls", sheetName = "backgroundvars", append=TRUE)

table_temp <-  KRvac %>% 
  cross_rpct(
    cell_vars = list( b4, birth_order, v025, v024,v106, v190, total()),
    col_vars = list(ch_card_ever_had,ch_card_seen ),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("vaccination tables")
write.xlsx(table_temp, "Chap10_CH/Tables_VAC.xls", sheetName = "card", append=TRUE)
