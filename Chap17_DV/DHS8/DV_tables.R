# /*****************************************************************************************************
# Program: 			DV_tables.R
# Purpose: 			produce tables for domestic violence indicators
# Author:				Shireen Assaf
# Date last modified: September 29 2021 by Shireen Assaf
# 
# *This do file will produce the following tables in excel:
# 1. 	Tables_DV_viol:		Contains the tables for indicators of experiencing violence ever and help seeking
# 2. 	Tables_DV_cntrl:	Contains the tables for marital control indicators
# 3. 	Tables_DV_prtnr:	Contains the tables for indicators of experiencing violence by their partners ever and help seeking
# 
# Notes: 	The indicators are outputed for women age 15-49 in line 21. This can be commented out if the indicators are required for all women.	
# *****************************************************************************************************/

#****************************************************
# COVARIATES

# //marital group of never married and ever married
IRdata <- IRdata %>%
  mutate(marital_2 =
           case_when(v502==0 ~ 1, v502 %in% c(1,2) ~ 2 )) %>% 
  set_value_labels(marital_2 = c("never married" = 1, "ever married"=2)) %>%
  set_variable_labels(marital_2 = "Ever or never married")

# //duration of marriage
IRdata <- IRdata %>%
  mutate(mar_years =
           case_when(
             v512<=1 ~ 1,
             v512 %in% c(2,3,4) ~ 2,
             v512 %in% c(5,6,7,8,9) ~ 3,
             v512>=10 & v512<=50 ~ 4 )) %>% 
  set_value_labels(mar_years = c("<2" = 1, "2-4"=2, "5-9"=3, "10+"=4)) %>%
  set_variable_labels(mar_years = "Years since first cohabitation")

# //DV age groups
IRdata <- IRdata %>%
  mutate(dv_age =
           case_when(
             v012<20 ~ 1,
             v012>=20 & v012<=24 ~ 2 ,
             v012>=25 & v012<=29 ~ 3 , 
             v012>=30 & v012<=39 ~ 4 , 
             v012>=40 & v012<=49 ~ 5 )) %>% 
  set_value_labels(dv_age = c("15-19" = 1, "20-24"=2, "25-29"=3, "30-39"=4, "40-49"=5)) %>%
  set_variable_labels(dv_age = "Age groups")

# //living children
IRdata <- IRdata %>%
  mutate(livingchild =
           case_when(
             v218==0 ~ 0,
             v218 %in% c(1,2) ~ 1 ,
             v218 %in% c(3,4) ~ 2 , 
             v218>=5 & v218<98 ~ 3 , 
             v218>=98 ~ 9 )) %>% 
  set_value_labels(livingchild = c("0"=0, "1-2" = 1, "3-4"=2, "5+"=3, "DK/missing"=9)) %>%
  set_variable_labels(livingchild = "Living children")

# //work status and type of earnings 
IRdata <- IRdata %>%
  mutate(work =
           case_when(
             (v741==1 | v741==2) ~ 1 ,
             (v741==0 | v741==3) ~ 2 , 
             v741==9 ~ 9 , 
             TRUE ~ 0 )) %>% 
  set_value_labels(work = c("not employed"=0, "earns cash" = 1, "does not earn cash"=2, "missing type of earnings"=9)) %>%
  set_variable_labels(work = "work status and type of earnings")

# //gen husband/wife education difference
IRdata <- IRdata %>%
  mutate(edu_diff =
           case_when(
             (v715>=98 | v133>=98) & v502==1 ~ 5,
             (v715==0 & v133==0 & v502==1) ~ 4,
             (v715==v133) & v502==1 ~ 3,
             (v715<v133) & v502==1 ~ 2,
             (v715>v133) & v502==1 ~ 1 )) %>% 
  set_value_labels(edu_diff = c("husband better educated"=1, "wife better educated"=2, "equally educated"=3, "neither educated"=4, "DK/missing"=5)) %>%
  set_variable_labels(edu_diff = "Spousal education difference")
 
# //gen husband/wife age difference
IRdata <- IRdata %>%
  mutate(age_diff_temp= (v730-v012)) %>%
  mutate(age_diff =
           case_when(
             age_diff_temp >=10   ~ 5,
             age_diff_temp %in% c(5,6,7,8,9)  ~ 4,
             age_diff_temp %in% c(1,2,3,4)  ~ 3,
             age_diff_temp==0   ~ 2,
             age_diff_temp <0   ~ 1,
             v502==0 | v502==2 ~ 99)) %>%
  replace_with_na(replace = list(age_diff = c(99))) %>%
  set_value_labels(age_diff = c("Wife older"=1, "Same age"=2, "Wife 1-4 yrs younger"=3, "Wife 5-9 yrs younger"=4, "Wife 10+ yrs younger"=5)) %>%
  set_variable_labels(age_diff = "Spousal age difference")

# //husband's alcohol consumption
IRdata <- IRdata %>%
  mutate(husb_drink =
           case_when(
             d113==0 ~ 0,
             d114==0 & d113==1 ~ 1 ,
             d114==2 & d113==1 ~ 2 , 
             d114==1 & d113==1 ~ 3 )) %>% 
  set_value_labels(husb_drink = c("doesn't drink"=0, "drinks, never drunk" = 1, "drinks, sometimes drunk"=2, "drinks, often drunk"=3)) %>%
  set_variable_labels(husb_drink = "husband's drinking habits")

# 	//number of decisions women participate in
IRdata <- IRdata %>%
  mutate(decision_a = case_when(v743a %in% c(1,2)~1, v743a %in% c(4,5,6) ~ 0)) %>%
  mutate(decision_b = case_when(v743b %in% c(1,2)~1, v743b %in% c(4,5,6) ~ 0)) %>%
  mutate(decision_d = case_when(v743d %in% c(1,2)~1, v743d %in% c(4,5,6) ~ 0)) %>%
  mutate(decisions = decision_a + decision_b + decision_d ) 
  IRdata[["decisions"]] <- ifelse(IRdata[["v502"]]!=1, NA, IRdata[["decisions"]]) 	  
  IRdata <- IRdata %>% 
  set_variable_labels(decisions = "Number of decisions in which women participate")
  
# 	//number of beatings for which wife beating is justified
  IRdata <- IRdata %>%
    mutate(beat_a = case_when(v744a==1~1, v744a %in% c(0,8) ~ 0)) %>%
    mutate(beat_b = case_when(v744b==1~1, v744b %in% c(0,8) ~ 0)) %>%
    mutate(beat_c = case_when(v744c==1~1, v744c %in% c(0,8) ~ 0)) %>%
    mutate(beat_d = case_when(v744d==1~1, v744d %in% c(0,8) ~ 0)) %>%
    mutate(beat_e = case_when(v744e==1~1, v744e %in% c(0,8) ~ 0)) %>%
    mutate(beating = beat_a + beat_b + beat_c + beat_d + beat_e ) %>%
    set_variable_labels(beating = "Number of reasons beating is justified")
  
#****************************************************
# * the total will show on the last row of each table.
# * comment out the tables or indicator section you do not want.
# ****************************************************
# 
# * limiting to women age 15-49
# drop if v012<15 | v012>49
# 
IRdata <- IRdata %>%
  mutate(dwt = d005/1000000)

# 	**************************************************************************************************
# 	* Indicators for physical violence: excel file DV_tables will be produced
# 	**************************************************************************************************
# //EXPERIENCE OF PHYSICAL VIOLENCE (Tables_DV_viol)
# 	//Experience of physical violence: ever, frequency, and during pregnancy
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_phy, dv_phy_12m, dv_phy_12m_f, dv_phy_preg),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Experience of physical violence: ever, frequency, and during pregnancy")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Physical", append=TRUE)
# 	**************************************************************************************************
# 	//Persons committing physical violence
table_temp <-  IRdata %>% 
  calc_cro_cpct(
    col_vars = list(marital_2, total()),
    cell_vars = list(dv_phy_hus_curr, dv_phy_hus_form, dv_phy_bf_curr, dv_phy_bf_form, dv_phy_father,
                    dv_phy_mother, dv_phy_sibling, dv_phy_bychild, dv_phy_other_rel , dv_phy_mother_inlaw,
                    dv_phy_father_inlaw, dv_phy_teacher, dv_phy_atwork, dv_phy_police, dv_phy_other ),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Person committing physical violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Person phy", append=TRUE)
# 	**************************************************************************************************
# //EXPERIENCE OF SEXUAL VIOLENCE
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_sex, dv_sex_12m),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Experience of sexual violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Sexual", append=TRUE)

# 	**************************************************************************************************
# 	//Persons committing sexual violence
table_temp <-  IRdata %>% 
  calc_cro_cpct(
    cell_vars = list(dv_sex_hus_curr, dv_sex_hus_form, dv_sex_bf, dv_sex_father, dv_sex_brother,
                    dv_sex_other_rel , dv_sex_inlaw, dv_sex_friend, dv_sex_friend_fam, dv_sex_teacher, 
                    dv_sex_atwork, dv_sex_relig, dv_sex_police, dv_sex_stranger, dv_sex_other, dv_sex_missing ),
    col_vars =  list(marital_2, total()),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Person committing sexual violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Person sex", append=TRUE)

# 	**************************************************************************************************
# 	//Age at first experienced sexual violence
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, marital_2, total()),
    col_vars = list(dv_sex_age_10, dv_sex_age_12, dv_sex_age_15, dv_sex_age_18, dv_sex_age_22 ),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Age at first experienced of sexual violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Age sexual violence", append=TRUE)

# 	**************************************************************************************************
# 	//Experience of different forms of violence
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, total()),
    col_vars = list(dv_phy_only, dv_sex_only, dv_phy_sex, dv_phy_sex_any),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Experience of different types of violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Type violence", append=TRUE)

# ********************************************************************************
# 
# //HELP SEEKING FOR THOSE EXPERIENCE VIOLENCE (Tables_DV_viol)
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_phy_only, dv_sex_only, dv_phy_sex, dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_help_seek),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Sought help")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Help", append=TRUE)

# //SOURCES OF HELP
table_temp <-  IRdata %>% 
  calc_cro_cpct(
    cell_vars = list(dv_help_fam, dv_help_hfam, dv_help_husb, dv_help_bf, dv_help_friend, dv_help_neighbor, dv_help_relig,
                     dv_help_doc, dv_help_police, dv_help_lawyer, dv_help_sw, dv_help_other),
    col_vars =  list(dv_viol_type, dv_phy_sex_any ),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Sources of help")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_viol.xls", sheetName = "Sources help", append=TRUE)
# **************************************************************************************************

# //MARITAL CONTROL (Tables_DV_cntrl)
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_prtnr_jeals, dv_prtnr_accus, dv_prtnr_friends, dv_prtnr_fam, 
                    dv_prtnr_where, dv_prtnr_money, dv_prtnr_cntrl_3, dv_prtnr_cntrl_0),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
    set_caption("Marital control")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_cntrl.xls", sheetName = "Marital control", append=TRUE)
# ********************************************************************************
# //TYPES OF SPOUSAL VIOLENCE (Tables_DV_prtnr)
# Partner physical violence
table_temp <-  IRdata %>% 
  calc_cro_tpct(
    cell_vars = list(dv_prtnr_phy, dv_prtnr_phy_12m, dv_prtnr_phy_12m_f, 
                     dv_prtnr_push , dv_prtnr_push_12m , dv_prtnr_push_12m_f,
                     dv_prtnr_slap , dv_prtnr_slap_12m , dv_prtnr_slap_12m_f, 
                     dv_prtnr_twist , dv_prtnr_twist_12m, dv_prtnr_twist_12m_f,
                     dv_prtnr_punch , dv_prtnr_punch_12m, dv_prtnr_punch_12m_f ,
                     dv_prtnr_kick , dv_prtnr_kick_12m, dv_prtnr_kick_12m_f ,
                     dv_prtnr_choke , dv_prtnr_choke_12m, dv_prtnr_choke_12m_f,
                     dv_prtnr_weapon , dv_prtnr_weapon_12m, dv_prtnr_weapon_12m_f),
    col_vars = list(total()),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Partner physial violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt phy", append=TRUE)

########
# partner sexual violence
table_temp <-  IRdata %>% 
  calc_cro_tpct(
    cell_vars = list(dv_prtnr_sex, dv_prtnr_sex_12m, dv_prtnr_sex_12m_f, 
                     dv_prtnr_force, dv_prtnr_force_12m, dv_prtnr_force_12m_f,
                     dv_prtnr_force_act, dv_prtnr_force_act_12m, dv_prtnr_force_act_12m_f,
                     dv_prtnr_threat_act , dv_prtnr_threat_act_12m, dv_prtnr_threat_act_12m_f ),
    col_vars = list(total()),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Partner sexual violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt sex", append=TRUE)

########
#partner emotional violence
table_temp <-  IRdata %>% 
  calc_cro_tpct(
    cell_vars = list(dv_prtnr_emot, dv_prtnr_emot_12m, dv_prtnr_emot_12m_f, 
                     dv_prtnr_humil, dv_prtnr_humil_12m, dv_prtnr_humil_12m_f,
                     dv_prtnr_threat , dv_prtnr_threat_12m, dv_prtnr_threat_12m_f,
                     dv_prtnr_insult, dv_prtnr_insult_12m, dv_prtnr_insult_12m_f),
    col_vars = list(total()),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Partner emotional violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt emotional", append=TRUE)
########
#combinations of violence (ever and frequency)
table_temp <-  IRdata %>% 
  calc_cro_tpct(
    cell_vars = list(dv_prtnr_phy_sex, dv_prtnr_phy_sex_12m, dv_prtnr_phy_sex_12m_f ,
                     dv_prtnr_phy_sex_emot, dv_prtnr_phy_sex_emot_12m, dv_prtnr_phy_sex_emot_12m_f, 
                     dv_prtnr_phy_sex_any, dv_prtnr_phy_sex_any_12m, dv_prtnr_phy_sex_any_12m_f ,
                     dv_prtnr_phy_sex_emot_any, dv_prtnr_phy_sex_emot_any_12m, dv_prtnr_phy_sex_emot_any_12m_f),
    col_vars = list(total()),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Combinations of violence")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt viol freq", append=TRUE)
########
#combinations of violence (ever by background vars)
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_prtnr_phy, dv_prtnr_sex, dv_prtnr_emot, dv_prtnr_phy_sex, dv_prtnr_phy_sex_emot,
                    dv_prtnr_phy_sex_any, dv_prtnr_phy_sex_emot_any),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Combinations of violence by background vars")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt viol byvars", append=TRUE)
	
# //violence by empowerment indicators
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v701, husb_drink, edu_diff, age_diff, dv_prtnr_cntrl_cat, decisions, beating, d121, d129, total()),
    col_vars = list(dv_prtnr_phy, dv_prtnr_sex, dv_prtnr_emot, dv_prtnr_phy_sex, dv_prtnr_phy_sex_emot,
                    dv_prtnr_phy_sex_any, dv_prtnr_phy_sex_emot_any),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Combinations of violence by empowerment vars")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt viol byempower", append=TRUE)
########
# 	//violence by any partner in last 12 months
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_aprtnr_phy_12m, dv_aprtnr_sex_12m, dv_aprtnr_emot_12m, dv_aprtnr_phy_sex_12m, 
                    dv_aprtnr_phy_sex_emot_12m, dv_aprtnr_phy_sex_any_12m, dv_aprtnr_phy_sex_emot_any_12m),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Violence by any partner by background vars")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Any prt viol byvars", append=TRUE)

# 	//violence by duration of marriage
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(mar_years, total()),
    col_vars = list(dv_mar_viol_0, dv_mar_viol_2, dv_mar_viol_5, dv_mar_viol_10),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Violence by years of marriage")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Prt viol by_yrs_marriage", append=TRUE)

#//injuries due to spousal violence
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_phy, dv_phy_12m, dv_sex, dv_sex_12m, dv_phy_sex_any, dv_phy_sex_any_12m, total()),
    col_vars = list(dv_prtnr_cuts, dv_prtnr_injury, dv_prtnr_broken, dv_prtnr_injury_any),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Injuries")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Injuries", append=TRUE)

# ********************************************************************************
# //INITIATE VIOLENCE AGAINST HUSBAND	
table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(dv_prtnr_phy, dv_prtnr_phy_12m, dv_age, v025, v024, v502, work, livingchild, v106, v190, total()),
    col_vars = list(dv_abusedhus_phy, dv_abusedhus_phy_12m ),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Violence against husband by background vars")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Violence agst husb by vars", append=TRUE)

table_temp <-  IRdata %>% 
  calc_cro_rpct(
    cell_vars = list(v701, husb_drink, edu_diff, age_diff, dv_prtnr_cntrl_cat, decisions, beating, d121, d129, total()),
    col_vars = list(dv_abusedhus_phy, dv_abusedhus_phy_12m ),
    weight = dwt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Violence against husband by empowerment vars")

write.xlsx(table_temp, "Chap17_DV/Tables_DV_prtnr.xls", sheetName = "Violence agst husb by empowr", append=TRUE)

