# ******************************************************************************
# Program: 		ML_NETS_access.R
# Purpose: 		Code for POPULATION ACCESS TO ITNS and POPULATION/CHILD/PREGNANT WOMEN USE OF ITNS AMONG HH WITH ITNs  
# Data inputs: 		HR and PR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and modified by Shireen Assaf for the code share project - translated to R by Mahmoud Elkasabi
# Date last modified: September 5, 2024 by Courtney Allen
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_pop_access		"Population with access to an ITN"
# ml_slept_itn_hhitn	"Slept under an ITN last night amound household population with at least 1 ITN"
# -----------------------------------------------------------------------------#

# Number of ITNs per household
HRdata <- HRdata %>%
  mutate(itnhh_01 = case_when(hml10_1==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_02 = case_when(hml10_2==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_03 = case_when(hml10_3==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_04 = case_when(hml10_4==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_05 = case_when(hml10_5==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_06 = case_when(hml10_6==1 ~ 1,TRUE ~ 0)) %>%
  mutate(itnhh_07 = case_when(hml10_7==1 ~ 1,TRUE ~ 0)) %>%
  mutate(ml_numitnhh = itnhh_01 + itnhh_02 + itnhh_03 + itnhh_04 + itnhh_05 + itnhh_06 + itnhh_07,
         ml_numitnhh = set_label(ml_numitnhh, label = "Number of ITNs per household"))

HRdata_ <- HRdata %>% 
  select(hv001, hv002, ml_numitnhh, starts_with("hml10"))

HRmerge <- merge(HRdata_,
                 PRdata, by = c("hv001","hv002"))


# Households with > 1 ITN per 2 members
# Potential users divided by defacto household members is greater or equal to one
HRmerge <- HRmerge %>%
  mutate(ml_potuse = ml_numitnhh*2,
         ml_potuse = set_label(ml_potuse, label = "Potential ITN users in household"))

HRmerge <- HRmerge %>%
  mutate(ml_pop_access0 =ml_potuse/hv013) %>%
  mutate(ml_pop_access = case_when(
    ml_pop_access0 >= 1   ~ 1,
    TRUE   ~ ml_pop_access0),
    ml_pop_access = set_label(ml_pop_access, label = "Population with access to an ITN"))


HRmerge <- HRmerge %>%
  mutate(wt = hv005/1000000)

# number of de-facto persons
HRmerge <- HRmerge %>%
  mutate(numdefacto = case_when(hv013>8 ~ 8,TRUE ~ hv013)) 

# Number of ITNs by number of persons who stayed in the household the night before the survey
table_temp <-  HRmerge %>% 
  filter(hv103==1) %>%
  cross_rpct(
    cell_vars = list(numdefacto),
    col_vars = list(ml_numitnhh),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Number of ITNs by number of persons")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_itn_bynumbperson",append=TRUE)

# Percent of population with access to an ITN by background variables and number of persons who stayed in the household the night before the survey
table_temp = HRmerge %>%
  filter(hv103==1) %>%
  tab_cells(ml_pop_access) %>%
  tab_rows(numdefacto,hv025, hv024, hv270,total()) %>% 
  tab_weight(wt) %>% 
  tab_stat_fun(Mean = w_mean, "Weighted N" = w_n, method = list) %>%
  tab_pivot() %>% 
  tab_caption("population with access to an ITN")
table_temp$`#Total|Mean` <- 100* table_temp$`#Total|Mean`

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_pcnt_access",append=TRUE)

# Categorizing nets
HRmerge <- HRmerge %>%
  mutate(ml_netcat = case_when(
    hml12==0  ~ 0,
    hml12==1|hml12==2  ~ 1,
    hml12==3 ~ 2),
    ml_netcat = set_label(ml_netcat, label = "Mosquito net categorization"))

# Slept under an ITN last night among households with at least 1 ITN
HRmerge <- HRmerge %>%
  mutate(ml_slept_itn_hhitn = case_when((hml10_1==1|hml10_2==1|hml10_3==1|hml10_4==1|hml10_5==1|hml10_6==1|hml10_7==1) & ml_netcat==1 ~ 1,
                                        (hml10_1==1|hml10_2==1|hml10_3==1|hml10_4==1|hml10_5==1|hml10_6==1|hml10_7==1) & ml_netcat!=1 ~ 0),
         ml_slept_itn_hhitn = set_label(ml_slept_itn_hhitn, label = "Slept under an ITN last night amound household population with at least 1 ITN"))

# recode age
HRmerge <- HRmerge %>%
  mutate(age = case_when(
    hv105 %in%  (0:4) ~ 1,
    hv105 %in%  (5:14) ~ 2,
    hv105 %in%  (15:34) ~ 3,
    hv105 %in%  (35:49) ~ 4,
    hv105 %in%  (50:95) ~ 5,
    hv105 %in%  (96:99) ~ 99),
    age = add_labels(age, labels = c("0-4"=1, "5-14"=2,"15-34"=3, "35-49"=4,"50+"=5)),
    age = set_label(age, label = "Age"))%>%
  replace_with_na(replace = list(age = c(99)))

# Slept under an ITN last night among households with at least 1 ITN
table_temp <-  HRmerge %>% 
  filter(hv103==1) %>%
  cross_rpct(
    cell_vars = list(age, hv104, hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn_hhitn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Slept under an ITN last night among households with at least 1 ITN")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_slept_1itn",append=TRUE)

# Slept under an ITN last night among households with at least 1 ITN - Children under 5
table_temp <-  HRmerge %>% 
  filter(hv103==1 & hml16<5) %>%
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn_hhitn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Slept under an ITN last night among households with at least 1 ITN - Children under 5")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_chldslept_1itn",append=TRUE)


# Slept under an ITN last night among households with at least 1 ITN - Pregnant women age 15-49
table_temp <-  HRmerge %>% 
  filter(hv103==1 & hv104==2 & hml18==1 & hml16>=15 & hml16<=49) %>%
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_slept_itn_hhitn),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Slept under an ITN last night among households with at least 1 ITN - Pregnant women age 15-49")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "pr_prgwslept_1itn",append=TRUE)

