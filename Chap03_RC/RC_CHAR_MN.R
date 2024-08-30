# ******************************************************************************
# Program: 			  RC_CHAR_MN.R
# Purpose: 		    Code to compute respondent characteristics of men  
# Data inputs: 		MR dataset
# Data outputs:		coded variables
# Author:				  Mahmoud Elkasabi
# Date last modified: April 01 2021 by Mahmoud Elkasabi
# ******************************************************************************
#Please check the note on health insurance. This can be country specific and also reported for specific populations. 
#Please check the variables available for smoking and tobacco and see notes for these variables. Variable names have changed and these indicators are country specific.
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# rc_edu				"Highest level of schooling attended or completed"
# rc_edu_median		"Median years of education"
# rc_litr_cats		"Level of literacy"
# rc_litr				"Literate - higher than secondary or can read part or whole sentence"
# rc_media_newsp		"Reads a newspaper at least once a week"
# rc_media_tv			"Watches television at least once a week"
# rc_media_radio		"Listens to radio at least once a week"
# rc_media_allthree	"Accesses to all three media at least once a week"
# rc_media_none		"Accesses none of the three media at least once a week"
# rc_intr_ever		"Ever used the internet"
# rc_intr_use12mo		"Used the internet in the past 12 months"
# rc_intr_usefreq		"Internet use frequency in the past month - among users in the past 12 months"
# rc_empl				"Employment status"
# rc_occup			"Occupation among those employed in the past 12 months"
# rc_empl_type		"Type of employer among those employed in the past 12 months"
# rc_empl_earn		"Type of earnings among those employed in the past 12 months"
# rc_empl_cont		"Continuity of employment among those employed in the past 12 months"
# rc_hins_ss			"Health insurance coverage - social security"
# rc_hins_empl		"Health insurance coverage - other employer-based insurance"
# rc_hins_comm		"Health insurance coverage - mutual health org. or community-based insurance"
# rc_hins_priv		"Health insurance coverage - privately purchased commercial insurance"
# rc_hins_other		"Health insurance coverage - other type of insurance"
# rc_hins_any			"Have any health insurance"
# rc_tobc_cig			"Smokes cigarettes"
# rc_tobc_other		"Smokes other type of tobacco"
# rc_tobc_smk_any		"Smokes any type of tobacco"
# rc_smk_freq			"Smoking frequency"
# rc_cig_day			"Average number of cigarettes smoked per day"
# rc_tobc_snuffm		"Uses snuff smokeless tobacco by mouth"
# rc_tobc_snuffn		"Uses snuff smokeless tobacco by nose"
# rc_tobc_chew		"Chews tobacco"
# rc_tobv_betel		"Uses betel quid with tobacco"
# rc_tobc_osmkless	"Uses other type of smokeless tobacco"
# rc_tobc_anysmkless	"Uses any type of smokeless tobacco"
# rc_tobc_any			"Uses any type of tobacco - smoke or smokeless"
# -----------------------------------------------------------------------------#

MRdata <- MRdata %>%
  mutate(wt = mv005/1000000) %>%
  mutate(rc_edu = mv149,
  rc_edu = set_label(rc_edu, label = "Highest level of schooling attended or completed")) %>%
  mutate(eduyr = case_when(
    mv133<=20 ~ mv133, 
    mv133>20 & mv133<95 ~ 20, 
    mv133>95 | mv149>7 ~ 99)) %>%
  replace_with_na(replace = list(eduyr = c(99)))

MRdata <- MRdata %>%
  mutate(rc_litr_cats = case_when(
    !mv106 == 3 & mv155 == 2 ~ 1,
    !mv106 == 3 & mv155 == 1 ~ 2,
    !mv106 == 3 & mv155 == 0 ~ 3,
    !mv106 == 3 & mv155 == 3 ~ 4,
    !mv106 == 3 & mv155 == 4 ~ 5,
    mv106 == 3 ~ 0),
  rc_litr_cats = add_labels(rc_litr_cats, labels = c("Higher than secondary education"=0, "Can read a whole sentence"=1,
                                      "Can read part of a sentence"=2, "Cannot read at all"=3,
                                      "No card with required language"=4, "Blind/visually impaired"=5)),
  rc_litr_cats = set_label(rc_litr_cats, label = "Level of literacy")) %>%
  mutate(rc_litr = case_when(
    mv106==3 | mv155==1 | mv155==2 ~ 1, TRUE ~ 0),
  rc_litr = add_labels(rc_litr, labels = c("No"=0, "Yes"=1)),
  rc_litr = set_label(rc_litr, label = "Literate"))

# Media exposure
MRdata <- MRdata %>%
  mutate(rc_media_newsp = case_when(
    mv157 == 2 | mv157 == 3 ~ 1,
    mv157 == 0 | mv157 == 1 ~ 0),
  rc_media_newsp = add_labels(rc_media_newsp, labels = c("No"=0, "Yes"=1)),
  rc_media_newsp = set_label(rc_media_newsp, label = "Reads a newspaper at least once a week")) %>%
 
  mutate(rc_media_tv = case_when(
    mv159 == 2 | mv159 == 3 ~ 1,
    mv159 == 0 | mv159 == 1 ~ 0),
  rc_media_tv = add_labels(rc_media_tv, labels = c("No"=0, "Yes"=1)),
  rc_media_tv = set_label(rc_media_tv, label = "Watches television at least once a week")) %>%
  
  mutate(rc_media_radio = case_when(
    mv158 == 2 | mv158 == 3 ~ 1,
    mv158 == 0 | mv158 == 1 ~ 0),
  rc_media_radio = add_labels(rc_media_radio, labels = c("No"=0, "Yes"=1)),
  rc_media_radio = set_label(rc_media_radio, label = "Listens to radio at least once a week")) %>%
  
  mutate(rc_media_allthree = case_when(
    rc_media_newsp == 1 & rc_media_tv == 1 & rc_media_radio == 1  ~ 1,  TRUE ~ 0),
  rc_media_allthree = add_labels(rc_media_allthree, labels = c("No"=0, "Yes"=1)),
  rc_media_allthree = set_label(rc_media_allthree, label = "Accesses to all three media at least once a week")) %>%
  
  mutate(rc_media_none = case_when(
    rc_media_newsp == 0 & rc_media_tv == 0 & rc_media_radio == 0  ~ 1,
    TRUE ~ 0),
  rc_media_none = add_labels(rc_media_none, labels = c("No"=0, "Yes"=1)),
  rc_media_none = set_label(rc_media_none, label = "Accesses none of the three media at least once a week"))

if (!is.null(MRdata$mv171a)){
  
  MRdata <- MRdata %>%
    mutate(rc_intr_ever = case_when(
      mv171a == 1 | mv171a == 2 | mv171a == 3 ~ 1,
      mv171a == 0 ~ 0),
    rc_intr_ever = add_labels(rc_intr_ever, labels = c("No"=0, "Yes"=1)),
    rc_intr_ever = set_label(rc_intr_ever, label = "Ever used the internet")) %>%
    
    mutate(rc_intr_use12mo = case_when(
      mv171a == 1  ~ 1,
      mv171a == 0 | mv171a == 2 | mv171a == 3 ~ 0),
    rc_intr_use12mo = add_labels(rc_intr_use12mo, labels = c("No"=0, "Yes"=1)),
    rc_intr_use12mo = set_label(rc_intr_use12mo, label = "Used the internet in the past 12 months")) %>%
    
    mutate(rc_intr_usefreq = case_when(
      mv171a == 1  ~ mv171b, TRUE ~ 99))  %>%
    replace_with_na(replace = list(rc_intr_usefreq = c(99))) %>%
    mutate(rc_intr_usefreq = set_label(rc_intr_usefreq, label = "Internet use frequency in the past month - among users in the past 12 months"))
  
}

## Employment

MRdata <- MRdata %>%
  mutate(rc_empl = case_when(
    mv731 == 0 ~ 0,
    mv731 == 1 ~ 1,
    mv731 == 2 | mv731 == 3 ~ 2,
    mv731 == 8 ~ 9),
  rc_empl = add_labels(rc_empl, labels = c("Not employed in last 12 months"=0, 
                                 "Not curcently working but was employed in last 12 months"=1,
                                 "Curcently employed"=2,
                                 "Don't know/missing"=9)),
  rc_empl = set_label(rc_empl, label = "Employment status")) %>%
  
  mutate(emp = case_when(
    mv731 == 1 | mv731 == 2 | mv731 == 3  ~ 1,
    TRUE ~ 0)) %>%

  mutate(rc_occup = case_when(
    emp == 1 & mv717 == 1 ~ 1,
    emp == 1 & mv717 == 2 ~ 2,
    emp == 1 & (mv717 == 3 | mv717 == 7) ~ 3,
    emp == 1 & mv717 == 8 ~ 4,
    emp == 1 & mv717 == 9 ~ 5,
    emp == 1 & mv717 == 6 ~ 6,
    emp == 1 & (mv717 == 4 | mv717 == 5) ~ 7,
    emp == 1 & mv717>9 & mv717<98 ~ 8,
    emp == 1 & (mv717 == 98 | mv717 == 99 | is.na(mv717)) ~ 9 ),
  rc_occup = add_labels(rc_occup, labels = c("Professional"=1, 
                                 "Clerical"=2,
                                 "Sales and services"=3,
                                 "Skilled manual"=4,
                                 "Unskilled manual"=5, 
                                 "Domestic service"=6,
                                 "Agriculture"=7,
                                 "Other"=8,
                                 "Don't know/missing"=9)),
  rc_occup = set_label(rc_occup, label = "Occupation among those employed in the past 12 months")) %>%

  mutate(rc_empl_type = case_when(
    emp == 1 ~ mv719),
  rc_empl_type = set_label(rc_empl_type, label = "Type of employer among those employed in the past 12 months")) %>%
  
  mutate(rc_empl_earn = case_when(
    emp == 1 ~ mv741),
  rc_empl_earn = set_label(rc_empl_earn, label = "Type of earnings among those employed in the past 12 months")) %>%
  
  mutate(rc_empl_cont = case_when(
    emp == 1 ~ mv732),
  rc_empl_cont = set_label(rc_empl_cont, label = "Continuity of employment among those employed in the past 12 months")) %>%
  
  mutate(rc_agri = case_when(
    rc_occup == 7 ~ 1,
    !is.na(rc_occup) & !rc_occup==7 ~ 0),
  rc_agri = add_labels(rc_agri, labels = c("Non-Agriculture"=0, "Agriculture"=1)))

## Health insurance 
  
# Note: The different types of health insurance can be country specific. Please check the v481* variables to see which ones you need.
# In addition, some surveys report this for all women/men and some report it among those that have heard of insurance. Please check what the population of interest is for reporting these indicators.

MRdata <- MRdata %>%
  mutate(rc_hins_ss = mv481c,
  rc_hins_ss = set_label(rc_hins_ss, label = "Health insurance coverage - social security")) %>%
  mutate(rc_hins_empl = mv481b,
  rc_hins_empl = set_label(rc_hins_empl, label = "Health insurance coverage - other employer-based insurance")) %>%
  mutate(rc_hins_comm = mv481a,
  rc_hins_comm = set_label(rc_hins_comm, label = "Health insurance coverage - mutual health org. or community-based insurance")) %>%
  mutate(rc_hins_priv = mv481d,
  rc_hins_priv = set_label(rc_hins_priv, label = "Health insurance coverage - privately purchased commercial insurance")) %>%
  mutate(rc_hins_other = case_when(
    mv481e == 1 | mv481f == 1 | mv481g == 1 | mv481h == 1 | mv481x == 1 ~ 1,
    TRUE ~ 0),
  rc_hins_other = set_label(rc_hins_other, label = "Health insurance coverage - other type of insurance")) %>%
  mutate(rc_hins_any = case_when(
    mv481a == 1 | mv481b == 1 | mv481c == 1 | mv481d == 1 | mv481e == 1 | mv481f == 1 | mv481g == 1 | mv481h == 1 | mv481x == 1 ~ 1,
    TRUE ~ 0),
  rc_hins_any = set_label(rc_hins_any, label = "Have any health insurance"))


## Tobacco use
# please check all mv463* variables for types of smoking and tobacco use

  MRdata <- MRdata %>%
    mutate(rc_tobc_cig = case_when(
      (mv464a > 0 & mv464a <= 888) | (mv464b > 0 & mv464b <= 888) | (mv464c > 0 & mv464c <= 888) | 
      (mv484a > 0 & mv484a <= 888) | (mv484b > 0 & mv484b <= 888) | (mv484c > 0 & mv484c <= 888)  ~ 1,
      TRUE ~ 0),
    rc_tobc_cig = add_labels(rc_tobc_cig, labels = c("No"=0, "Yes"=1)),
    rc_tobc_cig = set_label(rc_tobc_cig, label = "Smokes cigarettes"))

# for older surveys use mv463a variables
#if (!is.null(MRdata$mv463a)){
#MRdata <- MRdata %>%
#  mutate(rc_tobc_cig = case_when(
#    mv463a == 1 ~ 1,
#    TRUE ~ 0),
#  rc_tobc_cig = add_labels(rc_tobc_cig, labels = c("No"=0, "Yes"=1)),
#  rc_tobc_cig = set_label(rc_tobc_cig, label = "Smokes cigarettes"))
#}  

MRdata <- MRdata %>%
  mutate(rc_tobc_other = case_when(
      (mv464d > 0 & mv464d <= 888) | (mv464e > 0 & mv464e <= 888) | (mv464f > 0 & mv464f <= 888) | (mv464g > 0 & mv464g <= 888) |
      (mv484d > 0 & mv484d <= 888) | (mv484e > 0 & mv484e <= 888) | (mv484f > 0 & mv484f <= 888) | (mv484g > 0 & mv484g <= 888) ~ 1,
      TRUE ~ 0),
  rc_tobc_other = add_labels(rc_tobc_other, labels = c("No"=0, "Yes"=1)),
  rc_tobc_other = set_label(rc_tobc_other, label = "Smokes other type of tobacco"))

# for older surveys use mv463 variables
#if (!is.null(MRdata$mv463b)){
#  MRdata <- MRdata %>%
#    mutate(rc_tobc_other = case_when(
#      mv463b==1 | mv463e==1 | mv463f==1 | mv463g==1 | mv463x==1 ~ 1,
#      TRUE ~ 0),
#    rc_tobc_other = add_labels(rc_tobc_other, labels = c("No"=0, "Yes"=1)),
#    rc_tobc_other = set_label(rc_tobc_other, label = "Smokes other type of tobacco"))
#}

MRdata <- MRdata %>%
  mutate(rc_tobc_smk_any = case_when(
      (mv464a > 0 & mv464a <= 888) | (mv464b > 0 & mv464b <= 888) | (mv464c > 0 & mv464c <= 888) |
      (mv464d > 0 & mv464d <= 888) | (mv464e > 0 & mv464e <= 888) | (mv464f > 0 & mv464f <= 888) | (mv464g > 0 & mv464g <= 888) |
      (mv484a > 0 & mv484a <= 888) | (mv484b > 0 & mv484b <= 888) | (mv484c > 0 & mv484c <= 888) |
      (mv484d > 0 & mv484d <= 888) | (mv484e > 0 & mv484e <= 888) | (mv484f > 0 & mv484f <= 888) | (mv484g > 0 & mv484g <= 888) ~ 1,
      TRUE ~ 0),
  rc_tobc_smk_any = add_labels(rc_tobc_smk_any, labels = c("No"=0, "Yes"=1)),
  rc_tobc_smk_any = set_label(rc_tobc_smk_any, label = "Smokes any type of tobacco"))

# for older surveys use mv463 variables
#  MRdata <- MRdata %>%
#    mutate(rc_tobc_smk_any = case_when(
#      mv463a==1 | mv463b==1 | mv463e==1 | mv463f==1 | mv463g==1 | mv463x==1  ~ 1,
#      TRUE ~ 0),
#    rc_tobc_smk_any = add_labels(rc_tobc_smk_any, labels = c("No"=0, "Yes"=1)),
#    rc_tobc_smk_any = set_label(rc_tobc_smk_any, label = "Smokes any type of tobacco"))


# Smoking frequency
  MRdata <- MRdata %>%
    mutate(rc_smk_freq = mv463aa,
    rc_smk_freq = add_labels(rc_smk_freq, labels = c("Non-smoker"=0, "Daily smoker"=1, "Occasional smoker"=2)),
    rc_smk_freq = set_label(rc_smk_freq, label = "Smoking frequency"))

# Average number of cigarettes per day
  MRdata <- MRdata %>%
    mutate(ciga = case_when(
      (mv464a >= 1 & mv464a < 888)   ~ mv464a, TRUE ~ 0)) %>%
   mutate(cigb = case_when(
      (mv464b >= 1 & mv464b < 888)   ~ mv464b, TRUE ~ 0)) %>%
   mutate(cigc = case_when(
      (mv464c >= 1 & mv464c < 888)   ~ mv464c, TRUE ~ 0)) %>%
   mutate(cigdaily = ciga + cigb + cigc) %>%
   mutate(rc_cig_day = case_when(
    (rc_smk_freq==1 ) & (cigdaily >= 1 & cigdaily <= 4) ~ 1,
    (rc_smk_freq==1 ) & (cigdaily >= 5 & cigdaily <= 9) ~ 2,
    (rc_smk_freq==1 ) & (cigdaily >= 10 & cigdaily <= 14) ~ 3,
    (rc_smk_freq==1 ) & (cigdaily >= 15 & cigdaily <= 24) ~ 4,
    (rc_smk_freq==1 ) & (cigdaily >= 25) ~ 5,
    (rc_smk_freq==1 ) & is.na(cigdaily) ~ 9),
    rc_cig_day = add_labels(rc_cig_day, labels = c("<5"=1, "5-9"=2, "10-14"=3, "15-24"=4, "25+"=5, "Don't know/missing"=9)),
    rc_cig_day = set_label(rc_cig_day, label = "Average number of cigarettes smoked per day"))

  MRdata <- MRdata %>%
    mutate(rc_tobc_snuffm = case_when(
      (mv464h >= 1) |  (mv484h >= 1)  ~ 1, TRUE ~ 0),
      rc_tobc_snuffm = add_labels(rc_tobc_snuffm, labels = c("No"=0, "Yes"=1)),
      rc_tobc_snuffm = set_label(rc_tobc_snuffm, label = "Uses snuff smokeless tobacco by mouth"))
  
  MRdata <- MRdata %>%
    mutate(rc_tobc_snuffn = case_when(
      (mv464i >= 1) |  (mv484i >= 1)  ~ 1, TRUE ~ 0),
      rc_tobc_snuffn = add_labels(rc_tobc_snuffn, labels = c("No"=0, "Yes"=1)),
      rc_tobc_snuffn = set_label(rc_tobc_snuffn, label = "Uses snuff smokeless tobacco by nose"))
  
  
  MRdata <- MRdata %>%
    mutate(rc_tobc_chew = case_when(
      (mv464j >= 1) |  (mv484j >= 1)  ~ 1, TRUE ~ 0),
      rc_tobc_chew = add_labels(rc_tobc_chew, labels = c("No"=0, "Yes"=1)),
      rc_tobc_chew = set_label(rc_tobc_chew, label = "Chews tobacco"))
  
  
  MRdata <- MRdata %>%
    mutate(rc_tobv_betel = case_when(
      (mv464k >= 1) |  (mv484k >= 1)  ~ 1, TRUE ~ 0),
      rc_tobv_betel = add_labels(rc_tobv_betel, labels = c("No"=0, "Yes"=1)),
      rc_tobv_betel = set_label(rc_tobv_betel, label = "Uses betel quid with tobacco"))
  
  MRdata <- MRdata %>%
    mutate(rc_tobc_osmkless = case_when(
      (mv464l >= 1) | (mv484l > 1) ~ 1, TRUE ~ 0),
      rc_tobc_osmkless = add_labels(rc_tobc_osmkless, labels = c("No"=0, "Yes"=1)),
      rc_tobc_osmkless = set_label(rc_tobc_osmkless, label = "Uses other type of smokeless tobacco"))
  
  MRdata <- MRdata %>%
    mutate(rc_tobc_anysmkless = case_when(
      (mv464h >= 1) | (mv484h >= 1) | (mv464i >= 1) | (mv484i >= 1) |
        (mv464j >= 1) | (mv484j >= 1) | (mv464k >= 1) | (mv484k >= 1) |
        (mv464l >= 1) | (mv484l >= 1) ~ 1,
      TRUE ~ 0),
      rc_tobc_anysmkless = add_labels(rc_tobc_anysmkless, labels = c("No"=0, "Yes"=1)),
      rc_tobc_anysmkless = set_label(rc_tobc_anysmkless, label = "Smokes any type of smokeless tobacco"))
  
  MRdata <- MRdata %>%
    mutate(rc_tobc_any = case_when(
      mv463aa == 1 | mv463aa == 2 | mv463ab == 1 | mv463ab == 2  ~ 1, TRUE ~ 0),
      rc_tobc_any = add_labels(rc_tobc_any, labels = c("No"=0, "Yes"=1)),
      rc_tobc_any = set_label(rc_tobc_any, label = "Uses any type of tobacco - smoke or smokeless"))