# ******************************************************************************
# Program: 			  RC_CHAR_WM.R
# Purpose: 		    Code to compute respondent characteristics of women  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Mahmoud Elkasabi
# Date last modified: April 01 2021 by Mahmoud Elkasabi
# ******************************************************************************
#The indicators are computed for age 15-49 in line 49. This can be commented out if the indicators are required for all women.
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
# rc_tobc_snuffm		"Uses snuff smokeless tobacco by mouth"
# rc_tobc_snuffn		"Uses snuff smokeless tobacco by nose"
# rc_tobc_chew		"Chews tobacco"
# rc_tobv_betel		"Uses betel quid with tobacco"
# rc_tobc_osmkless	"Uses other type of smokeless tobacco"
# rc_tobc_anysmkless	"Uses any type of smokeless tobacco"
# rc_tobc_any			"Uses any type of tobacco - smoke or smokeless"
# -----------------------------------------------------------------------------#

IRdata <- IRdata %>%
  filter(v012 <= 49) %>%
  mutate(wt = v005/1000000) %>%
  mutate(rc_edu = v149,
         rc_edu = set_label(rc_edu, label = "Highest level of schooling attended or completed")) %>%
  mutate(eduyr = case_when(
    v133<=20 ~ v133, 
    v133>20 & v133<95 ~ 20, 
    v133>95 | v149>7 ~ 99)) %>%
  replace_with_na(replace = list(eduyr = c(99)))

IRdata <- IRdata %>%
  mutate(rc_litr_cats = case_when(
    !v106 == 3 & v155 == 2 ~ 1,
    !v106 == 3 & v155 == 1 ~ 2,
    !v106 == 3 & v155 == 0 ~ 3,
    !v106 == 3 & v155 == 3 ~ 4,
    !v106 == 3 & v155 == 4 ~ 5,
    v106 == 3 ~ 0),
  rc_litr_cats = add_labels(rc_litr_cats, labels = c("Higher than secondary education"=0, "Can read a whole sentence"=1,
                                      "Can read part of a sentence"=2, "Cannot read at all"=3,
                                      "No card with required language"=4, "Blind/visually impaired"=5)),
  rc_litr_cats = set_label(rc_litr_cats, label = "Level of literacy")) %>%
  mutate(rc_litr = case_when(
    v106==3 | v155==1 | v155==2 ~ 1, TRUE ~ 0),
    rc_litr = add_labels(rc_litr, labels = c("No"=0, "Yes"=1)),
    rc_litr = set_label(rc_litr, label = "Literate"))


# Media exposure

IRdata <- IRdata %>%
  mutate(rc_media_newsp = case_when(
    v157 == 2 | v157 == 3 ~ 1,
    v157 == 0 | v157 == 1 ~ 0),
  rc_media_newsp = add_labels(rc_media_newsp, labels = c("No"=0, "Yes"=1)),
  rc_media_newsp = set_label(rc_media_newsp, label = "Reads a newspaper at least once a week")) %>%
 
  mutate(rc_media_tv = case_when(
    v159 == 2 | v159 == 3 ~ 1,
    v159 == 0 | v159 == 1 ~ 0),
  rc_media_tv = add_labels(rc_media_tv, labels = c("No"=0, "Yes"=1)),
  rc_media_tv = set_label(rc_media_tv, label = "Watches television at least once a week")) %>%
  
  mutate(rc_media_radio = case_when(
    v158 == 2 | v158 == 3 ~ 1,
    v158 == 0 | v158 == 1 ~ 0),
  rc_media_radio = add_labels(rc_media_radio, labels = c("No"=0, "Yes"=1)),
  rc_media_radio = set_label(rc_media_radio, label = "Listens to radio at least once a week")) %>%
  
  mutate(rc_media_allthree = case_when(
    rc_media_newsp == 1 & rc_media_tv == 1 & rc_media_radio == 1  ~ 1, TRUE ~ 0),
  rc_media_allthree = add_labels(rc_media_allthree, labels = c("No"=0, "Yes"=1)),
  rc_media_allthree = set_label(rc_media_allthree, label = "Accesses to all three media at least once a week")) %>%
  
  mutate(rc_media_none = case_when(
    rc_media_newsp == 0 & rc_media_tv == 0 & rc_media_radio == 0  ~ 1, TRUE ~ 0),
  rc_media_none = add_labels(rc_media_none, labels = c("No"=0, "Yes"=1)),
  rc_media_none = set_label(rc_media_none, label = "Accesses none of the three media at least once a week"))


if (!is.null(IRdata$v171a)){
  
  IRdata <- IRdata %>%
    mutate(rc_intr_ever = case_when(
      v171a == 1 | v171a == 2 | v171a == 3 ~ 1,
      v171a == 0 ~ 0),
      rc_intr_ever = add_labels(rc_intr_ever, labels = c("No"=0, "Yes"=1)),
      rc_intr_ever = set_label(rc_intr_ever, label = "Ever used the internet")) %>%
    
    mutate(rc_intr_use12mo = case_when(
      v171a == 1  ~ 1,
      v171a == 0 | v171a == 2 | v171a == 3 ~ 0),
    rc_intr_use12mo = add_labels(rc_intr_use12mo, labels = c("No"=0, "Yes"=1)),
    rc_intr_use12mo = set_label(rc_intr_use12mo, label = "Used the internet in the past 12 months")) %>%
    
    mutate(rc_intr_usefreq = case_when(
      v171a == 1  ~ v171b, TRUE ~ 99))  %>%
    replace_with_na(replace = list(rc_intr_usefreq = c(99))) %>%
    mutate(rc_intr_usefreq = set_label(rc_intr_usefreq, label = "Internet use frequency in the past month - among users in the past 12 months"))
  
}

## Employment

IRdata <- IRdata %>%
  mutate(rc_empl = case_when(
    v731 == 0 ~ 0,
    v731 == 1 ~ 1,
    v731 == 2 | v731 == 3 ~ 2,
    v731 == 8 ~ 9),
    rc_empl = add_labels(rc_empl, labels = c("Not employed in last 12 months"=0, 
                                 "Not curcently working but was employed in last 12 months"=1,
                                 "Curcently employed"=2,
                                 "Don't know/missing"=9)),
    rc_empl = set_label(rc_empl, label = "Employment status")) %>%
  
  mutate(emp = case_when(
    v731 == 1 | v731 == 2 | v731 == 3  ~ 1,
    TRUE ~ 0)) %>%

  mutate(rc_occup = case_when(
    emp == 1 & v717 == 1 ~ 1,
    emp == 1 & v717 == 2 ~ 2,
    emp == 1 & (v717 == 3 | v717 == 7) ~ 3,
    emp == 1 & v717 == 8 ~ 4,
    emp == 1 & v717 == 9 ~ 5,
    emp == 1 & v717 == 6 ~ 6,
    emp == 1 & (v717 == 4 | v717 == 5) ~ 7,
    emp == 1 & (v717 == 96 | v717 == 99 | is.na(v717)) ~ 9 ),
    rc_occup = add_labels(rc_occup, labels = c("Professional"=1, 
                                 "Clerical"=2,
                                 "Sales and services"=3,
                                 "Skilled manual"=4,
                                 "Unskilled manual"=5, 
                                 "Domestic service"=6,
                                 "Agriculture"=7,
                                 "Don't know/missing"=9)),
    rc_occup = set_label(rc_occup, label = "Occupation among those employed in the past 12 months")) %>%

  mutate(rc_empl_type = case_when(
    emp == 1 ~ v719),
  rc_empl_type = set_label(rc_empl_type, label = "Type of employer among those employed in the past 12 months")) %>%
  
  mutate(rc_empl_earn = case_when(
    emp == 1 ~ v741),
  rc_empl_earn = set_label(rc_empl_earn, label = "Type of earnings among those employed in the past 12 months")) %>%
  
  mutate(rc_empl_cont = case_when(
    emp == 1 ~ v732),
  rc_empl_cont = set_label(rc_empl_cont, label = "Continuity of employment among those employed in the past 12 months")) %>%
  
  mutate(rc_agri = case_when(
    rc_occup == 7 ~ 1,
    !is.na(rc_occup) & !rc_occup==7 ~ 0),
  rc_agri = add_labels(rc_agri, labels = c("Non-Agriculture"=0, 
                                  "Agriculture"=1)))

## Health insurance 
  
# Note: The different types of health insurance can be country specific. Please check the v481* variables to see which ones you need.
# In addition, some surveys report this for all women/men and some report it among those that have heard of insurance. Please check what the population of interest is for reporting these indicators.

IRdata <- IRdata %>%
  mutate(rc_hins_ss = v481c,
  rc_hins_ss = set_label(rc_hins_ss, label = "Health insurance coverage - social security")) %>%
  mutate(rc_hins_empl = v481b,
  rc_hins_empl = set_label(rc_hins_empl, label = "Health insurance coverage - other employer-based insurance")) %>%
  mutate(rc_hins_comm = v481a,
  rc_hins_comm = set_label(rc_hins_comm, label = "Health insurance coverage - mutual health org. or community-based insurance")) %>%
  mutate(rc_hins_priv = v481d,
  rc_hins_priv = set_label(rc_hins_priv, label = "Health insurance coverage - privately purchased commercial insurance")) %>%
  mutate(rc_hins_other = case_when(
    v481e == 1 | v481f == 1 | v481g == 1 | v481h == 1 | v481x == 1 ~ 1,
    TRUE ~ 0),
  rc_hins_other = set_label(rc_hins_other, label = "Health insurance coverage - other type of insurance")) %>%
  mutate(rc_hins_any = case_when(
    v481a == 1 | v481b == 1 | v481c == 1 | v481d == 1 | v481e == 1 | v481f == 1 | v481g == 1 | v481h == 1 | v481x == 1 ~ 1,
    TRUE ~ 0),
  rc_hins_any = set_label(rc_hins_any, label = "Have any health insurance"))


## Tobacco use
# please check all v463* variables for types of smoking and tobacco use

#for some surveys v463a was used instead of v463aa
#however v463a is not a yes/no variable for cigarette smoking, v463aa is the frequency

if (!is.null(IRdata$v463a)){
  
    IRdata <- IRdata %>%
    mutate(v463aa = v463a)
}

IRdata <- IRdata %>%
  mutate(rc_tobc_cig = case_when(
    v463aa == 1 | v463aa == 2 | v463e == 1 ~ 1, TRUE ~ 0),
  rc_tobc_cig = add_labels(rc_tobc_cig, labels = c("No"=0, "Yes"=1)),
  rc_tobc_cig = set_label(rc_tobc_cig, label = "Smokes cigarettes"))%>%
  mutate(rc_tobc_other = case_when(
    v463b == 1 | v463f == 1 | v463g == 1 ~ 1, TRUE ~ 0),
  rc_tobc_other = add_labels(rc_tobc_other, labels = c("No"=0, "Yes"=1)),
  rc_tobc_other = set_label(rc_tobc_other, label = "Smokes other type of tobacco"))%>%
  mutate(rc_tobc_smk_any = case_when(
    v463aa == 1 | v463aa == 2 | v463e == 1 | v463b == 1 | v463f == 1 | v463g == 1 ~ 1,
    TRUE ~ 0),
  rc_tobc_smk_any = add_labels(rc_tobc_smk_any, labels = c("No"=0, "Yes"=1)),
  rc_tobc_smk_any = set_label(rc_tobc_smk_any, label = "Smokes any type of tobacco"))

if (!is.null(IRdata$v463h)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobc_snuffm = v463h,
    rc_tobc_snuffm = set_label(rc_tobc_snuffm, label = "Uses snuff smokeless tobacco by mouth"))
}

if (!is.null(IRdata$v463d)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobc_snuffn = v463d,
    rc_tobc_snuffn = set_label(rc_tobc_snuffn, label = "Uses snuff smokeless tobacco by nose"))
}

if (!is.null(IRdata$v463c)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobc_chew = v463c,
    rc_tobc_chew = set_label(rc_tobc_chew, label = "Chews tobacco"))
}

if (!is.null(IRdata$v463i)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobv_betel = v463i,
    rc_tobv_betel = set_label(rc_tobv_betel, label = "Uses betel quid with tobacco"))
}

if (!is.null(IRdata$v463l)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobc_osmkless = v463l,
    rc_tobc_osmkless = set_label(rc_tobc_osmkless, label = "Uses other type of smokeless tobacco"))
}

if (!is.null(IRdata$v463h) & !is.null(IRdata$v463d) & !is.null(IRdata$v463c) & !is.null(IRdata$v463l)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobc_anysmkless = case_when(
      v463h == 1 | v463d == 1 | v463c == 1 | v463l == 1  ~ 1,
      TRUE ~ 0),
    rc_tobc_anysmkless = add_labels(rc_tobc_anysmkless, labels = c("No"=0, "Yes"=1)),
    rc_tobc_anysmkless = set_label(rc_tobc_anysmkless, label = "Smokes any type of smokeless tobacco"))
}

if (!is.null(IRdata$v463aa) & !is.null(IRdata$v463ab)){
  
  IRdata <- IRdata %>%
    mutate(rc_tobc_any = case_when(
      v463aa == 1 | v463aa == 2 | v463ab == 1 | v463ab == 2  ~ 1,
      TRUE ~ 0),
    rc_tobc_any = add_labels(rc_tobc_any, labels = c("No"=0, "Yes"=1)),
    rc_tobc_any = set_label(rc_tobc_any, label = "Uses any type of tobacco - smoke or smokeless"))
}