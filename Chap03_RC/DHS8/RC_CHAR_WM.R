# ******************************************************************************
# Program: 			  RC_CHAR_WM.R - DHS update
# Purpose: 		    Code to compute respondent characteristics of women  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Mahmoud Elkasabi, Ali Roghani, Trevor Croft, Courtney Allen
# Date last modified: August 23, 2024 by Courtney Allen
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
# rc_alc_any		"Consumed alcohol in the last one month" - NEW Indicator in DHS8
# rc_alc_freq		"Frequency of drinking among those who consumed alcohol in the last one month" - NEW Indicator in DHS8
# rc_alc_drinks		"Average number of drinks consumed among those who consumed alcohol in the last one month" - NEW Indicator in DHS8
# rc_place_birth	"Place of birth and residence"  - NEW Indicator in DHS8
# rc_migrant_5yrs	"Migrant that moved to current place of residence in the last 5 years" - NEW Indicator in DHS8
# rc_migrant_type	"Type of migrant that moved to current place of residence in the last 5 years" - NEW Indicator in DHS8
# rc_migrant_reason		"Reason for migration among those who moved to current place of residence in the last 5 years" - NEW Indicator in DHS8
# -----------------------------------------------------------------------------#

# Create weight
IRdata <- IRdata %>%  mutate(wt = v005/1000000) 

# INDICATORS ON EDUCATION AND LITERACY --------------------

# Highest level of schooling
IRdata <- IRdata %>% 
  mutate(rc_edu = v149,
       rc_edu = set_label(rc_edu, label = "Highest level of schooling attended or completed")) %>%
  mutate(eduyr = case_when(
    v133<=20 ~ v133, 
    v133>20 & v133<95 ~ 20, 
    v133>95 | v149>7 ~ 99)) %>%
  replace_with_na(replace = list(eduyr = c(99)))


# MEDIAN AGE FUNCTION ----------------------------------------------------------
calc_median_edu <-function() {
  
  # create a median years dataframe with cumulative proportions by each subgroup, use survey weights
  median_df <- data.frame(prop_cumulative = unclass(round(cumsum(prop.table(svytable(~temp_df$med_age_source, design=dhssvy2))),4)))
  median_df$age <- as.numeric(row.names(median_df))
  
  # find age groups before and after the cumulative 50% 
  median_df <- median_df %>%
    mutate(yr_before50 = case_when(prop_cumulative<0.5 & lead(prop_cumulative>0.5) ~ 1, TRUE ~ 0),
           yr_after50 = case_when(prop_cumulative>=0.5 & lag(prop_cumulative<0.5) ~ 1, TRUE ~ 0))
  
  # use equation for interpolated median for discrete variables (see https://www.dhsprogram.com/data/Guide-to-DHS-Statistics.cfm)
  
  # year before the cumulative 50%
  m1 <- median_df$age[median_df$yr_before50==1]
  
  # cumulative proportion for the year before the cumulative 50%
  p1 <- median_df$prop_cumulative[median_df$yr_before50==1]
  
  # cumulative proportion for the year after the cumulative 50%
  p2 <- median_df$prop_cumulative[median_df$yr_after50==1]
  
  # calculate median yrs
  median_yr <- round((m1 + ((0.5-p1)/(p2-p1))),1)
  
  print(median_yr)
}



# MEDIAN YEARS OF EDUCATION BY AGE GROUP AND SUBGROUP (WOMEN)----------------------
# mutate a dummy variable to loop through all women instead of subgroup
IRdata <- IRdata %>% mutate(all = 1) %>% set_variable_labels(all = "total") %>% set_value_labels(all = c("total"=1))
IRdata <- IRdata %>% mutate(med_age_source=eduyr)


# mutate a dummy variable to loop through all women instead of subgroup
IRdata <- IRdata %>% mutate(all = 1) %>% set_variable_labels(all = "total") %>% set_value_labels(all = c("total"=1))

# list of subgroup characteristics (residence, education, wealth, total)
subgroup <- c("v013", "v025", "v190", "all")

# create empty dataframe to fill in with results
median_edu <- data.frame("subgroup"=NA, "level"=NA, "median edu"=NA)

# set age range
beg_age <- 15
end_age <- 49

# create loop for each age group and each level of each subgroup characteristic
for(y in subgroup) {  
  
  # generate list of unique levels for each subgroup
  z <- as.vector(remove_val_labels(sort(unique(IRdata[[y]]))))
  
  # loop through each level of each subgroup
  for(x in z) {
      cat("finding median for ages", beg_age, "to", end_age, "subgroup:", y, "level=", x)
      
      #subset the age group using a beginning and ending age and subgroup
      temp_df <- IRdata %>%
        select(med_age_source, v012, v005, v021, v022, {{subgroup}}) %>%
        filter(v012>=beg_age & v012<=end_age)
      temp_df <- temp_df[ temp_df[y]==x, ]
      
      # weight data
      dhssvy2 <- svydesign(id = temp_df$v021, strata=temp_df$v022, weights = temp_df$v005/1000000, data=temp_df)
      
      median_yr <- calc_median_edu()
      
      #save results
      data_row <- data.frame(var_label(IRdata[,y]), val_label(IRdata[,y],x), median_yr)
      median_edu <- rbind(median_edu, setnames(data_row, names(median_edu)))
  }
}


# Literacy
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

# INDICATORS ON EMPLOYMENT --------------------

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
    emp == 1 & v717>9 & v717<98 ~ 8,
    emp == 1 & (v717 == 98 | v717 == 99 | is.na(v717)) ~ 9 ),
    rc_occup = add_labels(rc_occup, labels = c("Professional"=1, 
                                 "Clerical"=2,
                                 "Sales and services"=3,
                                 "Skilled manual"=4,
                                 "Unskilled manual"=5, 
                                 "Domestic service"=6,
                                 "Agriculture"=7,
                                 "Other"=8,
                                 "Don't know/missing"=9)),
    rc_occup = set_label(rc_occup, label = "Occupation among those employed in the past 12 months"),
    rc_empl_type = case_when(
      emp == 1 ~ v719),
    rc_empl_type = set_label(rc_empl_type, label = "Type of employer among those employed in the past 12 months"),
    rc_empl_earn = case_when(
      emp == 1 ~ v741),
    rc_empl_earn = set_label(rc_empl_earn, label = "Type of earnings among those employed in the past 12 months"),
    rc_empl_cont = case_when(
      emp == 1 ~ v732),
    rc_empl_cont = set_label(rc_empl_cont, label = "Continuity of employment among those employed in the past 12 months"),
    rc_agri = case_when(
      rc_occup == 7 ~ 1,
      !is.na(rc_occup) & !rc_occup==7 ~ 0),
    rc_agri = add_labels(rc_agri, labels = c("Non-Agriculture"=0, "Agriculture"=1)))

# INDICATORS ON HEALTH INSURANCE --------------------

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
    rc_hins_other = add_labels(rc_hins_other, labels = c("No"=0, "Yes"=1)),
    rc_hins_other = set_label(rc_hins_other, label = "Health insurance coverage - other type of insurance")) %>%
  mutate(rc_hins_any = case_when(
    v481a == 1 | v481b == 1 | v481c == 1 | v481d == 1 | v481e == 1 | v481f == 1 | v481g == 1 | v481h == 1 | v481x == 1 ~ 1,
    TRUE ~ 0),
    rc_hins_any = add_labels(rc_hins_any, labels = c("No"=0, "Yes"=1)),
    rc_hins_any = set_label(rc_hins_any, label = "Have any health insurance"))


# INDICATORS ON TOBACCO --------------------
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

# INDICATORS ON ALCOHOL USE --------------------
# Consumed any alcohol - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(rc_alc_any = case_when(
    between(v485a, 1, 31) | v485a == 95 ~ 1,
    TRUE ~ 0
  ),
  rc_alc_any = add_labels(rc_alc_any, labels = c("No"=0, "Yes"=1)),
  rc_alc_any = set_label(rc_alc_any, label = "Consumed alcohol in the last one month"))

# Frequency of drinking - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate( rc_alc_freq = case_when(
    v485a %in% c(25:31, 95) ~ 1,
    v485a %in% c(1:5) ~ 2,
    v485a %in% c(6:10) ~ 3,
    v485a %in% c(11:24) ~ 4
  ),
  rc_alc_freq = add_labels(rc_alc_freq, labels = c("Every day/almost daily" = 1, "1-5 days in the past month" = 2,
                                                   "6-10 days in the past month" = 3, "11-24 days in the past month" = 4)),
  rc_alc_freq = set_label(rc_alc_freq, label = "Frequency of drinking among those who consumed alcohol in the last one month"))


# Average number of drinks - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate( rc_alc_drinks = case_when(v485b == 0 ~ 0,
                                    v485b == 1 ~ 1,
                                    v485b == 2 ~ 2,
                                    v485b == 3 ~ 3,
                                    v485b == 4 ~ 4,
                                    v485b == 5 ~ 5,
                                    v485b >= 6 ~ 6
  ),
  rc_alc_drinks = add_labels(rc_alc_drinks, labels = c("Less than one" = 0,
                                                       "One" = 1,
                                                       "Two" = 2,
                                                       "Three" = 3,
                                                       "Four" = 4,
                                                       "Five" = 5,
                                                       "Six or more" = 6)),
  rc_alc_drinks = set_label(rc_alc_drinks, label = "Average number of drinks consumed among those who consumed alcohol in the last one month"))

# INDICATORS ON MIGRATION --------------------

# Place of birth - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(
    rc_place_birth = case_when(
      v104 == 95 & v172 != 96 ~ 1,
      v104 < 95 & v172 != 96 ~ 2,
      v172 == 96 ~ 3,
      TRUE ~ NA
    )
  ) %>%
  mutate(
    rc_place_birth = add_labels(
      rc_place_birth,
      labels = c(
        "Born in current place of residence" = 1,
        "Born in country but not in current place of residence" = 2,
        "Born outside of the country" = 3
      )
    ),
    rc_place_birth = set_label(rc_place_birth, label = "Place of birth and residence")
  )

#  Migrant that moved to current place of residence in the last 5 years - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(rc_migrant_5yrs = case_when(
    v104 > 4 & v104 < 96 & rc_place_birth > 1 ~ 0,
    v104 < 5 ~ 1
  ),
  rc_migrant_5yrs = add_labels(rc_migrant_5yrs, labels = c("No"=0, "Yes"=1)),
  rc_migrant_5yrs = set_label(rc_migrant_5yrs, label = "Migrant that moved to current place of residence in the last 5 years"))


# Type of migrant - NEW Indicator in DHS8 
IRdata <- IRdata %>%
  mutate(prevresid = case_when(
    between(v105, 0, 2) ~ 1,  # Urban category
    v105 == 3 ~ 2             # Rural category
  ))


IRdata <- IRdata %>%
  group_by(prevresid, v102) %>%
  mutate(
    rc_migrant_type = cur_group_id(),
    rc_migrant_type = ifelse(rc_migrant_5yrs != 1, NA, rc_migrant_type)
  ) %>%
  ungroup() %>%
  mutate(
    rc_migrant_type = factor(
      rc_migrant_type,
      levels = c(1, 2, 3, 4),
      labels = c("Urban to urban", "Rural to urban", "Urban to rural", "Rural to rural"),
      exclude = NULL
    )
  )

# Reason for migration - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(rc_migrant_reason = case_when(
    v175 == 1 ~ 1,
    v175 == 2 ~ 2,
    v175 == 3 ~ 3,
    v175 == 4 ~ 4,
    v175 == 5 ~ 5,
    v175 == 6 ~ 6,
    v175 > 6 & v175 < 96 ~ 7,
    v175 == 96 ~ 96,
    TRUE ~ NA  
  ),
  rc_migrant_reason = add_labels(rc_migrant_reason, labels = c("Employment" = 1,
                                                               "Education/training" = 2,
                                                               "Marriage" = 3,
                                                               "Family reunification/family reasons" = 4,
                                                               "Forced displacement" = 5,
                                                               "Migration" = 6,
                                                               "Other country-specific" = 7,
                                                               "Other" = 96)),
  rc_alc_drinks = set_label(rc_alc_drinks, label = "Reason for migration among those who moved to current place of residence in the last 5 years"))

