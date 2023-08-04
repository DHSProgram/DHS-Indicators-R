# /*****************************************************************************************************
# Program: 			PH_POP.R
# Purpose: 			Code to compute population characteristics, birth registration, education levels, household composition, orphanhood, and living arrangments
# Data inputs: 		PR dataset
# Data outputs:		coded variables
# Author:				Shireen Assaf for population indicators, and Tom Pullum and Mahmoud Elkasabi for living arrangements and orphanhood indicators
# Date last modified: January 31, 2022 by Mahmoud Elkasabi 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# ph_pop_age			  "De facto population by five-year age groups"
# ph_pop_depend		  "De facto population by dependency age groups"
# ph_pop_cld_adlt		"De facto population by child and adult populations"
# ph_pop_adols		  "De factor population that are adolesents"
# 	
# ph_birthreg_cert	  "Child under 5 with registered birth and birth certificate"
# ph_birthreg_nocert	"Child under 5 with registered birth and no birth certificate"
# ph_birthreg			    "Child under 5 with registered birth"
# 
# ph_highest_edu		  "Highest level of schooling attended or completed among those age 6 or over"
# ph_median_eduyrs_wm "Median years of education among those age 6 or over - Females"
# ph_median_eduyrs_mn "Median years of education among those age 6 or over - Males"
# 
# ph_wealth_quint		  "Wealth quintile - dejure population"
# 
# ph_chld_liv_arrang	"Living arrangement and parents survival status for child under 18"
# ph_chld_liv_noprnt	"Child under 18 not living with a biological parent"
# ph_chld_orph		    "Child under 18 with one or both parents dead"
# 
# ph_hhhead_sex		    "Sex of household head"
# ph_num_members		  "Number of usual household members"
# 	
# ph_orph_double		  "Double orphans under age 18"
# ph_orph_single		  "Single orphans under age 18"
# ph_foster			      "Foster children under age 18"
# ph_orph_foster		  "Orphans and/or foster children under age 18"
# ----------------------------------------------------------------------------*/


# *** Population characteristics ***

PRdata[["ager"]] <- ifelse(PRdata[["hv103"]]==1 , as.integer(PRdata[["hv105"]]/5), NA) 

# //Five year age groups
PRdata[["ph_pop_age"]] <- ifelse(PRdata[["ager"]]>=0 & PRdata[["ager"]]<=15, PRdata[["ager"]], 
                                 ifelse((PRdata[["ager"]]>=16 & PRdata[["ager"]]<=18) | (PRdata[["hv105"]]==95 & PRdata[["hv103"]]==1), 16, 
                                        ifelse((PRdata[["ager"]]>=19 & PRdata[["ager"]]<=20), 98, NA))) 

PRdata <- PRdata %>%
  set_value_labels(ph_pop_age = c("<5"=0, "5-9"=1, "10-14"=2, "15-19"=3, "20-24"=4, "25-29"=5, "30-34"=6,
                                  "35-39"=7, "40-44"=8, "45-49"=9, "50-54"=10, "55-59"=11, "60-64"=12, 
                                  "65-69"=13, "70-74"=14, "75-79"=15, "80+"=16, "Don't know/missing"=98)) %>%
  set_variable_labels(ph_pop_age = "De facto population by five-year age groups")

# //Dependency age groups
PRdata <- PRdata %>%
  mutate(ph_pop_depend =
           case_when(
             inrange(ager,0,2) ~ 1,
             inrange(ager,3,12)  ~ 2,
             inrange(ager,13,18)  ~ 3,
             inrange(ager,19,20) ~ 98)) %>%
  set_value_labels(ph_pop_depend = c("0-14"=1, "15-64"=2, "65+"=3, "Don't know/missing"=98)) %>%
  set_variable_labels(ph_pop_depend = "De facto population by dependency age groups")

# //Child and adult populations
PRdata <- PRdata %>%
  mutate(ph_pop_cld_adlt =
           case_when(
             hv103==1 & hv105<18  ~ 1,
             hv103==1 & hv105>=18 & hv105<97  ~ 2,
             hv103==1 & hv105>=98 ~ 98)) %>%
  set_value_labels(ph_pop_cld_adlt = c("0-17"=1, "18+"=2, "Don't know/missing"=98)) %>%
  set_variable_labels(ph_pop_cld_adlt = "De facto population by child and adult populations")

# //Adolescent population
PRdata <- PRdata %>%
  mutate(ph_pop_adols =
           case_when(
             hv103==1 & hv105>=10 & hv105<20  ~ 1,
             hv103==1 & (hv105<10 | hv105>=20) ~ 0)) %>%
  set_value_labels(ph_pop_adols = c("Adolescents 10-19"=1, "Not adolescents"=0)) %>%
  set_variable_labels(ph_pop_adols = "De facto population that are adolescents")

# *** Birth registration ***
# 
# //Child registered and with birth certificate
PRdata <- PRdata %>%
  mutate(ph_birthreg_cert =
           case_when(
             hv102==1 & hv105<5 & hv140==1 ~ 1,
             hv102==1 & hv105<5 & hv140!=1 ~ 0)) %>%
  set_value_labels(ph_birthreg_cert = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(ph_birthreg_cert = "Child under 5 with registered birth and birth certificate")

# //Child registered and with no birth certificate
PRdata <- PRdata %>%
  mutate(ph_birthreg_nocert =
           case_when(
             hv102==1 & hv105<5 & hv140==2 ~ 1,
             hv102==1 & hv105<5 & hv140!=2 ~ 0)) %>%
  set_value_labels(ph_birthreg_nocert = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(ph_birthreg_nocert = "Child under 5 with registered birth and no birth certificate")

# //Child is registered
PRdata <- PRdata %>%
  mutate(ph_birthreg =
           case_when(
             hv102==1 & hv105<5 & hv140 %in% c(1,2) ~ 1,
             hv102==1 & hv105<5 & hv140 %in% c(0,8,9) ~ 0)) %>%
  set_value_labels(ph_birthreg = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(ph_birthreg = "Child under 5 with registered birth")

# *** Wealth quintile ***
PRdata <- PRdata %>%
  mutate(ph_wealth_quint =
           case_when(hv102==1 ~ hv270)) %>%
  set_variable_labels(ph_wealth_quint = "Wealth quintile - dejure population")

# *** Education levels ***

# //Highest level of schooling attended or completed
PRdata <- PRdata %>%
  mutate(ph_highest_edu =
           case_when(hv103==1 & hv105>5 ~ hv109)) %>%
  set_variable_labels(ph_highest_edu = "Highest level of schooling attended or completed among those age 6 or over")

# //Median years of education - Females
  PRdata <- PRdata %>%
    mutate(eduyr=case_when( hv103==1 & hv105>5 & hv108<=96 & hv104==2 ~ hv108)) 
  
  #to obtain the 50% percentile
  sp50 <- matrixStats::weightedMedian(PRdata$eduyr, PRdata$wt, idxs = NULL, na.rm = TRUE)
  
  PRdata <- PRdata %>%
    mutate(dummy = case_when(eduyr<sp50  ~ 1, hv103==1 & hv105>5 & hv108<=96 & hv104==2  ~ 0  ))
  sL <- matrixStats::weightedMean(PRdata$dummy, PRdata$wt, idxs = NULL, na.rm = TRUE)
  
  PRdata <- PRdata %>%
    mutate(dummy =case_when(eduyr<=sp50 ~ 1,hv103==1 & hv105>5 & hv108<=96 & hv104==2 ~ 0  ))
  sU <- matrixStats::weightedMean(PRdata$dummy, PRdata$wt, idxs = NULL, na.rm = TRUE)

  # "Median years of education among those age 6 or over - Females"
  ph_median_eduyrs_wm=round(sp50-1+(0.5-sL)/(sU-sL),1)
  
# //Median years of education - Males
  PRdata <- PRdata %>%
    mutate(eduyr=case_when( hv103==1 & hv105>5 & hv108<=96 & hv104==1 ~ hv108)) 
  
  #to obtain the 50% percentile
  sp50 <- matrixStats::weightedMedian(PRdata$eduyr, PRdata$wt, idxs = NULL, na.rm = TRUE)
  
  PRdata <- PRdata %>%
    mutate(dummy = case_when(eduyr<sp50  ~ 1, hv103==1 & hv105>5 & hv108<=96 & hv104==1  ~ 0  ))
  sL <- matrixStats::weightedMean(PRdata$dummy, PRdata$wt, idxs = NULL, na.rm = TRUE)
  
  PRdata <- PRdata %>%
    mutate(dummy =case_when(eduyr<=sp50 ~ 1,hv103==1 & hv105>5 & hv108<=96 & hv104==1 ~ 0  ))
  sU <- matrixStats::weightedMean(PRdata$dummy, PRdata$wt, idxs = NULL, na.rm = TRUE)

  # "Median years of education among those age 6 or over - Males"
  ph_median_eduyrs_mn=round(sp50-1+(0.5-sL)/(sU-sL),1)
  

# *** Living arrangements ***
# 
# IMPORTANT: Children must be de jure residents AND co-residence with parents requires that
# the parents are also de jure residents
# 
# add a code 99 to hv112 if the mother is in the hh but is not de jure
# add a code 99 to hv114 if the mother is in the hh but is not de jure
# 
# Preparing files to produce the indicators, this required several merges

# keep only the variables we need
PR_temp <- select(PRdata,hv001, hv002, hvidx, hv005, hv009, hv024, hv025,hv270,
                  starts_with("hv10"),starts_with("hv11"), starts_with("ph_"))

# Prepare a file of potential mothers
PR_temp_mothers <- PR_temp %>%
  filter(hv104!=1) %>%
  filter(!(hv105<15))
PR_temp_mothers <- select(PR_temp_mothers,hv001, hv002, hvidx, hv102)
PR_temp_mothers <- PR_temp_mothers %>% rename(hv102_mo=hv102, hv112=hvidx)  

# Prepare a file of potential fathers
PR_temp_fathers <- PR_temp %>%
  filter(hv104!=2) %>%
  filter(!(hv105<15))
PR_temp_fathers <- select(PR_temp_fathers,hv001, hv002, hvidx, hv102)
PR_temp_fathers <- PR_temp_fathers %>% rename(hv102_fa=hv102, hv114=hvidx)  

# Prepare file of children for merges
PR_temp_children <- PR_temp %>%
  filter(hv102!=0) %>%
  filter(!(hv105>17)) %>%
  mutate(in_children = 1)

# Merge children with potential mothers
PR_temp_children <- merge(PR_temp_children, PR_temp_mothers, by = c("hv001", "hv002", "hv112"), all.y = TRUE, all.x = TRUE)

# Merge children with potential fathers
PR_temp_children <- merge(PR_temp_children, PR_temp_fathers, by = c("hv001", "hv002", "hv114"), all.y = TRUE, all.x = TRUE)

# Code 99 of the mother or father is not de jure
PR_temp_children[["hv112r"]] <- ifelse(PR_temp_children[["hv112"]]>0 & (PR_temp_children[["hv102_mo"]]==0 & !is.na(PR_temp_children[["hv102_mo"]])), 99, PR_temp_children[["hv112"]]) 
PR_temp_children[["hv114r"]] <- ifelse(PR_temp_children[["hv114"]]>0 & (PR_temp_children[["hv102_fa"]]==0 & !is.na(PR_temp_children[["hv102_fa"]])), 99, PR_temp_children[["hv114"]]) 

PR_temp_children <- PR_temp_children %>%
  filter(in_children==1)

# //Living arrangement for children under 18
PR_temp_children <- PR_temp_children %>%
  mutate(orphan_type =
           case_when(
             hv111==1 & hv113==1 ~ 1,
             hv111==1 & hv113==0 ~ 2,
             hv111==0 & hv113==1 ~ 3,
             hv111==0 & hv113==0 ~ 4,
             hv111>1  | hv113>1 ~ 5)) %>%
  set_value_labels(orphan_type = c("Both parents alive"=1, "Mother alive, father dead"=2, "Father alive, mother dead"=3,
                                   "Both parents dead"=4, "Info missing"=5)) 

PR_temp_children <- PR_temp_children %>%
  mutate(cores_type =
           case_when(
             (hv112r>0  & hv112r<99)  & (hv114r>0  & hv114r<99) ~ 1,
             (hv112r>0  & hv112r<99)  & (hv114r==0 | hv114r==99) ~ 2,
             (hv112r==0 | hv112r==99) & (hv114r>0  & hv114r<99) ~ 3,
             (hv112r==0 | hv112r==99) & (hv114r==0 | hv114r==99) ~ 4)) %>%
  set_value_labels(cores_type = c("Living with both parents"=1, "With mother, not father"=2, "With father, not mother"=3,
                                   "Living with neither parent"=4)) 

PR_temp_children <- PR_temp_children %>%
  mutate(ph_chld_liv_arrang =
           case_when(
             cores_type==1 ~ 1,
             cores_type==2 & (orphan_type==1 | orphan_type==3)  ~ 2,
             cores_type==2 & (orphan_type==2 | orphan_type==4) ~ 3,
             cores_type==3 & (orphan_type==1 | orphan_type==2) ~ 4,
             cores_type==3 & (orphan_type==3 | orphan_type==4) ~ 5,
             cores_type==4 & orphan_type==1 ~ 6,
             cores_type==4 & orphan_type==3 ~ 7,
             cores_type==4 & orphan_type==2 ~ 8,
             cores_type==4 & orphan_type==4 ~ 9,
             orphan_type==5 ~ 10
             )) %>%
  set_value_labels(ph_chld_liv_arrang = c("With both parents"=1 , "With mother only, father alive" = 2, 
                                          "With mother only, father dead"=3, "With father only, mother alive"=4, 
                                          "With father only, mother dead"=5, "With neither, both alive"=6,  
                                          "With neither, only father alive"=7, "With neither, only mother alive"=8, 
                                          "With neither, both dead"=9, "Survival info missing"=10)) %>%
  set_variable_labels(ph_chld_liv_arrang = "Living arrangement and parents survival status for child under 18") 

# //Child under 18 not living with either parent
PR_temp_children <- PR_temp_children %>%
  mutate(ph_chld_liv_noprnt =
           case_when(
             ph_chld_liv_arrang>=6 & ph_chld_liv_arrang<=9 ~ 1,
             TRUE ~ 0)) %>%
  set_value_labels(ph_chld_liv_noprnt = c("Yes"=1 , "No" = 0)) %>%
  set_variable_labels(ph_chld_liv_noprnt = "Child under 18 not living with a biological parent") 

# //Child under 18 with one or both parents dead
PR_temp_children <- PR_temp_children %>%
  mutate(ph_chld_orph =
           case_when(
             hv111==0 | hv113==0 ~ 1,
             TRUE ~ 0)) %>%
  set_value_labels(ph_chld_orph = c("Yes"=1 , "No" = 0)) %>%
  set_variable_labels(ph_chld_orph = "Child under 18 with one or both parents dead") 


# *** Orphanhood ***
# //Double orphan: both parents dead
PR_temp_children <- PR_temp_children %>%
  mutate(ph_orph_double =
           case_when(
             hv111==0 & hv113==0 ~ 1,
             TRUE ~ 0)) %>%
  set_value_labels(ph_orph_double = c("Yes"=1 , "No" = 0)) %>%
  set_variable_labels(ph_orph_double = "Child under 18 with both parents dead") 

# //Single orphan: one parent dead 
PR_temp_children <- PR_temp_children %>%
  mutate(ph_orph_single =
           case_when(
             ph_chld_orph==1 & ph_orph_double==0 ~ 1,
             TRUE ~ 0)) %>%
  set_value_labels(ph_orph_single = c("Yes"=1 , "No" = 0)) 

# //Foster child: not living with a parent but one or more parents alive
PR_temp_children <- PR_temp_children %>%
  mutate(ph_foster =
           case_when(
             cores_type==4 ~ 1,
             TRUE ~ 0)) %>%
  set_value_labels(ph_foster = c("Yes"=1 , "No" = 0)) 
 
# //Foster child and/or orphan
PR_temp_children <- PR_temp_children %>%
  mutate(ph_orph_foster =
           case_when(
             ph_foster==1 | ph_orph_single==1 | ph_orph_double==1 ~ 1,
             TRUE ~ 0)) %>%
  set_value_labels(ph_orph_foster = c("Yes"=1 , "No" = 0)) 


# *** Household characteristics *** 
PR_temp_wk <- PR_temp %>% 
  mutate(n=1) %>%  
  filter(hv102==1) 

PR_temp_wk <- select(PR_temp_wk,hv001, hv002, hvidx,n)

PR_temp_children0 <- merge(PR_temp_children, PR_temp_wk, by = c("hv001", "hv002", "hvidx"), all.y = TRUE)

# //Household size
HHSIZE <- PR_temp_children0 %>% 
  group_by(hv001, hv002) %>% 
  summarise(hhsize = sum(n, na.rm=TRUE)) 

PR_temp_children0 <- merge(PR_temp_children0, HHSIZE, by = c("hv001", "hv002"), all.y = TRUE)

ph_orph_double0 <- PR_temp_children0 %>% group_by(hv001, hv002) %>% summarise(ph_orph_double = sum(ph_orph_double, na.rm=TRUE)) 
ph_orph_single0 <- PR_temp_children0 %>% group_by(hv001, hv002) %>% summarise(ph_orph_single = sum(ph_orph_single, na.rm=TRUE)) 
ph_foster0 <- PR_temp_children0 %>% group_by(hv001, hv002) %>% summarise(ph_foster = sum(ph_foster, na.rm=TRUE)) 
ph_orph_foster0 <- PR_temp_children0 %>% group_by(hv001, hv002) %>% summarise(ph_orph_foster = sum(ph_orph_foster, na.rm=TRUE)) 

temp_children <- merge(ph_orph_double0, ph_orph_single0, by = c("hv001", "hv002"))
temp_children <- merge(temp_children, ph_foster0, by = c("hv001", "hv002"))
temp_children <- merge(temp_children, ph_orph_foster0, by = c("hv001", "hv002"))
temp_children <- merge(temp_children, HHSIZE, by = c("hv001", "hv002"))

# *** Household characteristics *** 
HHHEAD <- PR_temp %>%  
  filter(hv101==1) 
HHHEAD <- select(HHHEAD,hv001, hv002,hv005, hv024, hv025, hv101,hv102,hv103,hv104)
HHHEAD <- HHHEAD %>% rename(ph_hhhead_sex=hv104) 

temp_children <- merge(temp_children, HHHEAD, by = c("hv001", "hv002"))

temp_children[["ph_num_members"]] <- ifelse(temp_children[["hhsize"]]>9 , 9, temp_children[["hhsize"]]) 

temp_children[["ph_foster"]] <- ifelse(temp_children[["ph_foster"]]>1 , 1, temp_children[["ph_foster"]]) 
temp_children[["ph_orph_foster"]] <- ifelse(temp_children[["ph_orph_foster"]]>1 , 1, temp_children[["ph_orph_foster"]]) 
temp_children[["ph_orph_single"]] <- ifelse(temp_children[["ph_orph_single"]]>1 , 1, temp_children[["ph_orph_single"]]) 
temp_children[["ph_orph_double"]] <- ifelse(temp_children[["ph_orph_double"]]>1 , 1, temp_children[["ph_orph_double"]]) 

temp_children <- temp_children %>%
  set_value_labels(ph_orph_double = c("Yes"=1 , "No" = 0)) %>%
  set_value_labels(ph_orph_single = c("Yes"=1 , "No" = 0)) %>%
  set_value_labels(ph_foster = c("Yes"=1 , "No" = 0)) %>%
  set_value_labels(ph_orph_foster = c("Yes"=1 , "No" = 0)) %>%
  set_variable_labels(ph_orph_double = "Double orphans under age 18") %>%
  set_variable_labels(ph_orph_single = "Single orphans under age 18") %>%
  set_variable_labels(ph_foster = "Foster children under age 18") %>%
  set_variable_labels(ph_orph_foster = "Orphans and/or foster children under age 18") %>%
  set_variable_labels(ph_hhhead_sex = "Sex of household head")

temp_children <- temp_children %>%
  mutate(wt = hv005/1000000)

PR_temp_children <- PR_temp_children %>%
  mutate(wt = hv005/1000000)


