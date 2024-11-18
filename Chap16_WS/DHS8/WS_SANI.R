# /*****************************************************************************
# program: 			PH_SANI.R
# Purpose: 			creates variable for binary improved sanitation according to JSTOR standard 
# Data inputs: 		hr or pr file
# Data outputs:		none
# Author of do file:	03/15/2018	Courtney Allen
# Date last modified: 07/21/2023	Courtney Allen - for codeshare project
# Note:				These indicators can also be computed using the HR or PR file.
# 					If you want to produce estimates for households, use the HR file.
# 					If you want to produce estimates for the de jure population, 
# 					use the PR file and select for dejure household memebers using
# 					hv102==1. Please see the Guide to DHS Statistics.  
# *****************************************************************************/
# 
# NOTES AND VARIABLE LIST ------------------------------------------------------
# This do file can be run on any loop of countries indicating the dataset name 
# with variable called filename. Code should be same for pr or hr files.
# 
# VARIABLES CREATED:
# 	
# 	ph_sani_type		  "Type of sanitation facility"
# 	ph_sani_improve		"Access to improved sanitation"
# 	ph_sani_basic		  "Basic or limited sanitation facility"
# 	ph_sani_location	"Location of sanitation facility"
# 		
# 
# NOTE: 
# STANDARD CATEGORIES FOR SANITATION SOURCE BY IMPROVED/UNIMPROVED
# 	1-improved
# 		11	 flush - to piped sewer system
# 		12	 flush - to septic tank
# 		13	 flush - to pit latrine
# 		14	 flush - to somewhere else
# 		15   flush - don't know where
# 		16	 flush - unspecified
# 		20   pit latrine - improved but shared
# 		21	 pit latrine - ventilated improved pit (vip)
# 		22	 pit latrine - with slab
# 		41	 composting toilet
# 		51	 other improved
# 	2-unimproved 
# 		23	 pit latrine - without slab / open pit
# 		42	 bucket ph_sani_type
# 		43	 hanging toilet/latrine
# 		96	 other
# 	3-open defecation
# 		31	 no facility/bush/field/river/sea/lake
# 
# ******************************************************************************

# Generate type of sanitation facility  
 
#   NOTE: this cycles through ALL country specific coding and ends around line 1495.
#   Surveys are specified through their country code [hv000] and year [hv007] or month [hv006] when   necessary.



# create a variable for sanitation type, this var will be overwritten if country-specific coding is needed
WASHdata <- WASHdata %>% mutate(ph_sani_type = hv205)


# 	recode country-specific responses to standard codes ------------------------

if (WASHdata$hv000[1]=="AF7")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==43 ~ 23,
    hv205==44 ~ 51,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="AM4" & WASHdata$hv007[1]==2000) {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="AM4" & WASHdata$hv007[1]==2004) {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==41 ~ 42,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="AM6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==44 ~ 15,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="AO7")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 14,
    hv205==14 ~ 11,
    hv205==15 ~ 12,
    hv205==16 ~ 14,
    hv205==17 ~ 11,
    hv205==18 ~ 12,
    hv205==19 ~ 14,
    hv205==22 ~ 21,
    hv205==24 ~ 21,
    hv205==25 ~ 21,
    hv205==26 ~ 23,
    hv205==27 ~ 21,
    hv205==28 ~ 21,
    hv205==29 ~ 23,
    hv205==42 ~ 23,
    TRUE ~ hv205  
  )) }
# same recode for 4 surveys: three that all have hv000=BD3 (BDHR31, BDHR3A, and BDHR41) and one that is BD4
if (WASHdata$hv000[1] %in% c("BD3", "BD4")) {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 12,
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    hv205==24 ~ 43,
    TRUE ~ hv205  
  )) }
# same recode for 3 surveys: BFHR21, BFHR31, and BFHR43. BFHR31 and BFHR43 do not have a 41 category for hv205 and BFHR43 does not have a 12 category. 
if (WASHdata$hv000[1] %in% c("BF2", "BF3", "BF4")){
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BJ3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BJ4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==22 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BJ5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==15 ~ 14,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]<95)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==13 ~ 12,
    hv205==21 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]<98)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BO4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BR2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==41 ~ 96,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BR3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 14,
    hv205==13 ~ 14,
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="BU6" & WASHdata$hv007[1] %in% c(2012,2013))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==23 ~ 22,
    hv205==24 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="CD5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==23 ~ 21,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="CF3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="CG5" & WASHdata$hv007[1]==2005)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="CG5" & WASHdata$hv007[1]==2009)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==22 ~ 23,
    TRUE ~ hv205  
  )) }
if (WASHdata$hv000[1]=="CI3" & WASHdata$hv007[1]==94)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==23 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CI3" & WASHdata$hv007[1]>97)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CI5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CM2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 42,
    hv205==22 ~ 23,
    hv205==32 ~ 31,
    hv205==33 ~ 31,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CM3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CM4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CO2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CO3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 13,
    hv205==13 ~ 14,
    hv205==21 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]==2000)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 13,
    hv205==13 ~ 14,
    hv205==21 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]==2004)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 14,
    hv205==21 ~ 23,
    hv205==22 ~ 31,
    TRUE ~ hv205   
  )) }
# same recode for two surveys COHR61 and COHR72
if (WASHdata$hv000[1]=="CO5" | (WASHdata$hv000[1]=="CO7"  & WASHdata$hv007[1]>=2015))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 14,
    hv205==21 ~ 23,
    hv205==22 ~ 31,
    TRUE ~ hv205   
  )) }

if (WASHdata$hv000[1]=="DR2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==96)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    hv205==23 ~ 22,
    hv205==24 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==99)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for 3 surveys: DRHR4B, DRHR52, and DRHR5A
if (WASHdata$hv000[1] %in% c("DR4", "DR5"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    hv205==23 ~ 22,
    hv205==24 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="EG2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 15,
    hv205==14 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for 4 surveys: EGHR33, EGHR42, EGHR4A, and EGHR51. Only EGHR51 has category 32 for hv205
if (WASHdata$hv000[1] %in% c("EG3", "EG4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==21 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="EG5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="EG6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==16 ~ 12,
    hv205==17 ~ 11,
    hv205==18 ~ 11,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1992)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1997)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 41,
    hv205==25 ~ 42,
    hv205==26 ~ 43,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GA3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for three surveys: GHHR31, GHHR41, and GHHR4B. Only GHHR4B has category 32 for hv205 and only GHHR41 has category 23. 
if (WASHdata$hv000[1] %in% c("GH2", "GH3", "GH4")) {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GH7" & WASHdata$hv007[1]==2019)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==16 ~ 51,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GN3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==31 ~ 23,
    hv205==41 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GN4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==23 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]==95)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]>98)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 11,
    hv205==13 ~ 12,
    hv205==21 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GU6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 14,
    hv205==14 ~ 15,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="GY4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==61 ~ 43,
    TRUE ~ hv205   
  )) }
# same recode for two surveys HNHR52 and HNHR62
if (WASHdata$hv000[1] %in% c("HN5", "HN6"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 15,
    hv205==21 ~ 51,
    hv205==22 ~ 41,
    hv205==24 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="HT3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==23 ~ 21,
    hv205==24 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="HT4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="HT5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==51 ~ 42,
    hv205==61 ~ 43,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="HT6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==44 ~ 43,
    hv205==45 ~ 51,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: IAHR23 and IAHR42. Only IAHR23 has category 41 for hv205
if (WASHdata$hv000[1] %in% c("IA2", "IA3"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: IAHR52 (2005-2006), IAHR74 (2015-16), and IAHR7E (2019-21)
if (WASHdata$hv000[1] %in% c("IA5", "IA6", "IA7"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==44 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ID4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 12,
    hv205==12 ~ 14,
    hv205==21 ~ 15,
    hv205==41 ~ 23,
    hv205==51 ~ 31,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: IDHR51 and IDHR63. Only IDHR51 has categories 33 to 36 for hv205
if (WASHdata$hv000[1] %in% c("ID5", "ID6"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 12,
    hv205==12 ~ 14,
    hv205==13 ~ 15,
    hv205==21 ~ 23,
    hv205==32 ~ 31,
    hv205==33 ~ 31,
    hv205==34 ~ 31,
    hv205==35 ~ 31,
    hv205==36 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ID7" & WASHdata$hv007[1]==2017)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==16 ~ 14,
    hv205==17 ~ 15,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: JOHR31 and JOHR42. Only JOHR42 has category 22 for hv205
if (WASHdata$hv000[1] %in% c("JO3","JO4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: JOHR51 and JOHR61 both are hv000=JO5
if (WASHdata$hv000[1]=="JO5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==12 ~ 13,
    hv205==13 ~ 14,
    TRUE ~ hv205   
  )) }
# same recode for three surveys: KEHR33, KEHR3A, and KEHR42. Only KEHR33 has category 41 for hv205 and there is no category 12 in KEHR42
if (WASHdata$hv000[1] %in% c("KE2", "KE3", "KE4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="KH4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 12,
    hv205==12 ~ 14,
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for two surveys KKHR31 and KKHR42, both are hv000=KK3. Only KKHR31 has categories 12 and 22 for hv205
if (WASHdata$hv000[1]=="KK3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="KM3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="KY3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="LS4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MA2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MA4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==32 ~ 42,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MB4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==31 ~ 41,
    hv205==41 ~ 42,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: MDHR21 and MDHR31. Only MDHR21 has categories 12, 23, and 41 for hv205
if (WASHdata$hv000[1] %in% c("MD2", "MD3"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==23 ~ 22,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MD4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MD5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: MDHR61 and MDHR6A both are hv000=MD6
if (WASHdata$hv000[1]=="MD6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==23 ~ 22,
    hv205==24 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ML3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: MLHR41 and MLHR53
if (WASHdata$hv000[1]=="ML4" | (WASHdata$hv000[1]=="ML5" & WASHdata$hv007[1]==2006))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for 3 surveys: MWHR22, MWHR41, and MWHR4E. Only MWHR22 has categories 12 and 41 for hv205
if (WASHdata$hv000[1] %in% c("MW2", "MW4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MW5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MZ3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MZ4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 31,
    hv205==30 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MZ6" & WASHdata$hv007[1]==2011)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==16 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="MZ6" & WASHdata$hv007[1]==2015)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: NCHR31 and NCHR41. NCHR31 does not have category 32 for hv205 and NCHR41 does not have category 24 for hv205
if (WASHdata$hv000[1] %in% c("NC3", "NC4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==15 ~ 14,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==24 ~ 43,
    hv205==30 ~ 31,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: NGHR41 and NGHR4B. NGHR41 does not have category 32 for hv205 and NGHR4B does not have categories 12 and 23
if (WASHdata$hv000[1] %in% c("NG3", "NG4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==23 ~ 42,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NI2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NI3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NI5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NM2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==23 ~ 42,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NM4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 22,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==23 ~ 42,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NP3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==31 ~ 42,
    hv205==32 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NP4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="NP8")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==44 ~ 42,
    hv205==45 ~ 51,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PE2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==23 ~ 22,
    hv205==24 ~ 22,
    hv205==25 ~ 23,
    hv205==26 ~ 23,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PE3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==14 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==32 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PE4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==13 ~ 15,
    hv205==14 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==30 ~ 31,
    hv205==41 ~ 31,
    TRUE ~ hv205   
  )) }
# same recode for five surveys: PEHR51, PEHR5I, PEHR61, PEHR61, and PEHR6I
if (WASHdata$hv000[1] %in% c("PE5", "PE6"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==22 ~ 12,
    hv205==24 ~ 31,
    hv205==32 ~ 31,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PH2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 23,
    hv205==24 ~ 43,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: PHHR3B and PHHR41. Only PHHR3B has category 30 for hv205 and only PHHR41 has category 32
if (WASHdata$hv000[1] %in% c("PH3", "PH4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 13,
    hv205==22 ~ 23,
    hv205==30 ~ 31,
    hv205==31 ~ 43,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PH6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==51 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PH7")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==71 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PK2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 15,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="PK5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 14,
    hv205==14 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="RW2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 15,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
# same recode for three surveys: RWHR41, RWHR53, and RWHR5A
if (WASHdata$hv000[1] %in% c("RW4", "RW5"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="SL5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==71 ~ 31,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: SNHR21 and SNHR32 both are hv000=SN2 . Only survey SNHR32 has category 41 for hv205
if (WASHdata$hv000[1]=="SN2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="SN4")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 14,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for 6 surveys: SNHR61, SNHR6D, SNHR6R, SNHR7H, SNHR7I, and SNHRG0. All are hv000=SN6. 
if (WASHdata$hv000[1]=="SN6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 22,
    hv205==26 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="SZ5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: TDHR31 and TDHR41
if (WASHdata$hv000[1] %in% c("TD3", "TD4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="TD6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 12,
    hv205==12 ~ 13,
    hv205==13 ~ 14,
    hv205==14 ~ 15,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="TG3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==21 ~ 22,
    hv205==22 ~ 23,
    hv205==23 ~ 12,
    hv205==24 ~ 22,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="TR2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 13,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: TRHR41 and TRHR4A. Only survey TRHR41 has category 12 for hv205
if (WASHdata$hv000[1] %in% c("TR3", "TR4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 13,
    TRUE ~ hv205   
  )) }
# same recode for 5 surveys: TZHR21, TZHR3A, TZHR41, TZHR4A, and TZHR4I. Only surveys TZHR21 and TZHR3A have category 12 for hv205
if (WASHdata$hv000[1] %in% c("TZ2", "TZ3", "TZ4") | (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2003, 2004)))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2007,2008))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 23,
    TRUE ~ hv205   
  )) }
# same recode for three surveys: TZHR6A, TZHR7B, and TZHR7I
if (WASHdata$hv000[1] %in% c("TZ6", "TZ7"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 22,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: UGHR33 and UGHR41. Only UGHR33 has category 12 for hv205
if (WASHdata$hv000[1] %in% c("UG3", "UG4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: UGHR52 and UGHR5A.
if (WASHdata$hv000[1]=="UG5") {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==22 ~ 23,
    hv205==23 ~ 22,
    hv205==24 ~ 23,
    hv205==25 ~ 22,
    TRUE ~ hv205   
  )) }
# There are multiple surveys with country code UG6 in the year 2011 (UGHR6A and UGHR61), so need to specify maximum month of interview. UGHR61 had interviews up until hv006 (month) = 12. UGHR6A has no survey specific coding necessary.
if (WASHdata$hv000[1]=="UG6" & WASHdata$hv007[1]==2011 & max(WASHdata$hv006==12))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==22 ~ 23,
    hv205==23 ~ 22,
    hv205==24 ~ 23,
    hv205==25 ~ 22,
    hv205==44 ~ 51,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="UG6" & WASHdata$hv007[1] %in% c(2014,2015))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 22,
    hv205==25 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="UZ3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
# same recode for two surveys VNHR31 and VNHR41. Both are hv000=VNT
if (WASHdata$hv000[1]=="VNT")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="VN5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="YE2")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==13 ~ 11,
    hv205==14 ~ 15,
    hv205==24 ~ 14,
    hv205==25 ~ 23,
    hv205==26 ~ 15,
    hv205==31 ~ 42,
    hv205==32 ~ 31,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="YE6")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==24 ~ 23,
    hv205==25 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ZA3")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 42,
    hv205==22 ~ 23,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ZA7" & WASHdata$hv007[1]==2016)  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==23 ~ 21,
    hv205==44 ~ 51,
    TRUE ~ hv205   
  )) }
# same recode for three surveys: ZMHR21, ZMHR31, and ZMHR42. Only survey ZMHR21 has category 41 for hv025 and survey ZMHR42 does not have categories 41 or 12. 
if (WASHdata$hv000[1] %in% c("ZM2", "ZM3", "ZM4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    hv205==41 ~ 96,
    TRUE ~ hv205   
  )) }
# same recode for two surveys: ZWHR31 and ZWHR42. Only survey ZWHR31 has category 12 for hv205. 
if (WASHdata$hv000[1] %in% c("ZW3", "ZW4"))  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==11 ~ 15,
    hv205==12 ~ 15,
    hv205==21 ~ 23,
    hv205==22 ~ 21,
    TRUE ~ hv205   
  )) }
if (WASHdata$hv000[1]=="ZW5")  {
  WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when( 
    hv205==91 ~ 41,
    hv205==92 ~ 42,
    TRUE ~ hv205   
  )) }



# End of country specific codes

# Types of sanitation ----------------------------------------------------------

# label type of sanitation 

WASHdata <- WASHdata %>% mutate(ph_sani_type = case_when(
  is.na(ph_sani_type) ~ 99,
  TRUE ~ ph_sani_type)) %>%
  set_value_labels(ph_sani_type = 
                   c("flush - to piped sewer system" = 11,
                     "flush - to septic tank"	= 12,
                     "flush - to pit latrine"	= 13,	
                     "flush - to somewhere else" = 14,
                     "flush - don't know where/unspecified" = 15,
                     "pit latrine - ventilated improved pit (vip)" = 21,	
                     "pit latrine - with slab" = 22,
                     "pit latrine - without slab / open pit" = 23,
                     "no facility/bush/field/river/sea/lake" = 31, 		
                     "composting toilet" = 41,
                     "bucket toilet" = 42,
                     "hanging toilet/latrine" = 43,		
                     "other improved" = 51,
                     "other" = 96,
                     "missing" = 99)) %>%
  set_variable_labels(ph_sani_type = "Type of sanitation")


# create improved sanitation indicator 
WASHdata <- WASHdata %>% mutate(ph_sani_improve = case_when(
  ph_sani_type %in% c(11, 12, 13, 15, 21, 22, 41, 51) ~ 1,
  ph_sani_type %in% c(14, 23, 42, 43, 96) ~ 2,
  ph_sani_type ==31 ~ 3,
  ph_sani_type ==99 ~ NA)) %>%
  set_value_labels(ph_sani_improve = 
                   c("improved sanitation" = 1,
                     "unimproved sanitation" = 2,
                     "open defecation" = 3)) %>%
  set_variable_labels(ph_sani_improve = "Improved sanitation")

# NOTE: an older definition of improved sanitation checked to see if there was a shared toilet [hv225==1]


# create basic or limited sanitation services indicator - hv225 may not exist
WASHdata <- WASHdata %>% 
  mutate(ph_sani_basic = case_when(
  ph_sani_improve==1 & hv225==0 ~ 1,
  ph_sani_improve==1 & hv225==1 ~ 2,
  ph_sani_improve==2 ~ 3,
  ph_sani_improve==3 ~ 4)) %>%
  set_value_labels(ph_sani_basic = 
                   c("basic sanitation" = 1,
                     "limited sanitation" = 2,
                     "unimproved sanitation" = 3,
                     "open defecation" = 4)) %>% 
  set_variable_labels(ph_sani_basic = "Basic or limited sanitation")

# create sanitation facility location indicator (this variable may sometimes be country specific - e.g. sh109a in some Ghana surveys)
WASHdata <- WASHdata %>% 
  mutate(ph_sani_location = hv238a) %>%
  set_variable_labels(ph_sani_location =	"Location of sanitation facility")

