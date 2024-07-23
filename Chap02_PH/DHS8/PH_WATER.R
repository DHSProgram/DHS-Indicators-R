# /*****************************************************************************
# Program: 			PH_WATER.do
# Purpose: 			creates variable for binary improved water source according to JSTOR standard 
# Data inputs: 		hr or pr file
# Data outputs:		none
# Author of do file:	04/08/2018	Courtney Allen
# Date last modified: 07/20/2023	Courtney Allen - for codeshare project
# Note:				These indicators can also be computed using the HR or PR file.
# 					If you want to produce estimates for households, use the HR file.
# 					If you want to produce estimates for the de jure population, 
# 					use the PR file and select for dejure household memebers using
# 					hv102==1. Please see the Guide to DHS Statistics.  
# 					
# 					*****
# *****************************************************************************
# 
# NOTES AND VARIABLE LIST ------------------------------------------------------
# This do file can be run on any loop of countries indicating the dataset name 
# with variable called filename. Code should be same for pr or hr files.
# 
# VARIABLES CREATED
# 	ph_wtr_trt_boil		"Treated water by boiling before drinking"
# 	ph_wtr_trt_chlor	"Treated water by adding bleach or chlorine before drinking"
# 	ph_wtr_trt_cloth	"Treated water by straining through cloth before drinking"
# 	ph_wtr_trt_filt		"Treated water by ceramic, sand, or other filter before drinking"
# 	ph_wtr_trt_solar	"Treated water by solar disinfection before drinking"
# 	ph_wtr_trt_stand	"Treated water by letting stand and settle before drinking"
# 	ph_wtr_trt_other	"Treated water by other means before drinking"
# 	ph_wtr_trt_none		"Did not treat water before drinking"
# 	ph_wtr_trt_appr		"Appropriately treated water before drinking"
# 	ph_wtr_source 		"Source of drinking water"
# 	ph_wtr_improve 		"Improved drinking water" 
# 	ph_wtr_time			"Round trip time to obtain drinking water"
# 	ph_wtr_basic		"Basic water service"
# 	ph_wtr_avail		"Availability of water among those using piped water or water from tube well or borehole"
#   interview season  "Interview in dry or rainy season"
# 
# NOTE: 
# STANDARD CATEGORIES FOR WATER SOURCE BY IMPROVED/UNIMPROVED
# 	0-unimproved 
# 		30	 well - protection unspecified	
# 		32	 unprotected well
# 		40	 spring - protection unspecified
# 		42	 unprotected spring
# 		43	 surface water (river/dam/lake/pond/stream/canal/irrigation channel)
# 		96	 other	
# 	1-improved
# 		11	 piped into dwelling
# 		12	 piped to yard/plot
# 		13	 public tap/standpipe
# 		14	 piped to neighbor
# 		15	 piped outside of yard/lot
# 		21	 tube well or borehole
# 		31	 protected well
# 		41	 protected spring
# 		51	 rainwater
# 		61	 tanker truck	
# 		62	 cart with small tank, cistern, drums/cans
# 		65	 purchased water	
# 		71	 bottled water
# 		72	 purified water, filtration plant
# 		73	 satchet water
# 	
# *****************************************************************************



# create water treatment indicators --------------------------------------------


# treated water by boiling
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_boil = case_when(
  hv237a>=8 ~ 0,
  TRUE ~ hv237a)) %>%
  set_value_labels(ph_wtr_trt_boil = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_boil =	"Treated water by boiling before drinking")

#	treated water by adding bleach or chlorine
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_chlor = case_when(
  hv237b>=8 ~ 0,
  TRUE ~ hv237b)) %>%
  set_value_labels(ph_wtr_trt_chlor = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_chlor = "Treated water by adding bleach or chlorine before drinking")

#	treated water by straining through cloth
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_cloth = case_when(
  hv237c>=8 ~ 0,
  TRUE ~ hv237c)) %>%
  set_value_labels(ph_wtr_trt_cloth = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_cloth = "Treated water by straining through cloth before drinking")

# treated water by ceramic, sand, or other filter
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_filt = case_when(
  hv237d>=8 ~ 0,
  TRUE ~ hv237d)) %>%
  set_value_labels(ph_wtr_trt_filt = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_filt = "Treated water by ceramic, sand, or other filter before) drinking")

# treated water by solar disinfection
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_solar = case_when(
  hv237e>=8 ~ 0,
  TRUE ~ hv237e)) %>%
  set_value_labels(ph_wtr_trt_solar = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_solar ="Treated water by solar disinfection")

# treated water by letting stand and settle
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_stand = case_when(
  hv237f>=8 ~ 0,
  TRUE ~ hv237f)) %>%
  set_value_labels(ph_wtr_trt_stand = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_stand ="Treated water by letting stand and settle before drinking")

# treated water by other means
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_other = case_when(
  hv237g==1 ~ 1,
  hv237h==1 ~ 1,
  hv237j==1 ~ 1,
  hv237k==1 ~ 1,
  hv237x==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(ph_wtr_trt_other = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_other = "Treated water by other means before drinking")

# any treatment or none
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_none = case_when(
  hv237!=0 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(ph_wtr_trt_none = c(
    "No treatment" = 0,
    "Some treatment" = 1)) %>%
  set_variable_labels(ph_wtr_trt_none =	"Did not treat water before drinking")

# using appropriate treatment
# NOTE: Appropriate water treatment includes: boil, add bleach or chlorine,	ceramic, sand or other filter, and solar disinfection.

WASHdata <- WASHdata %>% mutate(ph_wtr_trt_appr = case_when(
  hv237a==1 ~ 1,
  hv237b==1 ~ 1,
  hv237d==1 ~ 1,
  hv237e==1 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(ph_wtr_trt_appr = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_appr = "Appropriately treated water before drinking")

# time to obtain drinking water (round trip)
WASHdata <- WASHdata %>% mutate(ph_wtr_time = case_when(
  hv204 %in% c(0, 996) ~ 0,
  between(hv204, 1, 30) ~ 1,
  between(hv204, 31,900) ~ 2,
  hv204>=998 ~ 3)) %>%
  set_value_labels(ph_wtr_time = 
                     c("water on premises" = 0,
                       "30 minutes or less" = 1,
                       "More than 30 minutes" = 2,
                       "don't know" = 3)) %>%
  set_variable_labels(ph_wtr_time = "Round trip time to obtain water")



# generate water source indicator ----------------------------------------------

# create a variable for water source, this var will be overwritten if country-specific coding is needed
WASHdata <- WASHdata %>% mutate(ph_sani_type = hv205)


# country-specific coding ------------------------------------------------------
if (WASHdata$hv000[1]=="AF7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="AL7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="AM4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="AM4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==32 ~ 41,
    hv201==41 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="AM6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 11,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="AM7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="AO7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==33 ~ 21,
    hv201==63 ~ 62,
    TRUE ~ hv201  
  )) }
# same recode for 3 surveys that all have hv000=BD3 (BDHR31, BDHR3A, and BDHR41). BDHR41 does not have category 41 for hv201
if (WASHdata$hv000[1]=="BD3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==31 ~ 43,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BD4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==23 ~ 31,
    hv201==24 ~ 32,
    hv201==41 ~ 43,
    hv201==42 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BD7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BF2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="BF3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 11,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==51 ~ 71,
    hv201==61 ~ 65,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="BF4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BF7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BJ3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==31 ~ 41,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==42 ~ 51,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BJ4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==52 ~ 51,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="BJ5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==52 ~ 51,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BJ7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]<95)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==13 ~ 14,
    hv201==21 ~ 30,
    hv201==32 ~ 43,
    hv201==51 ~ 61,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]==98)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 15,
    hv201==21 ~ 30,
    hv201==31 ~ 43,
    hv201==51 ~ 61,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="BO4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 15,
    hv201==22 ~ 32,
    hv201==42 ~ 43,
    hv201==45 ~ 14,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BO5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BR2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="BR3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==61 ~ 71,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="BU7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="CD5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==34 ~ 32,
    hv201==35 ~ 32,
    hv201==36 ~ 32,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="CF3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="CG5" & WASHdata$hv007[1]==2005)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==42 ~ 43,
    TRUE ~ hv201   
  )) }
# same recode for two surveys: CIHR35 and CIHR3A both are hv000=CI3. Only survey CIHR35 has categories 51 and 61 for hv201
if (WASHdata$hv000[1]=="CI3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201
  ))}
if (WASHdata$hv000[1]=="CI5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==42 ~ 40,
    hv201==44 ~ 43,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="CM2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==22 ~ 32,
    hv201==31 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 96,
    hv201==61 ~ 71,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="CM3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==22 ~ 32,
    hv201==31 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 65,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="CM4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 15,
    hv201==22 ~ 32,
    hv201==31 ~ 32,
    hv201==41 ~ 43,
    hv201==42 ~ 41,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="CM7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==92 ~ 73,
    TRUE ~ hv201 
  )) }
# same recode for two surveys: COHR22 and COHR31. Only survey COHR22 has category 71 for hv201
if (WASHdata$hv000[1] %in% c("CO2", "CO3"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 11,
    hv201==13 ~ 15,
    hv201==14 ~ 13,
    hv201==21 ~ 30,
    hv201==31 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]==2000)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 11,
    hv201==21 ~ 30,
    hv201==41 ~ 43,
    TRUE ~ hv201   
  )) }
# same recode for two surveys COHR53, COHR61, COHR72
if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]>=2004 | (WASHdata$hv000[1] %in% c("CO5", "CO7")))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 11,
    hv201==22 ~ 32,
    hv201==42 ~ 43,
    TRUE ~ hv201 
  )) }
# same recode for two surveys: DRHR21 and DRHR32. Only survey DRHR21 has category 71 for hv201
if (WASHdata$hv000[1]=="DR2" | (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==96))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==31 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==99 )  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==25 ~ 31,
    hv201==26 ~ 31,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="DR4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==41 ~ 43,
    TRUE ~ hv201 
    )) }
# same recode for two surveys: EGHR21 and EGHR33. Only survey EGHR21 has category 71 for hv201
if (WASHdata$hv000[1] %in% c("EG2", "EG3"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 43,
    hv201==71 ~ 96,
    TRUE ~ hv201   
  )) }
# same recode for two surveys: EGHR42 and EGHR4A. Both surveys are hv000=EG4. Only survey EGHR42 has category 72 for hv201
if (WASHdata$hv000[1]=="EG4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 30,
    hv201==31 ~ 30,
    hv201==32 ~ 30,
    hv201==33 ~ 30,
    hv201==41 ~ 43,
    hv201==72 ~ 65,
    TRUE ~ hv201   
  )) }
# this is survey EGHR51 which is also hv000=EG4 as the previous two surveys. Use hv007=2005 to specify 
if (WASHdata$hv000[1]=="EG4" & WASHdata$hv007[1]==2005)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 42,
    hv201==31 ~ 21,
    hv201==32 ~ 31,
    hv201==33 ~ 41,
    hv201==41 ~ 43,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1992)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 15,
    hv201==21 ~ 32,
    hv201==22 ~ 42,
    hv201==23 ~ 31,
    hv201==24 ~ 41,
    hv201==31 ~ 43,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1997)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 15,
    hv201==21 ~ 32,
    hv201==22 ~ 42,
    hv201==31 ~ 21,
    hv201==32 ~ 31,
    hv201==33 ~ 41,
    hv201==41 ~ 43,
    TRUE ~ hv201    
  )) }
# same recode for ETHR71 and ETHR81
if (WASHdata$hv000[1]=="ET7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="GA3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 31,
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==24 ~ 32,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    hv201==61 ~ 71,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="GA6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==32 ~ 31,
    hv201==33 ~ 32,
    hv201==34 ~ 32,
    TRUE ~ hv201   
  )) }
# same recode for two surveys: GHHR31 and GHHR41. Only survey GHHR41 has category 61 for hv201
if (WASHdata$hv000[1] %in% c("GH2", "GH3"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==35 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="GH4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    hv201==81 ~ 73,
    TRUE ~ hv201 
  )) }
# same recode for two surveys: GHHR5A and GHHR72
if (WASHdata$hv000[1] %in% c("GH5", "GH6"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==72 ~ 73,
    TRUE ~ hv201   
  )) }
# same recode for two surveys: GHHR7B and GHHR82. Both are hv000=GH7
if (WASHdata$hv000[1]=="GH7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="GM7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="GN3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 41,
    hv201==32 ~ 42,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==35 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="GN4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==34 ~ 21,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    TRUE ~ hv201   
  )) }
# same recode for GNHR71 and GNHR81. Both are hv000==GN7
if (WASHdata$hv000[1]=="GN7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]==95)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 13,
    hv201==12 ~ 13,
    hv201==13 ~ 15,
    hv201==22 ~ 30,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]==98)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 13,
    hv201==12 ~ 13,
    hv201==13 ~ 15,
    hv201==14 ~ 13,
    hv201==21 ~ 30,
    hv201==31 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="GU6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 15,
    hv201==31 ~ 13,
    hv201==32 ~ 30,
    hv201==41 ~ 43,
    hv201==42 ~ 43,
    hv201==43 ~ 41,
    hv201==44 ~ 42,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="GY4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==81 ~ 43,
    hv201==91 ~ 62,
    hv201==92 ~ 72,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="HN5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 11,
    hv201==14 ~ 11,
    hv201==21 ~ 32,
    hv201==31 ~ 30,
    hv201==32 ~ 21,
    hv201==41 ~ 43,
    hv201==62 ~ 13,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="HN6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 11,
    hv201==14 ~ 11,
    hv201==31 ~ 30,
    hv201==44 ~ 13,
    hv201==45 ~ 43,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="HT3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==35 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==52 ~ 65,
    hv201==61 ~ 71,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="HT4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 30,
    hv201==32 ~ 30,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    hv201==81 ~ 65,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="HT5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==63 ~ 43,
    hv201==64 ~ 65,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="HT6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==32 ~ 31,
    hv201==33 ~ 32,
    hv201==34 ~ 32,
    hv201==72 ~ 65,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="HT7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 65,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="IA2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==24 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="IA3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==22 ~ 21,
    hv201==23 ~ 30,
    hv201==24 ~ 32,
    hv201==25 ~ 31,
    hv201==26 ~ 32,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="IA7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==92 ~ 72,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ID2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==71 ~ 96,
    TRUE ~ hv201  
  )) }
# same recode for two surveys: IDHR31 (1994) and IDHR3A (1997). Both are hv000=ID3
if (WASHdata$hv000[1]=="ID3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==31 ~ 41,
    hv201==32 ~ 42,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ID4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
# same recode for two surveys: IDHR51 (2002) and IDHR63 (2007). Only IDHR63 has category 81 for hv201
if (WASHdata$hv000[1] %in% c("ID5", "ID6"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==33 ~ 32,
    hv201==34 ~ 32,
    hv201==35 ~ 32,
    hv201==36 ~ 31,
    hv201==37 ~ 31,
    hv201==38 ~ 31,
    hv201==44 ~ 40,
    hv201==45 ~ 43,
    hv201==46 ~ 43,
    hv201==47 ~ 43,
    hv201==81 ~ 72,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="ID7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 71,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="JO3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="JO4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==41 ~ 40,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KE2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==22 ~ 32,
    hv201==31 ~ 43,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==71 ~ 96,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="KE3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 43,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KE4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201    )) }
if (WASHdata$hv000[1]=="KE6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KE7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="KH4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 21,
    hv201==34 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="KH8")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KK3" & WASHdata$hv007[1]==95)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==51 ~ 61,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KK3" & WASHdata$hv007[1]==99)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==24 ~ 43,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KM3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    hv201==42 ~ 51,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="KY3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==51 ~ 61,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="LB7" & WASHdata$hv007[1]==2016)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="LB7" & WASHdata$hv007[1] >=2019)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==92 ~ 73,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="LS4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==34 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="LS5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MA2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="MA4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MB4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==63 ~ 62,
    hv201==81 ~ 41,
    hv201==82 ~ 42,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MD2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MD3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 30,
    hv201==23 ~ 32,
    hv201==24 ~ 21,
    hv201==25 ~ 30,
    hv201==26 ~ 32,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="MD4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201   
  )) }
# same recode for two surveys: MDHR7- (2016) and MDHR8- (2021), both have hv000==MD7
if (WASHdata$hv000[1]=="MD7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ML3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="ML4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
# same recode for two surveys: MLHR7- (2018) and MDHR8- (2021), both have hv000==ML7
if (WASHdata$hv000[1]=="ML7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MM7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MR7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="MV5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==52 ~ 51,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="MV7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    hv201==52 ~ 51,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="MW2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==13 ~ 12,
    hv201==22 ~ 30,
    hv201==23 ~ 31,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="MW4" & WASHdata$hv007[1]==2000)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==32 ~ 21,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="MW4" & WASHdata$hv007[1] %in% c(2004,2005))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201  
  )) }
# same recode for two surveys: MWHR7A (2015) and MWHR7I (2017). Both are hv000=MW7
  if (WASHdata$hv000[1]=="MW7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="MZ3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 14,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="MZ4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 14,
    hv201==21 ~ 12,
    hv201==22 ~ 14,
    hv201==23 ~ 32,
    hv201==41 ~ 43,
    TRUE ~ hv201  
  )) }
# same recode for two surveys: MZHR62 (2011) and MZHR71 (2015). Both are hv000=MZ6
if (WASHdata$hv000[1]=="MZ6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==33 ~ 21,
  TRUE ~ hv201    )) }
if (WASHdata$hv000[1]=="MZ7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="NC3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 43,
    hv201==32 ~ 40,
    hv201==41 ~ 51,
    hv201==61 ~ 65,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NC4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==31 ~ 30,
    hv201==32 ~ 30,
    hv201==41 ~ 43,
    hv201==42 ~ 40,
    hv201==61 ~ 72,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NG3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==52 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 21,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="NG4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    hv201==62 ~ 65,
    TRUE ~ hv201 
  )) }
# same recode for three surveys: NGHR61 (2010), NGHR6A (2013), and NGHR71 (2015). All are hv000=NG6
if (WASHdata$hv000[1]=="NG6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==72 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NG7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==92 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NG8")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NI2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NI3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==24 ~ 32,
    hv201==25 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 62,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NI5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==41 ~ 40,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NI6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==63 ~ 65,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NI7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NM2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==35 ~ 21,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NM4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 42,
    hv201==31 ~ 21,
    hv201==32 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="NP3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==24 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==34 ~ 41,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NP4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 21,
    hv201==32 ~ 21,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==43 ~ 41,
    TRUE ~ hv201 
  )) }
#	same recode for two surveys: NPHR51 (2006) and NPHR61 (2011). 
if (WASHdata$hv000[1] %in% c("NP5", "NP6"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==44 ~ 41,
    TRUE ~ hv201   
  )) }
#	same recode for two surveys: NPHR7H (2016) and NPHR81 (2022). 
if (WASHdata$hv000[1] %in% c("NP7", "NP8"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="NP8")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201    
    )) }
if (WASHdata$hv000[1]=="PE2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==13 ~ 12,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="PE3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 11,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201   
  )) }
# same recode for six surveys: PEHR41,51,5I,61,6A,and 6I. The last three surveys all are hv000=PE6. Only survey PEHR41 has category 42 for hv201
if (WASHdata$hv000[1] %in% c("PE4", "PE5", "PE6"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="PG7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="PH2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 11,
    hv201==22 ~ 12,
    hv201==23 ~ 30,
    hv201==24 ~ 30,
    hv201==31 ~ 32,
    hv201==71 ~ 96,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="PH3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 31,
    hv201==22 ~ 32,
    hv201==31 ~ 41,
    hv201==32 ~ 42,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==35 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="PH4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    FTRUE ~ hv201  
  )) }
# same recode for two surveys: PHHR52 (2008) and PHHR61 (2013). Only survey PHHR52 has categories 72 and 73 fpr hv201 
if (WASHdata$hv000[1] %in% c("PH5", "PH6"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==33 ~ 32,
    hv201==72 ~ 14,
    hv201==73 ~ 14,
    TRUE ~ hv201
  )) }
# same recode for two surveys: PHHR71 (2017) and PHHR81 (2022)	
if (WASHdata$hv000[1] %in% c("PH7", "PH8"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="PK2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==13 ~ 12,
    hv201==23 ~ 21,
    hv201==24 ~ 32,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201  
  )) }
# same recode for two surveys: PKHR52 (2006) and PKHR61 (2012). Only survey PKHR61 has category 63 for hv201
if (WASHdata$hv000[1] %in% c("PK5", "PK6"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 21,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="PK7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==63 ~ 72,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="RW2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==13 ~ 12,
    hv201==23 ~ 21,
    hv201==24 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201    
  )) }
# same recode for three surveys: RWHR41, RWHR53, and RWHR5A. Survey RWHR41 does not have category 21 for hv201
if (WASHdata$hv000[1] %in% c("RW4", "RW5"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201
  )) }
# same recode for two surveys: RWHR7- (2017) and RWHR8- (2019-20), both have hv000=="RW7"
  if (WASHdata$hv000[1]=="RW7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201
    )) }
# same recode for two surveys: SLHR73 (2016) and SLHR7A- (2019), both have hv000=="SL7"
if (WASHdata$hv000[1]=="SL7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201
  )) }
# same recode for two surveys: SNHR21 (1992-93) and SNHR32 (1997). Both are hv000=SN2. Only survey SNHR32 has categories 34, 41, and 61 for variable hv201
if (WASHdata$hv000[1]=="SN2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201
  )) }
# same recode for two surveys: SNHR4A (2005) and SNHR51 (2006).
if (WASHdata$hv000[1] %in% c("SN4", "SN5"))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201
  )) }
# same recode for four surveys: SNHR7Z (2017), SNHR80 (2018), SNHR8B (2019), SNHR8I (2020-21), all are hv000==SN7
if (WASHdata$hv000[1]=="SN7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TD3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 32,
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==24 ~ 31,
    hv201==31 ~ 43,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 65,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TD4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==44 ~ 43,
    hv201==52 ~ 65,
    hv201==53 ~ 65,
    hv201==54 ~ 65,
    hv201==55 ~ 65,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TG3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 14,
    hv201==22 ~ 31,
    hv201==23 ~ 32,
    hv201==31 ~ 41,
    hv201==32 ~ 43,
    hv201==41 ~ 51,
    hv201==42 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TG6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==72 ~ 73,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TG7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==72 ~ 73,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TJ7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TL7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TR2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==42 ~ 43,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 96,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TR3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 41,
    hv201==32 ~ 40,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    hv201==71 ~ 72,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TR4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==21 ~ 30,
    hv201==31 ~ 30,
    hv201==42 ~ 40,
    hv201==81 ~ 72,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TR7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 13,
    TRUE ~ hv201
  )) }
# same recode for two surveys: TZHR21 (1991-92) and TZHR3A (1996). Only survey TZHR21 has categories 51 and 71 for hv201
if (WASHdata$hv000[1]=="TZ2" | (WASHdata$hv000[1]=="TZ3" & WASHdata$hv007[1]==96))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==35 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TZ3" & WASHdata$hv007[1]==99)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 31,
    hv201==23 ~ 21,
    hv201==31 ~ 41,
    hv201==32 ~ 42,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2003,2004))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==32 ~ 21,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TZ4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==24 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==34 ~ 21,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    hv201==62 ~ 65,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2007,2008))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==24 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==34 ~ 21,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    hv201==62 ~ 65,
    hv201==91 ~ 62,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2009, 2010))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==24 ~ 32,
    hv201==25 ~ 32,
    hv201==33 ~ 31,
    hv201==34 ~ 31,
    hv201==35 ~ 31,
    hv201==36 ~ 21,
    hv201==45 ~ 40,
    TRUE ~ hv201
  )) }
#	same recode for two surveys: TZHR7B (2015-16) and TZHR7I (2017), both are hv000=TZ7
if (WASHdata$hv000[1]=="TZ7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 41,
    hv201==41 ~ 51,
    hv201==61 ~ 71,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 21,
    hv201==34 ~ 21,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    hv201==81 ~ 41,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG5" & WASHdata$hv007[1]==2006)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 21,
    hv201==23 ~ 21,
    hv201==33 ~ 31,
    hv201==34 ~ 31,
    hv201==35 ~ 32,
    hv201==36 ~ 32,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    hv201==46 ~ 43,
    hv201==91 ~ 41,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG5" & WASHdata$hv007[1] %in% c(2009,2010))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==33 ~ 31,
    hv201==34 ~ 31,
    hv201==35 ~ 21,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    hv201==46 ~ 43,
    TRUE ~ hv201
  )) }
# same recode can be used for two surveys: UGHR61 and UGHR6A. Only survey UGHR61 has categories 22,23,33,34,35,36,44,45 and 46 and only survey UGHR6A has category 81 for hv201. Both surveys are hv000=UG6 and both are also hv007=2011
if (WASHdata$hv000[1]=="UG6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 21,
    hv201==23 ~ 21,
    hv201==33 ~ 31,
    hv201==34 ~ 31,
    hv201==35 ~ 32,
    hv201==36 ~ 32,
    hv201==44 ~ 43,
    hv201==45 ~ 43,
    hv201==46 ~ 43,
    hv201==81 ~ 41,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG6" & WASHdata$hv007[1] %in% c(2014,2015))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 21,
    hv201==44 ~ 41,
    hv201==63 ~ 62,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG7" & WASHdata$hv007[1]==2016)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==63 ~ 62,
    hv201==72 ~ 73,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UG7" & WASHdata$hv007[1] %in% c(2018,2019))  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    hv201==63 ~ 62,
    hv201==72 ~ 73,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="UZ3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201
  )) }
# same recode for two surveys VNHR31 and VNHR41. Both are hv000=VNT. Only survey VNHR31 has category 61 for hv201
if (WASHdata$hv000[1]=="VNT")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="VNT")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==34 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="VN5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==31 ~ 30,
    hv201==32 ~ 30,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    hv201==44 ~ 43,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="YE2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 11,
    hv201==14 ~ 12,
    hv201==23 ~ 21,
    hv201==24 ~ 32,
    hv201==32 ~ 43,
    hv201==35 ~ 51,
    hv201==36 ~ 43,
    hv201==71 ~ 96,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="YE6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==14 ~ 72,
    hv201==15 ~ 72,
    hv201==32 ~ 30,
    hv201==43 ~ 40,
    hv201==44 ~ 41,
    hv201==45 ~ 43,
    hv201==72 ~ 62,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="ZA3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==31 ~ 43,
    hv201==41 ~ 51,
    hv201==51 ~ 61,
    hv201==61 ~ 71,
    TRUE ~ hv201
  )) }
if (WASHdata$hv000[1]=="ZA7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ZM2")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 30,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==51 ~ 61,
    hv201==71 ~ 96,
    TRUE ~ hv201 
  )) }
if (WASHdata$hv000[1]=="ZM3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 30,
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==24 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="ZM4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==22 ~ 32,
    hv201==23 ~ 32,
    hv201==24 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ZM7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ZW3")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==12 ~ 13,
    hv201==21 ~ 31,
    hv201==22 ~ 32,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    TRUE ~ hv201  
  )) }
if (WASHdata$hv000[1]=="ZW4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==21 ~ 31,
    hv201==22 ~ 32,
    hv201==23 ~ 21,
    hv201==31 ~ 40,
    hv201==32 ~ 43,
    hv201==33 ~ 43,
    hv201==41 ~ 51,
    TRUE ~ hv201   
  )) }
if (WASHdata$hv000[1]=="ZW5")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==71 ~ 62,
    hv201==81 ~ 43,
    TRUE ~ hv201   
    )) }
if (WASHdata$hv000[1]=="ZW7")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when( 
    hv201==13 ~ 14,
    hv201==14 ~ 13,
    TRUE ~ hv201  
  )) }

 
# special code for Cambodia ----------------------------------------------------
  
# NOTE: Cambodia collects data on water source for both the wet season and dry season. Below, an indicator is created for the dry season and water source and a wet	season water source. For all following indicators that use water source, only	the water source that corresponds to the season of interview (hv006 = month	of interview) is used.

# 	e.g. If the interview took place during the dry season, then the dry season	water source is used for standard indicators in this code. 


if (WASHdata$hv000[1]=="KH4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 21,
    hv201==34 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201  
  )) }

if (WASHdata$hv000[1]=="KH4")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source_dry= case_when( 
    hv201==11 ~ 12,
    hv201==12 ~ 13,
    hv201==21 ~ 32,
    hv201==22 ~ 32,
    hv201==32 ~ 31,
    hv201==33 ~ 21,
    hv201==34 ~ 31,
    hv201==41 ~ 40,
    hv201==42 ~ 43,
    TRUE ~ hv201    
  )) }
if (WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]<=2006)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet = hv201w)
}
if (WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]<=2006)  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source_dry = hv201d)
}
# KHHR61 and KHHR73 used the same variables for wet and dry season
if ((WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]>=2010) | WASHdata$hv000[1]=="KH6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet = sh104b)
}
if ((WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]>=2010) | WASHdata$hv000[1]=="KH6")  {
  WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet = sh102)
}


# check if interview took place in dry season or wet season
if (WASHdata$hv000[1] %in% c("KH4", "KH5", "KH6")) {
  WASHdata <- WASHdata %>% mutate(interview_season = case_when(
    hv006 %in% c(2, 3, 4, 11, 12) ~ 1,
    hv006 %in% c(5, 6, 7, 8, 9, 10) ~ 2)) %>%
    set_value_labels(interview_season = c(
      "dry season" = 1,
      "wet season" = 0)) %>%
    set_variable_labels(interview_season = "Interview in dry or rainy season") %>%
    # now replace water_source variable with the variable that matches the interview season
    mutate(ph_water_source = case_when(
    interview_season==1 ~ ph_wtr_source_dry,
    interview_season==2 ~ ph_wtr_source_wet))
}

# create water source indicators -----------------------------------------------
	
# create water source labels
WASHdata <- WASHdata %>% mutate(ph_wtr_source = case_when(
  is.na(ph_wtr_source) ~ 99,
  TRUE ~ ph_wtr_source)) %>%
  set_value_labels(ph_wtr_source =
                 c("piped into dwelling" = 11,
                   "piped to yard/plot" = 12,
                   "public tap/standpipe" = 13,
                   "piped to neighbor" = 14,
                   "piped outside of yard/lot" = 15,
                   "tube well or borehole" = 21,
                   "well - protection unspecified" = 30,
                   "protected well" = 31,
                   "unprotected well" = 32,
                   "spring - protection unspecified" = 40,
                   "protected spring" = 41,
                   "unprotected spring" = 42,
                   "surface water (river/dam/lake/pond/stream/canal/irrigation channel)" = 43,
                   "rainwater" = 51,
                   "tanker truck" = 61,
                   "cart with small tank, cistern, drums/cans" = 62,
                   "purchased water" = 65,
                   "bottled water" = 71,
                   "purified water, filtration plant" = 72,
                   "satchet water" = 73,
                   "other" = 96,
                   "missing" = 99)) %>%
set_variable_labels(ph_wtr_source = "Source of drinking water")
	
# improved water source
WASHdata <- WASHdata %>% mutate(ph_wtr_improve = case_when(
  ph_wtr_source %in% c(11, 12, 13, 14, 15, 21, 31, 41, 51, 61, 62, 65, 71, 72, 73) ~ 1,
  ph_wtr_source %in% c(30, 32, 40, 42, 43, 96) ~ 0,
  ph_wtr_source==99 ~ 99)) %>%
  set_value_labels(ph_wtr_improve = c(
    "improved" = 1,
    "unimproved/surface water" = 0,
    "missing" = 99)) %>%
  set_variable_labels(ph_wtr_improve = "Improved Water Source")

# basic or limited water source
WASHdata <- WASHdata %>% mutate(ph_wtr_basic = case_when(
  ph_wtr_improve==1 & ph_wtr_time<=1 ~ 1,
  ph_wtr_improve==1 & ph_wtr_time>1 ~ 2,
  ph_wtr_improve==0 ~ 3)) %>%
  set_value_labels(ph_wtr_basic = 
                     c("basic water services" = 1,
                       "limited water services" = 2,
                       "unimproved water source" = 3)) %>%
  set_variable_labels(ph_wtr_basic = "Basic or limited water services")

# availability of piped water or water from tubewell
# NOTE this var may not exist in every dataset, so use "try()"
WASHdata <- WASHdata %>% 
  try(mutate(ph_wtr_basic = hv201a)) %>%
  try(set_variable_labels(ph_wtr_avail = "Availability of water among those using piped water or water from tube well or borehole"))

