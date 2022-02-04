# /*****************************************************************************************************
# Program: 			  PH_HOUS.R
# Purpose: 			  Code to compute household characteristics, possessions, and smoking in the home
# Data inputs: 		HR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: November 16, 2021 by Shireen Assaf 
# Note:				  These indicators can also be computed using the PR file but you would need to select for dejure household members
# 					    using hv102==1. Please see the Guide to DHS Statistics.  
# 				    	There may be some other country specific household possessions available in the dataset that may not be coded here. 
# *****************************************************************************************************/
# 
# ----------------------------------------------------------------------------
# Variables created in this file:
# 
# ph_electric		  "Have electricity"
# ph_floor		    "Flooring material"
# ph_rooms_sleep	"Rooms for sleeping"
# ph_cook_place	  "Place for cooking"
# ph_cook_fuel	  "Type fo cooking fuel"
# ph_cook_solid	  "Using solid fuel for cooking"
# ph_cook_clean	  "Using clean fuel for cooking"
# 	
# ph_smoke		    "Frequency of smoking at home"	
# 
# ph_radio		    "Owns a radio"
# ph_tv			      "Owns a tv"
# ph_mobile		    "Owns a mobile phone"
# ph_tel			    "Owns a non-mobile telephone"
# ph_comp			    "Owns a computer"
# ph_frig			    "Owns a refrigerator"
# ph_bike			    "Owns a bicycle"
# ph_cart			    "Owns a animal drawn cart"
# ph_moto			    "Owns a motorcycle/scooter"
# ph_car			    "Owns a car or truck"
# ph_boat			    "Owns a boat with a motor"
# ph_agriland		  "Owns agricultural land"
# ph_animals		  "Owns livestock or farm animals"
# ----------------------------------------------------------------------------

HRdata <- HRdata %>%
  mutate(wt = hv005/1000000)

# *** Household characteristics ***
# 
# //Have electricity
HRdata <- HRdata %>%
  mutate(ph_electric = hv206) %>%
  set_value_labels(ph_electric = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_electric = "Have electricity")

# //Flooring material
HRdata <- HRdata %>%
  mutate(ph_floor = hv213) %>%
  set_variable_labels(ph_floor = "Flooring material")

# //Number of rooms for sleeping
HRdata <- HRdata %>%
  mutate(ph_rooms_sleep =
           case_when(
             hv216==1  ~ 1 ,  
             hv216==2  ~ 2 ,
             hv216>=3 ~ 3,
             hv216==0 | is.na(hv216) ~ 9)) %>%
  set_value_labels(ph_rooms_sleep = c( "Missing"=9, "Three or more" =3, "Two" =2, "One" =1)) %>%
  set_variable_labels(ph_rooms_sleep = "Rooms for sleeping")

# //Place for cooking
HRdata <- HRdata %>%
  mutate(ph_cook_place =
           case_when(
             hv241<9  ~ hv241 ,
             hv226==95  ~ 4,
             hv241>=9 | is.na(hv241) ~ 9)) %>%
  set_value_labels(ph_cook_place = c("Missing"=9, "Other" =6, "No food cooked in household" =4, "Outdoors" =3, "In a seperate building"=2, "In the house"=1)) %>%
  set_variable_labels(ph_cook_place = "Place for cooking")

# //Type of cooking fuel
HRdata <- HRdata %>%
  mutate(ph_cook_fuel = hv226) %>%
  set_variable_labels(ph_cook_fuel = "Type of cooking fuel")

# //Solid fuel for cooking
HRdata <- HRdata %>%
  mutate(ph_cook_solid =
           case_when(
             hv226<6 | hv226>11 | is.na(hv226) ~ 0 ,
             hv226>=6 & hv226<=11 ~ 1)) %>%
  set_value_labels(ph_cook_solid = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_cook_solid = "Using solid fuel for cooking")

# //Clean fuel for cooking
HRdata <- HRdata %>%
  mutate(ph_cook_clean =
           case_when(
             hv226>4 | is.na(hv226) ~ 0 ,
             hv226<=4 ~ 1)) %>%
  set_value_labels(ph_cook_clean = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_cook_clean = "Using clean fuel for cooking")

# //Frequency of smoking in the home
HRdata <- HRdata %>%
  mutate(ph_smoke =
           case_when(
             hv252<9  ~ hv252 ,
             hv252>=9 | is.na(hv252) ~ 9)) %>%
  set_value_labels(ph_smoke = c("Missing" = 9, "Never"=0, "Daily"=1, "Weekly"=2, "Monthlly"=3, "Less than once a month"=4)) %>%
  set_variable_labels(ph_smoke = "Frequency of smoking at home")

# *** Household possessions ***

# //Radio
HRdata <- HRdata %>%
  mutate(ph_radio = 
           case_when(
             hv207==0 | is.na(hv207) ~ 0,
             hv207==1 ~ 1)) %>%
  set_value_labels(ph_radio = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_radio = "Owns a radio")

# //TV
HRdata <- HRdata %>%
  mutate(ph_tv = 
           case_when(
             hv208==0 | is.na(hv208) ~ 0,
             hv208==1 ~ 1)) %>%
  set_value_labels(ph_tv = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_tv = "Owns a TV")

# //Mobile phone
HRdata <- HRdata %>%
  mutate(ph_mobile = 
           case_when(
             hv243a==0 | is.na(hv243a) ~ 0,
             hv243a==1 ~ 1)) %>%
  set_value_labels(ph_mobile = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_mobile = "Owns a mobile phone")

# //Non-mobile phone
HRdata <- HRdata %>%
  mutate(ph_tel = 
           case_when(
             hv221==0 | is.na(hv221) ~ 0,
             hv221==1 ~ 1)) %>%
  set_value_labels(ph_tel = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_tel = "Owns a non-mobile telephone")

# //Computer
HRdata <- HRdata %>%
  mutate(ph_comp = 
           case_when(
             hv243e==0 | is.na(hv243e) ~ 0,
             hv243e==1 ~ 1)) %>%
  set_value_labels(ph_comp = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_comp = "Owns a computer")

# //Refrigerator
HRdata <- HRdata %>%
  mutate(ph_frig = 
           case_when(
             hv209==0 | is.na(hv209) ~ 0,
             hv209==1 ~ 1)) %>%
  set_value_labels(ph_frig = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_frig = "Owns a refrigerator")

# //Bicycle
HRdata <- HRdata %>%
  mutate(ph_bike = 
           case_when(
             hv210==0 | is.na(hv210) ~ 0,
             hv210==1 ~ 1)) %>%
  set_value_labels(ph_bike = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_bike = "Owns a bicycle")

# //Animal drawn cart
HRdata <- HRdata %>%
  mutate(ph_cart = 
           case_when(
             hv243c==0 | is.na(hv243c) ~ 0,
             hv243c==1 ~ 1)) %>%
  set_value_labels(ph_cart = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_cart = "Owns an animal drawn cart")

# //Motorcycle or scooter
HRdata <- HRdata %>%
  mutate(ph_moto = 
           case_when(
             hv211==0 | is.na(hv211) ~ 0,
             hv211==1 ~ 1)) %>%
  set_value_labels(ph_moto = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_moto = "Owns a motorcycle/scooter")

# //Car or truck
HRdata <- HRdata %>%
  mutate(ph_car = hv211) %>%
  set_value_labels(ph_car = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_car = "Owns a car or truck")

# //Boat with a motor
HRdata <- HRdata %>%
  mutate(ph_boat = 
           case_when(
             hv243d==0 | is.na(hv243d) ~ 0,
             hv243d==1 ~ 1)) %>%
  set_value_labels(ph_boat = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_boat = "Owns a boat with a motor")

# //Agricultural land
HRdata <- HRdata %>%
  mutate(ph_agriland = 
           case_when(
             hv244==0 | is.na(hv244) ~ 0,
             hv244==1 ~ 1)) %>%
  set_value_labels(ph_agriland = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_agriland = "Owns agricultural land")

# //Livestook
HRdata <- HRdata %>%
  mutate(ph_animals = 
           case_when(
             hv246==0 | is.na(hv246) ~ 0,
             hv246==1 ~ 1)) %>%
  set_value_labels(ph_animals = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ph_animals = "Owns livestock or farm animals")

