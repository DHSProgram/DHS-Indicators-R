# ******************************************************************************
# Program: 			  MS_MAR.do
# Purpose: 			  Code to create marital indicators
# Data inputs: 		IR and MR survey list
# Data outputs:		coded variables and scalars
# Author:				  Courtney Allen and Ali Roghani for code share project
# Date last modified: August 20 by Courtney Allen
# ******************************************************************************


# Variables created in this file: ----------------------------------------------
#  
# ms_mar_stat		"Current marital status"
# ms_mar_union	"Currently in union"
# ms_mar_never	"Never in union"
# ms_mar_regis	"Current marriage is registered with civil authorities" - NEW Indicator in DHS8
# ms_mar_doc		"Current marriage is registered and has documentation recognizing the marriage" - NEW Indicator in DHS8
# ms_mar_cert		"Current marriage is registered and has a marriage certificate" - NEW Indicator in DHS8
# ms_afm_15		  "First marriage by age 15"
# ms_afm_18		  "First marriage by age 18"
# ms_afm_20		  "First marriage by age 20"
# ms_afm_22		  "First marriage by age 22"
# ms_afm_25		  "First marriage by age 25"
#
# ONLY IN IR FILES:
# ms_cowives_num	"Number of co-wives"
# ms_cowives_any	"One or more co-wives"
#
# ONLY IN MR FILES:
# ms_wives_num	"Number of wives"
# 
# Datafiles created
#
# median_mar              datafile with median age at first marriage (MAFM) by 5-year age groups among women
# median_mar_subgroup     datafile with median age at first marriage (MAFM) by subgroup characteristics among women 20-49 and 25-49
# median_mar_men          datafile with median age at first marriage (MAFM) by 5-year age groups among men
# median_mar_subgroup_men datafile with median age at first marriage (MAFM) by subgroup characteristics among men 25-59 and 30-59 (age range may vary in survey)


# MEDIAN AGE FUNCTION ----------------------------------------------------------
calc_median_age <-function() {

  # create a age at first marriage (afm) dataframe with cumulative proportions by each age, use survey weights
  median_df <- data.frame(prop_cumulative = unclass(round(cumsum(prop.table(svytable(~temp_df$ms_age, design=dhssvy2))),4)))
  median_df$age <- as.numeric(row.names(median_df))
  
  # find age groups before and after the cumulative 50% 
  median_df <- median_df %>%
    mutate(age_before50 = case_when(prop_cumulative<0.5 & lead(prop_cumulative>0.5) ~ 1, TRUE ~ 0),
           age_after50 = case_when(prop_cumulative>=0.5 & lag(prop_cumulative<0.5) ~ 1, TRUE ~ 0))
  
  # use equation for interpolated median for completed time periods (see https://www.dhsprogram.com/data/Guide-to-DHS-Statistics.cfm)
  
  # age group before the cumulative 50%
  m1 <- median_df$age[median_df$age_before50==1]

  # cumulative proportion for the age group before the cumulative 50%
  p1 <- median_df$prop_cumulative[median_df$age_before50==1]
  
  # cumulative proportion for the age group after the cumulative 50%
  p2 <- median_df$prop_cumulative[median_df$age_after50==1]
  
 # calculate median age
  median_age <- round((m1 + ((0.5-p1)/(p2-p1)) + 1),1)
  
  # replace with NA if 50% of subgroup has not been married before start of subgroup
  median_age <- ifelse(median_age > beg_age, "NA", median_age)
  print(median_age)
}


		
# MARITAL STATUS (WOMEN)--------------------------------------------------------

# Create yes and no category labels
yesno = c("Yes" = 1, "No" = 0)

# Current marital status
IRdata <- IRdata %>%
  mutate(ms_mar_stat = v501) 
  
# Currently in union
IRdata <- IRdata %>%
  mutate(ms_mar_union = case_when(v502==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_mar_union = c("In union" = 1, "Not in union" = 0)) %>%
  set_variable_labels(ms_mar_union = "Currently in union")
           
# Never in union
IRdata <- IRdata %>%
  mutate(ms_mar_never = case_when(v501==0 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_mar_never = c("Not in union" = 1, "In union" = 0)) %>%
  set_variable_labels(ms_mar_never = "Never in union")

# Marriage is registered - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(ms_mar_regis = case_when(
    v502 == 1 & (v542 == 1 | v544 == 1) ~ 1,
    v502 == 1 &(v542 != 1 & v544 != 1) ~ 0,
    is.na(v502) & is.na(v542) | is.na(v544) ~ NA 
  )) %>%
  set_value_labels(ms_mar_regis = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(ms_mar_regis = "Current marriage is registered with civil authorities")

# Marriage is registered and documented - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(ms_mar_doc = case_when(
    v502 == 1 & (v542==1 | v544==1) & (v543a==1 | v543b==1 | v543c==1 | v543d==1 | v543x==1) ~ 1, 
    v502 == 1 ~ 0)) %>%
  set_value_labels(ms_mar_doc = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(ms_mar_doc = "Current marriage is registered and has documentation")

# Marriage is registered and has marriage certificate - NEW Indicator in DHS8
IRdata <- IRdata %>%
  mutate(ms_mar_cert = case_when(
    v502 == 1 & (v542 == 1 | v544 == 1) & (v543a == 1 | v543b == 1) ~ 1,
    v502 ==1 ~ 0)) %>%
  set_value_labels(ms_mar_cert = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(ms_mar_cert = "Current marriage is registered and has a marriage certificate")


# Count the number of currently married women whose marriage is registered and have a marriage certificate
num_registered_cert_marriages <- sum(IRdata$ms_mar_cert == 1)


# Specific case for Cambodia 2021-22
if(IRdata$v000[1]=="KH8") {
IRdata <- IRdata %>% mutate(
  ms_mar_regis = case_when(
    v000 == "KH8" & v501 == 1 & (s706 == 1 | v544 == 1) ~ 1,
    v501==1 ~ 0),
  ms_mar_doc = case_when(
    v000 == "KH8" ~ NA),
  ms_mar_cert = case_when(
    v000 == "KH8" & v501 == 1 & s706 == 1 ~ 1,
    v501 == 1 ~ 0)) %>%
  set_variable_labels(
    ms_mar_regis = "Current marriage is registered with civil authorities",
    ms_mar_doc = "NA - Current marriage is registered and has documentation recognizing the marriage",
    ms_mar_cert = "Current marriage is registered and has a marriage certificate")
}


# CO-WIVES (WOMEN)--------------------------------------------------------------

# Number of co-wives
IRdata <- IRdata %>%
  mutate(ms_cowives_num = case_when(
    v505==0 ~ 0, v505==1 ~ 1, 
    v505>=2 & v505<=97 ~ 2, 
    v505==98 ~ 98)) %>%
  set_value_labels(ms_cowives_num = c("None" = 0, "1" = 1, "2+" = 2, "Don't know" = 98)) %>%
  set_variable_labels(ms_cowives_num = "Number of co-wives")
    
# One or more co-wives
IRdata <- IRdata %>%
    mutate(ms_cowives_any = case_when(
      v505>=1 & v505<=97 ~ 1, 
      v505==0 | v505==98 ~ 0)) %>%
  set_value_labels(ms_cowives_any = c("1+" = 1, "None or DK" = 0)) %>%
  set_variable_labels(ms_cowives_any  = "One or more co-wives")
      

     
# MARRIED BY SPECIFIC AGES (WOMEN)----------------------------------------------

# First marriage by age 15
IRdata <- IRdata %>%
  mutate(ms_afm_15 = case_when(v511>=0 & v511<15 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_afm_15 = yesno) %>%
  set_variable_labels(ms_afm_15 = "First marriage by age 15")
           
# First marriage by age 18
IRdata <- IRdata %>%
  mutate(ms_afm_18 = case_when(v511>=0 & v511<18 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_afm_18 = yesno) %>%
  set_variable_labels(ms_afm_18 = "First marriage by age 18")

# First marriage by age 20
IRdata <- IRdata %>%
  mutate(ms_afm_20 = case_when(v511>=0 & v511<20 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_afm_20 = yesno) %>%
  set_variable_labels(ms_afm_20 = "First marriage by age 20")

# First marriage by age 22
IRdata <- IRdata %>%
  mutate(ms_afm_22 = case_when(v511>=0 & v511<22 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_afm_22 = yesno) %>%
  set_variable_labels(ms_afm_22 = "First marriage by age 22")

# First marriage by age 25
IRdata <- IRdata %>%
  mutate(ms_afm_25 = case_when(v511>=0 & v511<25 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_afm_25 = yesno) %>%
  set_variable_labels(ms_afm_25 = "First marriage by age 25")





# MARITAL STATUS (MEN) ---------------------------------------------------------
	
# Current marital status
MRdata <- MRdata %>%
  mutate(ms_mar_stat = mv501) 

# Currently in union
MRdata <- MRdata %>%
  mutate(ms_mar_union = case_when(mv502==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_mar_union = c("In union" = 1, "Not in union" = 0)) %>%
  set_variable_labels(ms_mar_union = "Currently in union")

# Never in union
MRdata <- MRdata %>%
  mutate(ms_mar_never = case_when(mv501==0 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_mar_union = c("In union" = 1, "Not in union" = 0)) %>%
  set_variable_labels(ms_mar_never = "Never in union")

# Number of wives 
MRdata <- MRdata %>%
  mutate(ms_wives_num = case_when(
    mv505==1 ~ 1,
    mv505>=2 & mv505<97 ~ 2,
    mv505==98 ~ 98)) %>%
  set_value_labels(ms_wives_num = c("1" = 1, "2+" = 2, "Don't know" = 98)) %>%
  set_variable_labels(ms_wives_num = "Number of wives")



# MARRIED BY SPECIFIC AGES (MEN) -----------------------------------------------------
	
	# First marriage by age 15
	MRdata <- MRdata %>%
	  mutate(ms_afm_15 = case_when(
	    mv511>=0 & mv511<15 ~ 1, TRUE ~ 0)) %>%
	  set_value_labels(ms_afm_15 = yesno) %>%
	  set_variable_labels(ms_afm_15 = "First marriage by age 15")
	
	# First marriage by age 18
	MRdata <- MRdata %>%
	  mutate(ms_afm_18 = case_when(mv511>=0 & mv511<18 ~ 1, TRUE ~ 0)) %>%
	  set_value_labels(ms_afm_18 = yesno) %>%
	  set_variable_labels(ms_afm_18 = "First marriage by age 18")
	
	# First marriage by age 20
	MRdata <- MRdata %>%
	  mutate(ms_afm_20 = case_when(mv511>=0 & mv511<20 ~ 1, TRUE ~ 0)) %>%
	  set_value_labels(ms_afm_20 = yesno) %>%
	  set_variable_labels(ms_afm_20 = "First marriage by age 20")
	
	# First marriage by age 22
	MRdata <- MRdata %>%
	  mutate(ms_afm_22 = case_when(mv511>=0 & mv511<22 ~ 1, TRUE ~ 0)) %>%
	  set_value_labels(ms_afm_22 = yesno) %>%
	  set_variable_labels(ms_afm_22 = "First marriage by age 22")
	
	# First marriage by age 25
	MRdata <- MRdata %>%
	  mutate(ms_afm_25 = case_when(mv511>=0 & mv511<25 ~ 1, TRUE ~ 0)) %>%
	  set_value_labels(ms_afm_25 = yesno) %>%
	  set_variable_labels(ms_afm_25 = "First marriage by age 25")

	
	
	
	
# MEDIAN AGES ---------------------------------------------------

	
	# NOTES
	#  Medians are calculated based on completed time periods using program defined
	#  at beginning of this R script: calc_median_age()
	#  See https://www.dhsprogram.com/data/Guide-to-DHS-Statistics.cfm for full details.
	#  
	#  Some age groups do not have medians because 50% of the women have not been married
	#  the beginning of the age group. Each median is calculated and saved as a vector. 
	#
	#  Medians can found for subgroups by adding to the variable list below in 
	#  the same manner

	

# MEDIAN AGE AT FIRST MARRIAGE BY AGE GROUP  (WOMEN)---------------------------------
	
	# if respondent has not been married (v511==NA) then replace with 99 to put these at the tail end of distribution for median
	IRdata <- IRdata %>% mutate(ms_age =case_when(is.na(v511) ~ 99, TRUE ~ v511))
	
	# list of the beginning age groups
	beg_age_list <- c(15, 20, 25, 30, 35, 40, 45)
	
	# create empty dataframe to fill in with results
	median_mar <- data.frame("age group"=NA, "median afm"=NA)
	
	
# create loop for each age group
for (a in beg_age_list) {
  beg_age <- a
  end_age <- a+4
  
	    cat("finding median for ages", beg_age, "to", end_age)
	  	    
	    #subset the age group using a beginning and ending age and subgroup
	    temp_df <- IRdata %>% filter(v012>= beg_age & v012<= end_age) %>%
	      select(ms_age, v021, v022, v005)

	    # weight data
	    dhssvy2 <- svydesign(id = temp_df$v021, strata=temp_df$v022, weights = temp_df$v005/1000000, data=temp_df)
	    
	    median_age <- calc_median_age()

	   #save results
     data_row <- data.frame(paste0(beg_age,"-",end_age), median_age)
	   median_mar <- rbind(median_mar, setnames(data_row, names(median_mar)))
	   
  
}
	
	
# MEDIAN AGE AT FIRST MARRIAGE BY AGE GROUP AND SUBGROUP (WOMEN)----------------------
	

  # mutate a dummy variable to loop through all women instead of subgroup
  IRdata <- IRdata %>% mutate(all = 1) %>% set_variable_labels(all = "total") %>% set_value_labels(all = c("total"=1))
  
  # list of subgroup characteristics (residence, education, wealth, total)
  subgroup <- c("v025", "v106", "v190", "all")
  
  # list of the beginning age groups (among subgroups only two age groups used: 20-49, 25-49)
  beg_age_list <- c( 20, 25)
  end_age <- max(IRdata$v012)   
  
	# create empty dataframe to fill in with results
	median_mar_subgroup <- data.frame("age group"=NA, "subgroup"=NA, "level"=NA, "median afm"=NA)
	
	
	
	# create loop for each age group and each level of each subgroup characteristic
	for (a in beg_age_list) {
	  beg_age <- a
	  
	  for(y in subgroup) {  
	    
	    # generate list of unique levels for each subgroup
	    z <- as.vector(remove_val_labels(sort(unique(IRdata[[y]]))))
	    
	    # loop through each level of each subgroup
	    for(x in z) {
	      cat("finding median for ages", beg_age, "to", end_age, "subgroup:", y, "level=", x)
	      
	      #subset the age group using a beginning and ending age and subgroup
	      temp_df <- IRdata %>% filter(v012>= beg_age & v012<= end_age) %>%
	        select(ms_age, v025, v106, v190, all, v021, v022, v005)
	      temp_df <- temp_df[ temp_df[y]==x, ]
	      
	      # weight data
	      dhssvy2 <- svydesign(id = temp_df$v021, strata=temp_df$v022, weights = temp_df$v005/1000000, data=temp_df)
	      
	      median_age <- calc_median_age()
	      
	      
	      #save results
	      data_row <- data.frame(paste0(beg_age,"-",end_age), var_label(IRdata[,y]), val_label(IRdata[,y],x), median_age)
	      median_mar_subgroup <- rbind(median_mar_subgroup, setnames(data_row, names(median_mar_subgroup)))
	      
	    }
	  }
	  
	}
	
	

	

# MEDIAN AGE AT FIRST MARRIAGE BY AGE GROUP  (MEN)---------------------------------
	
	# if respondent has not been married (v511==NA) then replace with 99 to put these at the tail end of distribution for median
	MRdata <- MRdata %>% mutate(ms_age =case_when(is.na(mv511) ~ 99, TRUE ~ mv511))
	
	# list of the beginning age groups
	beg_age_list <- c(15, 20, 25, 30, 35, 40, 45)
	
	
	# create empty dataframe to fill in with results
	median_mar_men <- data.frame("age group"=NA, "median afm"=NA)
	
	
	# create loop for each age group and each level of each subgroup characteristic
	for (a in beg_age_list) {
	  beg_age <- a
	  end_age <- a+4
	  
	  cat("finding median for ages", beg_age, "to", end_age)
	  
	  #subset the age group using a beginning and ending age and subgroup
	  temp_df <- MRdata %>% filter(mv012>= beg_age & mv012<= end_age) %>%
	    select(ms_age, mv021, mv022, mv005)
	  
	  # weight data
	  dhssvy2 <- svydesign(id = temp_df$mv021, strata=temp_df$mv022, weights = temp_df$mv005/1000000, data=temp_df)
	  
	  median_age <- calc_median_age()
	  
	  
	  #save results
	  data_row <- data.frame(paste0(beg_age,"-",end_age), median_age)
	  median_mar_men <- rbind(median_mar_men, setnames(data_row, names(median_mar_men)))
	  
	  
	}
	
	
	# MEDIAN AGE AT FIRST MARRIAGE BY AGE GROUP AND SUBGROUP (MEN)----------------------
	
	
	# mutate a dummy variable to loop through all women instead of subgroup
	MRdata <- MRdata %>% mutate(all = 1) %>% set_variable_labels(all = "total") %>% set_value_labels(all = c("total" = 1))
	
	# list of subgroup characteristics (residence, education, wealth, total)
	subgroup <- c("mv025", "mv106", "mv190", "all")
	
	# list of the beginning age groups  (among subgroups only two age groups used: 20-49, 25-49)
	beg_age_list <- c( 25, 30)  # beginning ages may be higher for men's age groups, change accordingly
	end_age <- max(MRdata$mv012)  
	
	# create empty dataframe to fill in with results
	median_mar_subgroup_men <- data.frame("age group"=NA, "subgroup"=NA, "level"=NA, "median afm"=NA)
	
	
	
	# create loop for each age group and each level of each subgroup characteristic
	for (a in beg_age_list) {
	  beg_age <- a
	  
	  for(y in subgroup) {  
	    
	    # generate list of unique levels for each subgroup
	    z <- as.vector(remove_val_labels(sort(unique(MRdata[[y]]))))
	    
	    # loop through each level of each subgroup
	    for(x in z) {
	      cat("finding median for ages", beg_age, "to", end_age, "subgroup:", y, "level=", x)
	      
	      #subset the age group using a beginning and ending age and subgroup
	      temp_df <- MRdata %>% filter(mv012>= beg_age & mv012<= end_age) %>%
	        select(ms_age, mv025, mv106, mv190, all, mv021, mv022, mv005)
	      temp_df <- temp_df[ temp_df[y]==x, ]
	      
	      # weight data
	      dhssvy2 <- svydesign(id = temp_df$mv021, strata=temp_df$mv022, weights = temp_df$mv005/1000000, data=temp_df)
	      median_age <- calc_median_age()
	      
	      
	      #save results
	      data_row <- data.frame(paste0(beg_age,"-",end_age), var_label(MRdata[,y]), val_label(MRdata[,y],x), median_age)
	      median_mar_subgroup_men <- rbind(median_mar_subgroup_men, setnames(data_row, names(median_mar_subgroup_men)))
	      
	    }
	  }
	  
	}
	
	


	
	
	
	