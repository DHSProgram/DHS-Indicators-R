# /*****************************************************************************************************
# Program: 			RH_PNC.R
# Purpose: 			Code PNC indicators for women and newborns
# Data inputs: 	IR dataset
# Data outputs:	coded variables
# Author:				Lindsay Mallick and Shireen Assaf
# Date last modified: September 15 2021 by Mahmoud Elkasabi 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------//
# Variables created in this file:
# rh_pnc_wm_timing	"Timing after delivery for mother's PNC check"
# rh_pnc_wm_2days 	"PNC check within two days for mother"
# rh_pnc_wm_pv 		  "Provider for mother's PNC check"
# 
# rh_pnc_nb_timing	"Timing after delivery for newborn's PNC check"
# rh_pnc_nb_2days 	"PNC check within two days for newborn"
# rh_pnc_nb_pv 		  "Provider for newborn's PNC check"	
# /----------------------------------------------------------------------------*/

# age of child. If b19_01 is not available in the data use v008 - b3_01
if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
  IRdata [[paste("b19_01")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $b19_01)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  IRdata <- IRdata %>%
    mutate(age = b19_01)
} else {
  IRdata <- IRdata %>%
    mutate(age = v008 - b3_01)
}


# ** For surveys 2005 or after, postnatal care was asked for both institutional and non-institutional births. 
# ** surveys before 2005 only ask PNC for non-institutional births but assumed women received PNC if they delivered at health facilities	 
# ** This is checked using variable m51_1 which was used in older surveys
# ** If the code does not run, perhaps it is because you need to use m51a_1. Uncomment the next line in that case.
# 	*cap gen m51_1=m51a_1

# ** To check if survey has m51_1, which was in the surveys before 2005. 
if ("TRUE" %in% (!("m51_1" %in% names(IRdata))))
  IRdata [[paste("m51_1")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $m51_1)))
{ m51_included <- 0} else { m51_included <- 1}

### *** Mother's PNC *** ###		
	
if (m51_included==1) {
# //PNC timing for mother	
   IRdata <- IRdata %>%
     mutate(rh_pnc_wm_timing =
              case_when(
                (m51_1 >=242 & m51_1<=297) | (m51_1>=306 & m51_1<=397) | (m50_1==0 | m50_1==9) | (m52_1>29 & m52_1<97) | is.na(m52_1)  ~ 0 ,
                m51_1  %in% c(100, 101, 102, 103)  ~ 1 ,
                (m51_1 >=104 & m51_1<=123) | m51_1 == 200  ~ 2 ,
                (m51_1 >=124 & m51_1<=171) | m51_1 %in% c(201,202)~ 3,
                (m51_1 >=172 & m51_1<=197) | m51_1 %in% c(203,204,205,206) ~ 4, 
                (m51_1 >=207 & m51_1<=241) | (m51_1 >=301 & m51_1<=305)  ~ 5, 
                m51_1  %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9)) 
   
     IRdata[["rh_pnc_wm_timing"]] <- ifelse(IRdata[["bidx_01"]]!=1 | IRdata[["age"]]>=24, NA, IRdata[["rh_pnc_wm_timing"]])
     
     IRdata <- IRdata %>%
     set_value_labels(rh_pnc_wm_timing = c("No PNC" = 0, "<4hr" = 1, "4-23hrs"=2, "1-2days"=3, "3-6days"=4, "7-41days"=5, "don't know/missing"=9)) %>%
     set_variable_labels(rh_pnc_wm_timing = "Timing after delivery for mother's PNC check")

   
# //PNC within 2days for mother	  
   IRdata <- IRdata %>%
     mutate(rh_pnc_wm_2days =
              case_when(
                rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
                rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
                bidx_01!=1 | age>=24 ~ 99 )) %>%
     replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) %>%
     set_value_labels(rh_pnc_wm_2days = c("No Visit w/in 2 days" = 0, "visit w/in 2 days" = 1 )) %>%
     set_variable_labels(rh_pnc_wm_2days = "PNC check within two days for mother")

# //PNC provider for mother	
# This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report. 
   IRdata <- IRdata %>%
     mutate(rh_pnc_wm_pv =
              case_when(
                ((age<24 & rh_pnc_wm_2days==1) & m52_1==0) | (age<24 & rh_pnc_wm_2days==0)  ~ 0,
                (age<24 & rh_pnc_wm_2days==1) & m52_1 ==11 ~ 1,
                (age<24 & rh_pnc_wm_2days==1) & m52_1 %in% c(12,13) ~ 2,
                (age<24 & rh_pnc_wm_2days==1) & m52_1 %in% c(14,15) ~ 3,
                (age<24 & rh_pnc_wm_2days==1) & m52_1>=16 & m52_1<=90 ~ 4,
                (age<24 & rh_pnc_wm_2days==1) & m52_1==96 ~ 5 ,
                (age<24 & rh_pnc_wm_2days==1) & m52_1>96 ~ 9)) %>%
     set_value_labels(rh_pnc_wm_pv = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  )) %>%
     set_variable_labels(rh_pnc_wm_pv = "Provider for mother's PNC check")

}

if (m51_included==0) {
# //PNC timing for mother		
# 	*did the mother have any check
  IRdata <- IRdata %>%
    mutate(momcheck =
             case_when(
               (m62_1!=1 & m66_1!=1) & age<24 ~ 0,
               (m62_1==1 | m66_1==1) & age<24 ~ 1,
               age<24 ~ 0)) 

IRdata <- IRdata %>%
    mutate(pnc_wm_time =
             case_when(
               (m64_1 >= 11 & m64_1 <= 29) & age<24 ~ as.numeric(m63_1),
               age<24 & momcheck == 1 ~ 999)) %>%
    mutate(pnc_wm_time1000 = 
             case_when(
               pnc_wm_time<1000 ~ 1)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time1000==1 & (m64_1 > 30 & m64_1 < 100) & age<24 ~ 0,
               TRUE ~ pnc_wm_time )) %>%
    mutate(pnc_wm_time999 = 
             case_when(
               pnc_wm_time==999 ~ 1)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time999==1 & (m68_1 >= 11 & m68_1 <= 29) & age<24 ~ m67_1,
               TRUE ~ pnc_wm_time ))  %>%
    mutate(pnc_wm_time0 =
             case_when(
               m67_1 < 1000 & m68_1 > 30 & m68_1 < 100 & age<24 ~ 1,
               TRUE ~ 0)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time0==1 ~ 0,
               pnc_wm_time0==0 ~ as.numeric(pnc_wm_time))) %>%
    mutate(pnc_wm_time00 =
             case_when(
               momcheck==0 & age<24 ~ 1)) %>%
    mutate(pnc_wm_time =
             case_when(
               pnc_wm_time00==1 ~ 0,
               TRUE ~ as.numeric(pnc_wm_time)))
    
IRdata <- IRdata %>%
    mutate(rh_pnc_wm_timing =
             case_when(
               (pnc_wm_time >=242 & pnc_wm_time<=299) | (pnc_wm_time>=306 & pnc_wm_time<900) | pnc_wm_time==0  ~ 0 ,
               pnc_wm_time  %in% c(100, 101, 102, 103)  ~ 1 ,
               (pnc_wm_time >=104 & pnc_wm_time<=123) | pnc_wm_time==200 ~ 2 ,
               (pnc_wm_time >=124 & pnc_wm_time<=171) | pnc_wm_time %in% c(201,202)~ 3,
               (pnc_wm_time >=172 & pnc_wm_time<=197) | pnc_wm_time %in% c(203,204,205,206) ~ 4, 
               (pnc_wm_time >=207 & pnc_wm_time<=241) | (pnc_wm_time >=301 & pnc_wm_time<=305)  ~ 5, 
               pnc_wm_time  %in% c(198, 199, 298, 299, 298, 399, 998, 999)  ~ 9 ,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(rh_pnc_wm_timing = c(99))) %>%
    set_value_labels(rh_pnc_wm_timing = c("No check or past 41 days" = 0, "<4hr" = 1, "4-23hrs"=2, "1-2days"=3, "3-6days"=4, "7-41days"=5, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_pnc_wm_timing = "Timing after delivery for mother's PNC check")  


# //PNC within 2days for mother	  
  IRdata <- IRdata %>%
    mutate(rh_pnc_wm_2days =
             case_when(
               rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
               rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) %>%
    set_value_labels(rh_pnc_wm_2days = c("Not in 2 days" = 0, "Within 2 days" = 1 )) %>%
    set_variable_labels(rh_pnc_wm_2days = "PNC check within two days for mother")

# //PNC provider for mother	
# This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report. 
#Providers of PNC for facility deliveries   
  IRdata <- IRdata %>%
    mutate(pnc_wm_pv_hf =
             case_when(
               age<24 & rh_pnc_wm_2days==1 & m64_1==0 ~ 0,
               age<24 & rh_pnc_wm_2days==0 ~ 0,
               age<24 & rh_pnc_wm_2days==1 & m64_1 ==11 ~ 1,
               age<24 & rh_pnc_wm_2days==1 & m64_1 %in% c(12,13) ~ 2,
               age<24 & rh_pnc_wm_2days==1 & m64_1 %in% c(14,15) ~ 3,
               age<24 & rh_pnc_wm_2days==1 & m64_1>=16 & m64_1<=90 ~ 4,
               age<24 & rh_pnc_wm_2days==1 & m64_1==96 ~ 5,
               age<24 & rh_pnc_wm_2days==1 & !(m64_1 %in% seq(11:96)) ~ 9 ,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(pnc_wm_pv_hf = c(99))) %>%
    set_value_labels(pnc_wm_pv_hf = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  ))

#Providers of PNC for home deliveries or checks after discharge    
    IRdata <- IRdata %>%
    mutate(pnc_wm_pv_home =
             case_when(
               age<24 & rh_pnc_wm_2days==1 & m68_1==0 ~ 0,
               age<24 & rh_pnc_wm_2days==0 ~ 0,
               age<24 & rh_pnc_wm_2days==1 & m68_1 ==11 ~ 1,
               age<24 & rh_pnc_wm_2days==1 & m68_1 %in% c(12,13) ~ 2,
               age<24 & rh_pnc_wm_2days==1 & m68_1 %in% c(14,15) ~ 3,
               age<24 & rh_pnc_wm_2days==1 & m68_1>=16 & m68_1<=90 ~ 4,
               age<24 & rh_pnc_wm_2days==1 & m68_1==96 ~ 5,
               age<24 & rh_pnc_wm_2days==1 & !(m68_1 %in% seq(11:96)) ~ 9 ,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(pnc_wm_pv_home = c(99)))  %>%
    set_value_labels(pnc_wm_pv_home = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  ))

#Combine two PNC provider variables 	
IRdata <- IRdata %>%
    mutate(rh_pnc_wm_pv =
             ifelse(pnc_wm_pv_hf==9 & rh_pnc_wm_2days==1 & age<24,pnc_wm_pv_home,pnc_wm_pv_hf))  %>%
    set_variable_labels(rh_pnc_wm_pv = "Provider for mother's PNC check")

}  

############################################
### *** Newborn's PNC *** ####
# 
# * some surveys (usually older surveys) do not have PNC indicators for newborns. For this you would need variables m70_1, m71_1, ..., m76_1
# ** To check if survey has m51_1, which was in the surveys before 2005. 
if ("TRUE" %in% (!("m70_1" %in% names(IRdata))))
  IRdata [[paste("m70_1")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $m70_1)))
{ m70_included <- 0} else { m70_included <- 1}

# Survey has newborn PNC indicators
if (m70_included==1) {
	
 	if (m51_included==1) {
			
#//PNC timing for newborn
 	  IRdata <- IRdata %>%
 	    mutate(rh_pnc_nb_timing =
 	             case_when(
 	               (m71_1 >=207 & m71_1<=297) | (m71_1>=301 & m71_1<397) | (m70_1==0 | m70_1==9) | (m72_1>29 & m72_1<97) | is.na(m72_1) ~ 0,
 	               m71_1 ==100 ~ 1 ,
 	               m71_1 %in% c(101,102,103) ~ 2 ,
 	               (m71_1 >=104 & m71_1<=123) | m71_1 ==200 ~ 3,
 	               (m71_1 >=124 & m71_1<=171) | m71_1 %in% c(201,202) ~ 4, 
 	               (m71_1 >=172 & m71_1<=197) | (m71_1 >=203 & m71_1<=206)  ~ 5, 
 	               m71_1  %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9)) 
 	  
 	  IRdata[["rh_pnc_nb_timing"]] <- ifelse(IRdata[["bidx_01"]]!=1 | IRdata[["age"]]>=24, NA, IRdata[["rh_pnc_nb_timing"]]) 	  
 	  
 	  IRdata <- IRdata %>%
 	  set_value_labels(rh_pnc_nb_timing = c("No check or past 7 day" = 0, "<1hr" = 1, "1-3hrs"=2, "4-23hrs"=3, "1-2days"=4, "3-6days"=5, "Don't know/missing"=9  )) %>%
 	  set_variable_labels(rh_pnc_nb_timing = "Timing after delivery for mother's PNC check")  

 	  
#//PNC within 2days for newborn	
 	  IRdata <- IRdata %>%
 	    mutate(rh_pnc_nb_2days =
 	             case_when(
 	               rh_pnc_nb_timing %in% c(1,2,3,4) ~ 1,
 	               rh_pnc_nb_timing %in% c(0,5,9) ~ 0,
 	               bidx_01!=1 | age>=24 ~ 99 )) %>%
 	    replace_with_na(replace = list(rh_pnc_nb_2days = c(99))) %>%
 	    set_value_labels(rh_pnc_nb_2days = c("No Visit within 2 days" = 0, "visit within 2 days" = 1 )) %>%
 	    set_variable_labels(rh_pnc_nb_2days = "PNC check within two days for newborn")

# //PNC provider for newborn
# This is country specific, please check table in final report
 	  IRdata <- IRdata %>%
 	    mutate(rh_pnc_nb_pv =
 	           case_when(
 	             ((age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) & (m72_1==0 | rh_pnc_nb_2days==0)) | (age<24 & rh_pnc_nb_2days==0) ~ 0,
 	             (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1 ==11 ~ 1,
 	             (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1 %in% c(12,13) ~ 2,
 	             (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1 %in% c(14,15) ~ 3,
 	             (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1>=16 & m72_1<=90 ~ 4,
 	             (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1==96 ~ 5,
 	             (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1>96 ~ 9)) %>%
 	    set_value_labels(rh_pnc_nb_pv = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  )) %>%
 	    set_variable_labels(rh_pnc_nb_pv = "Provider for newborn's PNC check")
}

# 
if (m51_included==0) {
# 
# //PNC timing for newborn	
# *Newborn check
  IRdata <- IRdata %>%
    mutate(nbcheck =
             case_when(
               (m70_1==1 | m74_1==1) ~ 1,
               (m70_1!=1 | m74_1!=1) ~ 0)) 

  IRdata <- IRdata %>%
    mutate(pnc_nb_timing_all =
             case_when(
               nbcheck!=1 & age<24 ~ 0,
               (m76_1 >= 11 & m76_1 <= 29) & age<24 ~ as.numeric(m75_1),
               age<24 & nbcheck == 1 ~ 999)) %>%
    mutate(pnc_nb_timing_all1000 = 
             case_when(
               pnc_nb_timing_all<1000 ~ 1)) %>%
    mutate(pnc_nb_timing_all =
             case_when(
               pnc_nb_timing_all1000==1 & (m76_1 > 30 & m76_1 < 100) & age<24 ~ 0,
               TRUE ~ pnc_nb_timing_all )) %>%
    mutate(pnc_nb_timing_all999 = 
             case_when(
               pnc_nb_timing_all==999 ~ 1)) %>%
    mutate(pnc_nb_timing_all =
             case_when(
               pnc_nb_timing_all999==1 & (m72_1 >= 11 & m72_1 <= 29) & age<24 ~ m71_1,
               TRUE ~ pnc_nb_timing_all ))  %>%
    mutate(pnc_nb_timing_all0 =
             case_when(
               m71_1 < 1000 & m72_1 > 30 & m72_1 < 100 & age<24 ~ 1,
               TRUE ~ 0)) %>%
    mutate(pnc_nb_timing_all =
             case_when(
               pnc_nb_timing_all0==1 ~ 0,
               pnc_nb_timing_all0==0 ~ as.numeric(pnc_nb_timing_all)))

IRdata <- IRdata %>%
  mutate(rh_pnc_nb_timing =
           case_when(
             (pnc_nb_timing_all >=207 & pnc_nb_timing_all<=297) | (pnc_nb_timing_all>=301 & pnc_nb_timing_all<397) | pnc_nb_timing_all==0  ~ 0 ,
             pnc_nb_timing_all  %in% c(100)  ~ 1 ,
             pnc_nb_timing_all  %in% c(101,102,103)  ~ 2 ,
             (pnc_nb_timing_all >=104 & pnc_nb_timing_all<=123) | pnc_nb_timing_all==200 ~ 3 ,
             (pnc_nb_timing_all >=124 & pnc_nb_timing_all<=171) | pnc_nb_timing_all %in% c(201,202)~ 4,
             (pnc_nb_timing_all >=172 & pnc_nb_timing_all<=197) | pnc_nb_timing_all %in% c(203,204,205,206) ~ 5, 
             pnc_nb_timing_all  %in% c(198, 199, 298, 299, 298, 399, 998, 999)  ~ 9 ,
             age>=24 ~ 99 )) %>%
  replace_with_na(replace = list(rh_pnc_nb_timing = c(99))) %>%
  set_value_labels(rh_pnc_nb_timing = c("No check or past 7 days" = 0, "less than 1 hour" = 1, "1-3 hours"=2, "4 to 23 hours"=3, "1-2 days"=4, "3-6 days new"=5, "Don't know/missing"=9  )) %>%
  set_variable_labels(rh_pnc_nb_timing = "Timing after delivery for mother's PNC check")  

# //PNC within 2days for newborn	  
IRdata <- IRdata %>%
  mutate(rh_pnc_nb_2days =
           case_when(
             rh_pnc_nb_timing %in% c(1,2,3,4) ~ 1,
             rh_pnc_nb_timing %in% c(0,5,9) ~ 0,
             age>=24 ~ 99 )) %>%
  replace_with_na(replace = list(rh_pnc_nb_2days = c(99))) %>%
  set_value_labels(rh_pnc_nb_2days = c("No Visit within 2 days" = 0, "visit within 2 days" = 1 )) %>%
  set_variable_labels(rh_pnc_nb_2days = "PNC check within two days for newborn")

# //PNC provider for newborn	
# This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report. 
#Providers of PNC for home deliveries or checks after discharge   
IRdata <- IRdata %>%
  mutate(pnc_nb_pv_home =
           case_when(
             age<24 & rh_pnc_nb_2days==1 & m72_1==0 ~ 0,
             age<24 & rh_pnc_nb_2days==0 ~ 0,
             age<24 & rh_pnc_nb_2days==1 & m72_1 ==11 ~ 1,
             age<24 & rh_pnc_nb_2days==1 & m72_1 %in% c(12,13) ~ 2,
             age<24 & rh_pnc_nb_2days==1 & m72_1 %in% c(14,15) ~ 3,
             age<24 & rh_pnc_nb_2days==1 & m72_1>=16 & m72_1<=90 ~ 4,
             age<24 & rh_pnc_nb_2days==1 & m72_1==96 ~ 5,
             age<24 & rh_pnc_nb_2days==1 & !(m72_1 %in% seq(11:96)) ~ 9 ,
             age>=24 ~ 99 )) %>%
  replace_with_na(replace = list(pnc_nb_pv_home = c(99))) %>%
  set_value_labels(rh_pnc_nb_timing = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know or missing"=9))

#Providers of PNC for facility deliveries   
IRdata <- IRdata %>%
  mutate(pnc_nb_pv_hf =
           case_when(
             age<24 & rh_pnc_nb_2days==1 & m76_1==0 ~ 0,
             age<24 & rh_pnc_nb_2days==0 ~ 0,
             age<24 & rh_pnc_nb_2days==1 & m76_1 ==11 ~ 1,
             age<24 & rh_pnc_nb_2days==1 & m76_1 %in% c(12,13) ~ 2,
             age<24 & rh_pnc_nb_2days==1 & m76_1 %in% c(14,15) ~ 3,
             age<24 & rh_pnc_nb_2days==1 & m76_1>=16 & m76_1<=90 ~ 4,
             age<24 & rh_pnc_nb_2days==1 & m76_1==96 ~ 5,
             age<24 & rh_pnc_nb_2days==1 & !(m76_1 %in% seq(11:96)) ~ 9 ,
             age>=24 ~ 99 )) %>%
  replace_with_na(replace = list(pnc_nb_pv_hf = c(99))) %>%
  set_value_labels(pnc_nb_pv_hf = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know or missing"=9))

#Combine two PNC provider variables 	
IRdata <- IRdata %>%
  mutate(rh_pnc_nb_pv =
           ifelse(pnc_nb_pv_hf==9 & rh_pnc_nb_2days==1 & age<24,pnc_nb_pv_home,pnc_nb_pv_hf))  %>%
  set_variable_labels(rh_pnc_nb_pv = "Provider for newborns's PNC check")

	}
 
}

# Survey does not have newborn PNC indicators
 if (m70_included==0) {
# replace indicators as NA
   IRdata <- IRdata %>%
     mutate( rh_pnc_nb_timing =NA) %>%
     mutate( rh_pnc_nb_2days =NA) %>%
     mutate( rh_pnc_nb_pv =NA) 
 }

