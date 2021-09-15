# *****************************************************************************************************
# Program: 			RH_ANC.R
# Purpose: 			Code ANC indicators
# Data inputs: 	IR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Sept 13, 2021 by Shireen Assaf 
# *****************************************************************************************************
# 
# ----------------------------------------------------------------------------
# Variables created in this file:
# rh_anc_pv       "Person providing assistance during ANC"
# rh_anc_pvskill  "Skilled assistance during ANC"
# rh_anc_numvs		"Number of ANC visits"
# rh_anc_4vs			"Attended 4+ ANC visits"
# rh_anc_moprg		"Attended ANC <4 months of pregnancy"
# rh_anc_median		"Median months pregnant at first visit" (scalar not a variable)
# rh_anc_iron			"Took iron tablet/syrup during the pregnancy of last birth"
# rh_anc_parast		"Took intestinal parasite drugs during pregnancy of last birth"
# rh_anc_prgcomp  "Informed of pregnancy complications during ANC visit"
# rh_anc_bldpres  "Blood pressure was taken during ANC visit"
# rh_anc_urine    "Urine sample was taken during ANC visit"
# rh_anc_bldsamp  "Blood sample was taken during ANC visit"
# rh_anc_toxinj   "Received 2+ tetanus injections during last pregnancy"
# rh_anc_neotet   "Protected against neonatal tetanus"
# ----------------------------------------------------------------------------

IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# period and age of child
# choose reference period, last 2 years (24 months) or last 5 years (60 months)
# Using a period of the last 2 years will not match final report but would provide more recent information.
# period = 24
IRdata <- IRdata %>%
  mutate(period = 60)

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

### *** ANC visit indicators *** ###

# //ANC by type of provider
# ** Note: Please check the final report for this indicator to determine the categories and adjust the code and label accordingly. 
IRdata <- IRdata %>%
  mutate(rh_anc_pv =
           case_when(
              m2a_1 == 1   ~ 1 ,
              m2b_1 == 1 ~ 2,
              m2c_1 == 1 | m2d_1 == 1 | m2e_1 == 1 ~ 3 ,
              m2f_1 == 1 | m2g_1 == 1 | m2h_1 == 1 | m2i_1 == 1 | m2j_1 == 1 | m2k_1 == 1 | m2l_1 == 1 | m2m_1 == 1 ~ 4 ,
              m2a_1 <2 ~ 5,
              m2a_1 == 9 ~ 9 ,
              age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_anc_pv = c(99))) %>%
  set_value_labels(rh_anc_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Other health worker"=3, "TBA/other/relative"=4, "No ANC"=5, "don't know/missing"=9  )) %>%
  set_variable_labels(rh_anc_pv = "Person providing assistance during ANC")

# //ANC by skilled provider
# ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
IRdata <- IRdata %>%
  mutate(rh_anc_pvskill =
           case_when(
             rh_anc_pv>=0 & rh_anc_pv<=2   ~ 1 ,  
             rh_anc_pv>2 & rh_anc_pv<=6 ~ 0)) %>%
  set_value_labels(rh_anc_pvskill = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_anc_pvskill = "Skilled assistance during ANC")

# //Number of ANC visits in 4 categories that match the table in the final report
IRdata <- IRdata %>%
  mutate(rh_anc_numvs =
           case_when(
             m14_1 == 0 ~ 0 ,
             m14_1 == 1 ~ 1 ,
             m14_1  %in% c(2,3)   ~ 2 ,
             m14_1>=4 & m14_1<=90  ~ 3 ,
             m14_1>90  ~ 9 ,
             age>=period ~ 99 )) %>%
  replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
  set_value_labels(rh_anc_numvs = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) %>%
  set_variable_labels(rh_anc_numvs = "Number of ANC visits")
 
# //4+ ANC visits  
IRdata <- IRdata %>%
  mutate(rh_anc_4vs =
           case_when(
             rh_anc_numvs==3 ~ 1,
             rh_anc_numvs %in% c(0,1,2,9)   ~ 0 )) %>%
  set_value_labels(rh_anc_4vs = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_anc_4vs = "Attended 4+ ANC visits")
 
# //Number of months pregnant at time of first ANC visit
IRdata <- IRdata %>%
  mutate(rh_anc_moprg =
           case_when(
             m14_1 == 0 ~ 0 ,
             m13_1  %in% c(0,1,2,3)   ~ 1 ,
             m13_1  %in% c(4,5)  ~ 2 ,
             m13_1  %in% c(6,7)~ 3,
             m13_1>=8 & m13_1<=90 ~ 4, 
             m13_1>90 & m13_1<100 ~ 9 ,
             age>=period ~ 99 )) %>%
  replace_with_na(replace = list(rh_anc_moprg = c(99))) %>%
  set_value_labels(rh_anc_moprg = c("No ANC" = 0, "<4" = 1, "4-5"=2, "6-7"=3, "8+"=4, "don't know/missing"=9  )) %>%
  set_variable_labels(rh_anc_moprg = "Number of months pregnant at time of first ANC visit")

# //ANC before 4 months
IRdata <- IRdata %>%
  mutate(rh_anc_4mo =
           case_when(
             rh_anc_moprg %in% c(0,2,3,4,9)   ~ 0 ,
             rh_anc_moprg==1 ~ 1)) %>%
  set_value_labels(rh_anc_4mo = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_anc_4mo = "Attended ANC <4 months of pregnancy")

# //Median number of months pregnant at time of 1st ANC
# * Any ANC visits (for denominator)
IRdata <- IRdata %>%
  mutate(ancany =
           case_when(
             m14_1 %in% c(0,99)   ~ 0 ,
             m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1))
IRdata <- IRdata %>%
  mutate(anctiming=
           case_when(
             m13_1 != 98 ~ m13_1,
             m13_1 == 98 ~ 99)) %>%    
  replace_with_na(replace = list(anctiming = c(99))) 


#to obtain the 50% percentile
sp50 <- matrixStats::weightedMedian(IRdata$anctiming, IRdata$wt, idxs = NULL, na.rm = TRUE)

IRdata <- IRdata %>%
mutate(dummy =
         case_when(
           anctiming<sp50 & ancany==1 ~ 1,
           ancany==1 ~ 0  ))
sL <- matrixStats::weightedMean(IRdata$dummy, IRdata$wt, idxs = NULL, na.rm = TRUE)

IRdata <- IRdata %>%
  mutate(dummy =
           case_when(
             anctiming<=sp50 & ancany==1 ~ 1,
             ancany==1 ~ 0  ))
sU <- matrixStats::weightedMean(IRdata$dummy, IRdata$wt, idxs = NULL, na.rm = TRUE)

IRdata <- IRdata %>%
  mutate(rh_anc_median=round(sp50+(0.5-sL)/(sU-sL),1)) %>%
  set_variable_labels(rh_anc_median = "Total- Median months pregnant at first visit")

#remove temporary values
rm(sL, sp50, sU)

### *** ANC components *** ###

# //Took iron tablets or syrup
IRdata <- IRdata %>%
  mutate(rh_anc_iron =
           case_when(
             m45_1 == 1 ~ 1 ,
             v208 ==0 | age>=period ~ 99,
             TRUE ~ 0)) %>%
  replace_with_na(replace = list(rh_anc_iron = c(99))) %>%
  set_value_labels(rh_anc_iron = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_iron = "Took iron tablet/syrup during pregnancy of last birth")

# //Took intestinal parasite drugs 
IRdata <- IRdata %>%
  mutate(rh_anc_parast =
           case_when(
             m60_1 == 1 ~ 1 ,
             v208 ==0 | age>=period ~ 99,
             TRUE ~ 0)) %>%
  replace_with_na(replace = list(rh_anc_parast = c(99))) %>%
  set_value_labels(rh_anc_parast = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_parast = "Took intestinal parasite drugs during pregnancy of last birth")
 
# 	* for surveys that do not have this variable
# 	cap gen rh_anc_parast=.
# 	
# * Among women who had ANC for their most recent birth	
# 
# //Informed of pregnancy complications
# this variable is not always available in the data. Please check.
IRdata <- IRdata %>%
  mutate(rh_anc_prgcomp =
           case_when(
             m43_1 == 1 & ancany==1 ~ 1  ,
             ancany==1 ~ 0 )) %>%
  set_value_labels(rh_anc_prgcomp = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_prgcomp = "Informed of pregnancy complications during ANC visit")

# //Blood pressure measured
IRdata <- IRdata %>%
  mutate(rh_anc_bldpres =
           case_when(
             m42c_1 == 1 & ancany==1 ~ 1  ,
             ancany==1 ~ 0 )) %>%
  set_value_labels(rh_anc_bldpres = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_bldpres = "Blood pressure was taken during ANC visit")

# //Urine sample taken
IRdata <- IRdata %>%
  mutate(rh_anc_urine =
           case_when(
             m42d_1 == 1 & ancany==1 ~ 1  ,
             ancany==1 ~ 0 )) %>%
  set_value_labels(rh_anc_urine = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_urine = "Urine sample was taken during ANC visit")

# //Blood sample taken
IRdata <- IRdata %>%
  mutate(rh_anc_bldsamp =
           case_when(
             m42e_1 == 1 & ancany==1 ~ 1  ,
             ancany==1 ~ 0 )) %>%
  set_value_labels(rh_anc_bldsamp = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_bldsamp = "Blood sample was taken during ANC visit")

# //tetnaus toxoid injections
IRdata <- IRdata %>%
  mutate(rh_anc_toxinj =
           case_when(
             m1_1 >1 & m1_1 <8 ~ 1 ,
             v208 ==0 | age>=period ~ 99,
             TRUE ~ 0)) %>%
  replace_with_na(replace = list(rh_anc_toxinj = c(99))) %>%
  set_value_labels(rh_anc_toxinj = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_toxinj = "Received 2+ tetanus injections during last pregnancy")

# //neonatal tetanus
# 	*older surveys do not have this indicator. m1a_1 (number of tetanus injections before pregnancy) is needed to compute this indicator
if ("TRUE" %in% (!("m1a_1" %in% names(IRdata))))
  IRdata [[paste("m1a_1")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $m1a_1)))
{ m1a_included <- 0} else { m1a_included <- 1}


# * for surveys that have this indicator
if (m1a_included==1) {
IRdata <- IRdata %>%
mutate(ageyr=as.integer(age/12)) %>%
mutate(tet2lastp=
         case_when(
              m1_1 >1 & m1_1<8 ~ 1,
              TRUE ~ 0)) 
IRdata[["totet0"]] <- 0
IRdata[["totet0"]] <- ifelse(!is.na(IRdata[["m1_1"]]) & IRdata[["m1_1"]]>0 & IRdata[["m1_1"]]<8, IRdata[["m1_1"]], 0)
IRdata[["totet"]] <- IRdata[["totet0"]]
IRdata[["totet"]] <- ifelse(!is.na(IRdata[["m1a_1"]]) & IRdata[["m1a_1"]]>0 & IRdata[["m1a_1"]]<8, IRdata[["m1a_1"]] + IRdata[["totet0"]], IRdata[["totet0"]])

IRdata <- IRdata %>%    
mutate(lastinj=
         case_when(
              m1_1>0 & m1_1 <8 ~ 0,
              m1d_1 <20 & (m1_1==0 | (m1_1>7 & m1_1<9996)) ~ (m1d_1 - ageyr),
              TRUE ~9999)) %>%
  mutate(ttprotect = 
           case_when(
              tet2lastp ==1 ~ 1,
              totet>=2 & lastinj<=2 ~ 1,
              totet>=3 &  lastinj<=4 ~ 1,
              totet>=4 &  lastinj<=9 ~ 1,
              totet>=5 ~ 1,
              TRUE ~ 0))
IRdata[["rh_anc_neotet"]] <- IRdata[["ttprotect"]]
IRdata[["rh_anc_neotet"]] <- ifelse(IRdata[["bidx_01"]]!=1 | IRdata[["age"]]>=IRdata[["period"]], NA, IRdata[["ttprotect"]])

IRdata <- IRdata %>% 
  set_value_labels(rh_anc_neotet = c("No" = 0, "Yes" = 1 )) %>%
  set_variable_labels(rh_anc_neotet = "Protected against neonatal tetanus")

#if m1a_1 is not available in the dataset
} else {
  IRdata <- IRdata %>%
  mutate( rh_anc_neotet =NA)
}
