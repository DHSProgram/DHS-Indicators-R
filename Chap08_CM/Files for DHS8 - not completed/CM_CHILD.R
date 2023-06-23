# ******************************************************************************
# Program: 			  CM_CHILD.R
# Purpose: 		    Produce child mortality indicators   
# Data inputs: 		BR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified:  September 15 2021 by Mahmoud Elkasabi
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Indicators created in this file:
# NNMR		"Neonatal Mortality Rate"
# PNNMR		"Post-neonatal Mortality Rate"
# IMR			"Infant Mortality Rate"
# CMR			"Child Mortality Rate"
# U5MR		"Under-5 Mortality Rate"
# -----------------------------------------------------------------------------#
#

BRdata <- BRdata %>%
  mutate(child_sex = b4) %>%
  mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
  mutate(months_age = b3-v011) %>%
  mutate(mo_age_at_birth =
           case_when(
             months_age < 20*12   ~ 1 ,
             months_age >= 20*12 & months_age < 30*12 ~ 2,
             months_age >= 30*12 & months_age < 40*12 ~ 3,
             months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
  mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
  mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
  mutate(birth_order =
           case_when(
             bord == 1  ~ 1,
             bord >= 2 & bord <= 3 ~ 2,
             bord >= 4 & bord <= 6 ~ 3,
             bord >= 7  ~ 4,
             bord == NA ~ 99)) %>%
  replace_with_na(replace = list(birth_order = c(99))) %>%
  mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
  mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
  mutate(prev_bint =
           case_when(
             b11 <= 23 ~ 1,
             b11 >= 24 & b11 <= 35 ~ 2,
             b11 >= 36 & b11 <= 47 ~ 3,
             b11 >= 48 ~ 4)) %>%
  mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))  %>%
  mutate(birth_size =
           case_when(
             m18 >= 4 & m18 <= 5 ~ 1,
             m18 <= 3 ~ 2,
             m18 > 5 ~ 99)) %>%
  mutate(birth_size = set_label(birth_size, label = "Birth size")) 

BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])

BRdata <- BRdata %>%
mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))

##################################################################################
# MORTALITY RATES ################################################################
##################################################################################

BRdata_CMORT <- (BRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", 
                            "b3", "b7", "v106", "v190", "child_sex", "mo_age_at_birth", "birth_order", "prev_bint","birth_size")])

# NNMR, PNNMR, IMR, CMR & U5MR
# TABLES 8.1, 8.2 and 8.3

# Different datasets for period ends: 5-9 and 10-14
BRdata_CMORT1 <- BRdata_CMORT
BRdata_CMORT2 <- BRdata_CMORT
BRdata_CMORT1$v008 <- BRdata_CMORT$v008 - 12 * (5)
BRdata_CMORT2$v008 <- BRdata_CMORT$v008 - 12 * (10)

  resn1 <- as.data.frame(chmort(BRdata_CMORT))
  resn1$period <- "0-4"

  resn2 <- as.data.frame(chmort(BRdata_CMORT1))
  resn2$period <- "5-9"
  resn3 <- as.data.frame(chmort(BRdata_CMORT2))
  resn3$period <- "10-14"

  resc <- as.data.frame(rbind(chmort(BRdata_CMORT, Class="v024",Period = 120),
                              chmort(BRdata_CMORT, Class="v025",Period = 120),
                              chmort(BRdata_CMORT, Class="v106",Period = 120),
                              chmort(BRdata_CMORT, Class="v190",Period = 120),
                              chmort(BRdata_CMORT, Class="child_sex",Period = 120),
                              chmort(BRdata_CMORT, Class="mo_age_at_birth",Period = 120),
                              chmort(BRdata_CMORT, Class="birth_order",Period = 120),
                              chmort(BRdata_CMORT, Class="prev_bint",Period = 120),
                              chmort(BRdata_CMORT, Class="birth_size",Period = 120)))
  
  resc$period <- "0-9"

  CHMORT <- vector("list", 4)
  CHMORT <- rbindlist(list(resn1,resn2,resn3,resc), fill = TRUE)
  
  CHMORT[["Class"]] <- ifelse(is.na(CHMORT[["Class"]]), "National", CHMORT[["Class"]])
  CHMORT <- CHMORT[CHMORT[["Class"]]!="missing",] 
  
  rownames(CHMORT) <- paste(seq(1:230) , rep(row.names(resn1),46))

write.xlsx(CHMORT, "Tables_child_mort.xlsx", sheetName = "CMR",append=TRUE)

########################################################################################

