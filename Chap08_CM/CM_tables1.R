# ******************************************************************************
# Program: 			  CM_tables1.R
# Purpose: 		    produce tables for perinatal mortality
# Author:				  Mahmoud Elkasabi
# Date last modified: September 23 2021 by Mahmoud Elkasabi
# ******************************************************************************

CM_PMRdata <- CM_PMRdata %>%
  mutate(wt=v005/1000000)

###########################################################################################################
# Stillbirths, tables are for the number of stillbirths
###########################################################################################################

table_temp1 <- CM_PMRdata %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1) %>% 
  mutate(Background = "National")  %>%
  mutate(Level = "All") 

# mother's age at birth
table_temp2 <- CM_PMRdata %>% group_by(mo_age_at_birth) %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1) %>%
  mutate (Background  = "mo_age_at_birth") %>%
  rename (Level = mo_age_at_birth)

# prenancy interval
table_temp3 <- CM_PMRdata %>% group_by(preg_interval) %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1) %>%
  mutate (Background  = "preg_interval") %>%
  rename (Level = preg_interval)

# residence
table_temp4 <- CM_PMRdata %>% group_by(v025) %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1) %>%
  mutate (Background  = "v025") %>%
  rename (Level = v025)

# region
table_temp5 <- CM_PMRdata %>% group_by(v024) %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1) %>%
  mutate (Background = "v024") %>%
  rename (Level = v024)

# education
table_temp6 <- CM_PMRdata %>% group_by(v106) %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1) %>%
  mutate (Background = "v106") %>%
  rename (Level = v106)

# wealth
table_temp7 <- CM_PMRdata %>% group_by(v190) %>% count(stillbirths, wt = wt) %>% filter(stillbirths==1)%>%
  mutate (Background  = "v190") %>%
  rename (Level = v190)

stillbirths <- rbind(table_temp1,table_temp2,table_temp3,table_temp4,table_temp5,table_temp6,table_temp7)

stillbirths$stillbirths <- NULL 
stillbirths <- stillbirths %>%
  rename (stillbirths = n)

###########################################################################################################
# Early neonatal deaths, tables are for the number of early neonatal deaths
###########################################################################################################

table_temp1 <- CM_PMRdata %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1) %>% 
  mutate(Background = "National")  %>%
  mutate(Level = "All") 

# mother's age at birth
table_temp2 <- CM_PMRdata %>% group_by(mo_age_at_birth) %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1) %>%
  mutate (Background  = "mo_age_at_birth") %>%
  rename (Level = mo_age_at_birth)

# prenancy interval
table_temp3 <- CM_PMRdata %>% group_by(preg_interval) %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1) %>%
  mutate (Background  = "preg_interval") %>%
  rename (Level = preg_interval)

# residence
table_temp4 <- CM_PMRdata %>% group_by(v025) %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1) %>%
  mutate (Background  = "v025") %>%
  rename (Level = v025)

# region
table_temp5 <- CM_PMRdata %>% group_by(v024) %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1) %>%
  mutate (Background = "v024") %>%
  rename (Level = v024)

# education
table_temp6 <- CM_PMRdata %>% group_by(v106) %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1) %>%
  mutate (Background = "v106") %>%
  rename (Level = v106)

# wealth
table_temp7 <- CM_PMRdata %>% group_by(v190) %>% count(earlyneonatal, wt = wt) %>% filter(earlyneonatal==1)%>%
  mutate (Background  = "v190") %>%
  rename (Level = v190)

earlyneonatal <- rbind(table_temp1,table_temp2,table_temp3,table_temp4,table_temp5,table_temp6,table_temp7)

earlyneonatal$earlyneonatal <- NULL 
earlyneonatal <- earlyneonatal %>%
  rename (earlyneonatal = n)

###########################################################################################################
# Perinatal mortality rate per 1000
###########################################################################################################

table_temp1 <- CM_PMRdata %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>% 
  mutate(Background = "National")  %>%
  mutate(Level = "All") 

# mother's age at birth
table_temp2 <- CM_PMRdata %>% group_by(mo_age_at_birth) %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>%
  mutate (Background  = "mo_age_at_birth") %>%
  rename (Level = mo_age_at_birth)

# prenancy interval
table_temp3 <- CM_PMRdata %>% group_by(preg_interval) %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>%
  mutate (Background  = "preg_interval") %>%
  rename (Level = preg_interval)

# residence
table_temp4 <- CM_PMRdata %>% group_by(v025) %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>%
  mutate (Background  = "v025") %>%
  rename (Level = v025)

# region
table_temp5 <- CM_PMRdata %>% group_by(v024) %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>%
  mutate (Background = "v024") %>%
  rename (Level = v024)

# education
table_temp6 <- CM_PMRdata %>% group_by(v106) %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>%
  mutate (Background = "v106") %>%
  rename (Level = v106)

# wealth
table_temp7 <- CM_PMRdata %>% group_by(v190) %>% summarise(cm_peri = weighted.mean(cm_peri,wt)) %>%
  mutate (Background  = "v190") %>%
  rename (Level = v190)

cm_peri <- rbind(table_temp1,table_temp2,table_temp3,table_temp4,table_temp5,table_temp6,table_temp7)

table_temp <- merge(stillbirths, earlyneonatal, by = c("Background", "Level"))
table_temp <- merge(table_temp, cm_peri, by = c("Background", "Level"))


write.xlsx(table_temp, "Tables_child_mort.xlsx", sheetName = "PMR",append=TRUE)

