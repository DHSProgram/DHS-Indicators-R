# ******************************************************************************
# Program: 		ML_BIOMARKERS.R - No changes in DHS8
# Purpose: 		Code anemia and malaria testing prevalence in children under 5 
# Data inputs: 		PR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: August 02, 2024 by Courtney Allen for DHS8 update
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_test_anemia	Tested for anemia in children 6-59 months
# ml_test_micmal 	Tested for Parasitemia (via microscopy) in children 6-59 months
# ml_test_rdtmal	Tested for Parasitemia (via RDT) in children 6-59 months

# ml_anemia		Anemia in children 6-59 months
# ml_micmalpos 	Parasitemia (via microscopy) in children 6-59 months
# ml_rdtmalpos	Parasitemia (via RDT) in children 6-59 months
# -----------------------------------------------------------------------------#

# Testing

# Tested for Anemia (de facto children who were eligible for hemoglobin measurement)
PRdata <- PRdata %>%
  mutate(ml_test_anemia = case_when(
    hv120==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & hc55==0 ~ 1,
    hv120==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1  ~ 0)) %>%
  set_value_labels(ml_test_anemia = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_test_anemia = "Tested for anemia in children 6-59 months")
    
    

# Tested for Parasitemia via microscopy
PRdata <- PRdata %>%
  mutate(ml_test_micmal = case_when(
    hv120==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml32==0 | hml32==1 | hml32==6)  ~ 1,
    hv120==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1  ~ 0)) %>%
  set_value_labels(ml_test_micmal = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_test_micmal = "Tested for Parasitemia (via microscopy) in children 6-59 months")

# Tested for Parasitemia via RDT
PRdata <- PRdata %>%
  mutate(ml_test_rdtmal = case_when(
    hv120==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml35==0 | hml35==1)  ~ 1,
    hv120==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1 ~ 0)) %>%
  set_value_labels(ml_test_rdtmal = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_test_rdtmal = "Tested for Parasitemia (via RDT) in children 6-59 months")

# Prevalence
  
# Anemia in children 6-59 months
PRdata <- PRdata %>%
  mutate(ml_anemia = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hc55==0 & hv042==1 & !(hc56<80)  ~ 0,
    hv103==1 & hc1>=6 & hc1<=59 & hc55==0 & hv042==1 & hc56<80  ~ 1)) %>%
  set_value_labels(ml_anemia = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_anemia = "Anemia in children 6-59 months")

# Parasitemia (via microscopy) in children 6-59 months
PRdata <- PRdata %>%
  mutate(ml_micmalpos = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml32==0 | hml32==6)  ~ 0,
    hml32==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1  ~ 1)) %>%
  set_value_labels(ml_micmalpos = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_micmalpos = "Parasitemia (via microscopy) in children 6-59 months")

# Parasitemia (vis RDT) in children 6-59 months
PRdata <- PRdata %>%
  mutate(ml_rdtmalpos = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml35==0)  ~ 0,
    hml35==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1   ~ 1)) %>%
  set_value_labels(ml_rdtmalpos = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_rdtmalpos = "Parasitemia (via RDT) in children 6-59 months")
