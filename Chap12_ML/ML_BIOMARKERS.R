# ******************************************************************************
# Program: 		ML_BIOMARKERS.R
# Purpose: 		Code anemia and malaria testing prevalence in children under 5 
# Data inputs: 		PR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor and Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: January 12, 2022 by Mahmoud Elkasabi
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

# Tested for Anemia
PRdata <- PRdata %>%
  mutate(ml_test_anemia = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & hc55!=0 ~ 0,
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & hc55==0 ~ 1),
    ml_test_anemia = set_label(ml_test_anemia, label = "Tested for anemia in children 6-59 months"))

# Tested for Parasitemia via microscopy
PRdata <- PRdata %>%
  mutate(ml_test_micmal = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & !(hml32==0 | hml32==1 | hml32==6)  ~ 0,
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml32==0 | hml32==1 | hml32==6)  ~ 1),
    ml_test_micmal = set_label(ml_test_micmal, label = "Tested for Parasitemia (via microscopy) in children 6-59 months"))

# Tested for Parasitemia via RDT
PRdata <- PRdata %>%
  mutate(ml_test_rdtmal = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & !(hml35==0 | hml35==1)  ~ 0,
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml35==0 | hml35==1)  ~ 1),
    ml_test_rdtmal = set_label(ml_test_rdtmal, label = "Tested for Parasitemia (via RDT) in children 6-59 months"))

# Prevalence
  
# Anemia in children 6-59 months
PRdata <- PRdata %>%
  mutate(ml_anemia = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hc55==0 & hv042==1 & !(hc56<80)  ~ 0,
    hv103==1 & hc1>=6 & hc1<=59 & hc55==0 & hv042==1 & hc56<80  ~ 1),
    ml_anemia = set_label(ml_anemia, label = "Anemia in children 6-59 months"))

# Parasitemia (via microscopy) in children 6-59 months
PRdata <- PRdata %>%
  mutate(ml_micmalpos = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml32==0 | hml32==6)  ~ 0,
    hml32==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1  ~ 1),
    ml_micmalpos = set_label(ml_micmalpos, label = "Parasitemia (via microscopy) in children 6-59 months"))

# Parasitemia (vis RDT) in children 6-59 months
PRdata <- PRdata %>%
  mutate(ml_rdtmalpos = case_when(
    hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml35==0)  ~ 0,
    hml35==1 & hv103==1 & hc1>=6 & hc1<=59 & hv042==1   ~ 1),
    ml_rdtmalpos = set_label(ml_rdtmalpos, label = "Parasitemia (via RDT) in children 6-59 months"))
