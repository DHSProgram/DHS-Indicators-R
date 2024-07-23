# ******************************************************************************
# Program: 		ML_EXISTING_ITN.R
# Purpose: 		Code for source of nets  
# Data inputs: 		HR survey list
# Data outputs:		coded variables
# Author:		Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: January 07, 2022 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_sleepnet 		"Someone slept under net last night"
# -----------------------------------------------------------------------------#

# Reshaping the dataset to a long format to tabulate among nets
myvars <- c(paste("hhid"),
            paste("hml10_", 1:7, sep = ""))
HRdata_long1 <- reshape::melt(as.data.frame(HRdata[myvars]), id = c("hhid"))
HRdata_long1$idx <- str_sub(HRdata_long1$variable,-1,-1)
HRdata_long1$variable <- NULL
names(HRdata_long1)[names(HRdata_long1) == c("value")] <- c("hml10")

myvars <- c(paste("hhid"),
            paste("hml21_", 1:7, sep = ""))
HRdata_long2 <- reshape::melt(as.data.frame(HRdata[myvars]), id = c("hhid"))
HRdata_long2$idx <- str_sub(HRdata_long2$variable,-1,-1)
HRdata_long2$variable <- NULL
names(HRdata_long2)[names(HRdata_long2) == c("value")] <- c("hml21")

HRdata_long <- merge(HRdata_long1,
                               HRdata_long2, by = c("hhid", "idx"))

myvars <- c("hhid","hv005","hv025", "hv024", "hv270")

HRdata_long3 <- (as.data.frame(HRdata[myvars]))

HRdata_long <- merge(HRdata_long,
                     HRdata_long3, by = c("hhid"))

# Identifying if the net was used 
HRdata_long <- HRdata_long %>%
  mutate(ml_sleepnet = case_when(
    hml21==1   ~ 1,
    TRUE ~ 0),
    ml_sleepnet = add_labels(ml_sleepnet, labels = c("No"=0, "Yes"=1)),
    ml_sleepnet = set_label(ml_sleepnet, label = "Someone slept under net last night"))

# Net is an ITN
HRdata_long <- HRdata_long %>%
  mutate(ml_ownnet = case_when(
    hml10==1   ~ 1,
    TRUE ~ 0),
    ml_ownnet = add_labels(ml_ownnet, labels = c("No"=0, "Yes"=1)),
    ml_ownnet = set_label(ml_ownnet, label = "Net is an ITN"))

HRdata_long <- HRdata_long %>%
  mutate(wt = hv005/1000000)

# Use of existing ITNs
table_temp <-  HRdata_long %>% 
  filter(ml_ownnet==1) %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_sleepnet),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
   set_caption("Use of existing ITNs")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "hh_ITN_use",append=TRUE)

