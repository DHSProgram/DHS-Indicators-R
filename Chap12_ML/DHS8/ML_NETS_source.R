# ******************************************************************************
# Program: 		ML_NETS_source.R - DHS8 update
# Purpose: 		Code for source of nets  
# Data inputs: 		HR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: August 14, 2024 by Courtney Allen for DHS8 updates
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_net_dist		"Received a net from mass distribution, ANC, immunization, or at birth"
# ml_net_source	"Source of mosquito net"
# ml_net_used		"Someone slept under ITN net last night"
# ml_net_notused 	"Mosquito net not used" - NEW Indicator in DHS8
# ml_net_reasons	"Reason net was not used the night" - NEW Indicator in DHS8
# -----------------------------------------------------------------------------#

# Reshaping the dataset to a long format to tabulate among nets
HRdata_long <- HRdata %>% 
  select(c("hhid","hv005","hv025", "hv024", "hv270", 
           starts_with(c("hml10", "hml21", "hml22", "hml23", "hml24")))) %>%
  pivot_longer(cols = starts_with(c("hml10", "hml21", "hml22", "hml23", "hml24")),
               names_to = c("var", "idx"),
               names_sep = "_",
               values_to = "value" ) %>%
  pivot_wider(names_from = "var",
              values_from = "value") %>%
  filter(!is.na(hml22)) %>%
  rename_with(~paste0(., "_1"), hml10:hml24) # rename helps copy over value labels

# relabel values for reshape
HRdata_long <- HRdata_long %>% copy_labels_from(., HRdata) %>% rename_with(~sub("_1", "", .x), hml10_1:hml24_1)


# *** Different denominators for tables in DHS8 tabplan ***
# NOTE    An insecticide-treated net (ITN) is a factory-treated net that does not require any further treatment
HRdata_long <- HRdata_long %>% mutate(
  ITNnet = hml10) %>%
  set_value_labels(ITNnet = c("ITN"=1,
                              "non-ITN"=0)) %>%
  set_variable_labels(ITNnet= "Type of net")


# Create weight for tables 
HRdata_long <- HRdata_long %>%
mutate(wt = hv005/1000000)

# Number of mosquito nets 
HRdata_long <- HRdata_long %>%
  mutate(ml_numnet = case_when(
    hml22>=0 & hml22<=99    ~ 1,
    TRUE ~ 0)) %>%
  set_variable_labels(ml_numnet= "Number of mosquito nets")

HRdata_long <- HRdata_long %>%
  filter(ml_numnet != 0)

# Received a mosquito net obtained from campaign, anc, or immunization 
HRdata_long <- HRdata_long %>%
  mutate(ml_net_dist = case_when(
    hml22>=1    ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_net_dist = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_net_dist="Received a net from mass distribution, ANC, immunization, or at birth")

# Source of net
# NOTE hml23_* can have several country specific categories. Please check and adjust the code accordingly. 
# In the code below, several country specific categories were grouped in category 9. 
HRdata_long <- HRdata_long %>%
  mutate(ml_net_source = case_when(
    hml22==1  ~ 1,
    hml22==2  ~ 2,
    hml22==3  ~ 3,
    hml23==10 | hml23==11 | hml23==12 | hml23==13   ~ 4,
    hml23>=20 & hml23<30 ~ 5,
    hml23==31 ~ 6,
    hml23==32 ~ 7,
    hml23==33 ~ 8,
    hml23==34 ~ 9,
    hml23==35 | hml22==4 ~ 10,
    hml23 >35 & hml23 <96   ~ 11,
    hml23==96 ~ 12,
    hml23>97  ~ 13)) %>%
  set_value_labels(ml_net_source = c("mass distribution campaign"=1, 
                                          "ANC visit"=2,
                                          "immunisation visit"=3,
                                          "gov. facility"=4,
                                          "private facility"=5,
                                          "pharmacy"=6,
                                          "shop/market"=7,
                                          "community health worker (CHW)"=8,
                                          "religious institution"=9,
                                          "school"=10,
                                          "other country specific"=11,
                                          "other"=12,
                                          "don't know/missing"=13))  %>%
      set_variable_labels(ml_net_source= "Source of mosquito net")


# Net used last night (someone slept under the net)
HRdata_long <- HRdata_long %>%
  mutate(ml_net_used = case_when(
    hml21==1   ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_net_used = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_net_used="Someone slept under net last night")


# Mosquito net NOT used (no one slept under this net last night)- NEW Indicator in DHS8
# NOTE: this is similar to the indicator ml_net_used, but is tabulated for different types of nets
HRdata_long <- HRdata_long %>% filter(!is.na(hml21)) %>%
  mutate(ml_net_notused = case_when(
    hml21 == 0 ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_net_notused = c("not used"=1, 
                                         "used or not sure"=0)) %>%
  set_variable_labels(ml_net_notused= "Mosquito net not used")



# Reasons did not use net - NEW Indicator in DHS8
HRdata_long <- HRdata_long %>% 
  mutate(ml_net_reason=case_when(
    hml24 > 8 & hml24 <96 ~ 9, # this groups country-specific reasons, check survey if they are of interest to you
    TRUE ~ hml24)) %>%
  set_value_labels(ml_net_reason = c("too hot"=1,
                                                "don't like net shape/color/size"=2,
                                                "don't like smell"=3,
                                                "unable to hang net"=4,
                                                "slept outdoors"=5,
                                                "usual user didn't sleep here"=6,
                                                "no mosquitoes/no malaria"=7,
                                                "extra net/saving for later"=8,
                                                "other country specific"=9,
                                                "other"=96)) %>%
set_variable_labels(ml_net_reason="Reason net was not used the night")



# TABLE-----Source of ITN mosquito nets
table_temp <-  HRdata_long %>% 
  cross_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_net_source),
    subgroup = ITNnet==1,
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Source of ITN mosquito nets")

table_temp
# save to workbook
sh = addWorksheet(wb, "hh_net_source")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

# TABLE-----Source of mosquito nets by type
table_temp <-  HRdata_long %>%
  cross_rpct(
    cell_vars = list(ITNnet, total()),
    col_vars = list(ml_net_source),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%
  set_caption("Source of mosquito nets by type")

table_temp
# save to workbook
sh = addWorksheet(wb, "hh_net_source_all")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)


# TABLE-----Use of existing ITNs last night
table_temp <-  HRdata_long %>% 
    cross_rpct(
      subgroup = ITNnet==1,
      cell_vars = list(hv025, hv024, hv270, total()),
      col_vars = list(ml_net_used),
      weight = wt,
      total_label = "N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Use of existing ITNs")
  
# save to workbook
sh = addWorksheet(wb, "hh_ITN_use")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

  

# TABLE-----Nets not used last night by type of net
table_temp <-  HRdata_long %>% 
    cross_rpct(
      cell_vars = list(hv025, hv024, hv270, ITNnet, total()),
      col_vars = list(ml_net_notused),
      weight = wt,
      total_label = "N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Net not used last night")
  
# save to workbook
sh = addWorksheet(wb, "hh_net_notused")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

  
 # TABLE-----Reason ITN nets not used
table_temp <-  HRdata_long %>% 
  cross_rpct(
    subgroup = ITNnet==1,  
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_net_reason, total()),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Reason ITN net not used last night")

# save to workbook
sh = addWorksheet(wb, "hh_net_reason")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)

  
  # TABLE-----Reason nets not used by type of net
  table_temp <-  HRdata_long %>% 
    cross_rpct(
      cell_vars = list(ITNnet, total()),
      col_vars = list(ml_net_reason, total()),
      weight = wt,
      total_label = "N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Reason net not used last night")
  
# save to workbook
sh = addWorksheet(wb, "hh_net_reason_all")
xl_write(table_temp, wb, sh)
saveWorkbook(wb, here(chap, "Tables_ML.xlsx"), overwrite = TRUE)
