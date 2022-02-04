# ******************************************************************************
# Program: 		ML_NETS_source.R
# Purpose: 		Code for source of nets  
# Data inputs: 		HR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: January 07, 2022 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_net_dist		"Received a net from mass distribution, ANC, immunization, or at birth"
# ml_net_source	"Source of mosquito net"
# -----------------------------------------------------------------------------#

# Reshaping the dataset to a long format to tabulate among nets
myvars <- c(paste("hhid"),
            paste("hml22_", 1:7, sep = ""))
HRdata_long1 <- reshape::melt(as.data.frame(HRdata[myvars]), id = c("hhid"))
HRdata_long1$idx <- str_sub(HRdata_long1$variable,-1,-1)
HRdata_long1$variable <- NULL
names(HRdata_long1)[names(HRdata_long1) == c("value")] <- c("hml22")

myvars <- c(paste("hhid"),
            paste("hml23_", 1:7, sep = ""))
HRdata_long2 <- reshape::melt(as.data.frame(HRdata[myvars]), id = c("hhid"))
HRdata_long2$idx <- str_sub(HRdata_long2$variable,-1,-1)
HRdata_long2$variable <- NULL
names(HRdata_long2)[names(HRdata_long2) == c("value")] <- c("hml23")

HRdata_long <- merge(HRdata_long1,
                     HRdata_long2, by = c("hhid", "idx"))

myvars <- c("hhid","hv005","hv025", "hv024", "hv270")

HRdata_long3 <- (as.data.frame(HRdata[myvars]))

HRdata_long <- merge(HRdata_long,
                     HRdata_long3, by = c("hhid"))

# Number of mosquito nets 
HRdata_long <- HRdata_long %>%
  mutate(ml_numnet = case_when(
    hml22>=0 & hml22<=99    ~ 1,
    TRUE ~ 0),
    ml_numnet = set_label(ml_numnet, label = "Number of mosquito nets"))

HRdata_long <- HRdata_long %>%
  filter(ml_numnet != 0)

# Received a mosquito net obtained from campaign, anc, or immunization 
HRdata_long <- HRdata_long %>%
  mutate(ml_net_dist = case_when(
    hml22>=1    ~ 1,
    TRUE ~ 0),
    ml_net_dist = set_label(ml_net_dist, label = "Received a net from mass distribution, ANC, immunization, or at birth"))

# Source of net
# Note hml23_ can have several country specific categories. Pleace check and adjust the code accordingly. 
# In the code below, several country specific categories were grouped in category 9. 
HRdata_long <- HRdata_long %>%
  mutate(ml_net_source = case_when(
    hml23==11 | hml23==12 | hml23==13   ~ 5,
    hml23>=20 & hml23<30 ~ 6,
    hml23==31   ~ 7,
    hml23==32   ~ 8,
    hml23 >32 & hml23 <96   ~ 9,
    hml23==96   ~ 10,
    hml23>97   ~ 11,
    hml22==1  ~ 1,
    hml22==2  ~ 2,
    hml22==3  ~ 3,
    hml22==4  ~ 4),
    ml_net_source = add_labels(ml_net_source, labels = c("mass distribution campaign"=1, "ANC visit"=2,"immunisation visit"=3,
                                                         "at birth"=4,"gov. facility"=5,"private facility"=6,"pharmacy"=7,
                                                         "shop/market"=8,"other country specific"=9,"other"=10,"don't know/missing"=11)),
    ml_net_source = set_label(ml_net_source, label = "Source of mosquito net"))


HRdata_long <- HRdata_long %>%
  mutate(wt = hv005/1000000)

# Source of mosquito nets
table_temp <-  HRdata_long %>% 
  calc_cro_rpct(
    cell_vars = list(hv025, hv024, hv270, total()),
    col_vars = list(ml_net_source),
    weight = wt,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Source of mosquito nets")

write.xlsx(table_temp, "Tables_ML.xlsx", sheetName = "hh_net_source",append=TRUE)

