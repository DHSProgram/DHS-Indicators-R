# ******************************************************************************
# Program: 		FF_PLAN.R
# Purpose: 		Code to compute fertility planning status in women  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: September 08 2021 by Mahmoud Elkasabi
# Note:	To construct the fertility planning status indicator, we need to include births in the
# five years before the survey as well as current pregnancy. This requires appending the
# data of births and pregnancies. 
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ff_plan_status		"Fertility planning status at birth of child"
# -----------------------------------------------------------------------------#

Data.Name <- IRdata

Data.Name$womanid <- seq.int(nrow(Data.Name))

# check whether b19 is in the file 
for (i in 1:6){
  if ("TRUE" %in% (!(paste("b19_0", i, sep = "") %in% names(Data.Name))))
    Data.Name[[paste("b19_0", i, sep = "")]] <- NA
}

if ("TRUE" %in% all(is.na(Data.Name$b19_01)))
{ suseb19 <- 0} else { suseb19 <- 1}

# 1. Construct a birth data ##########################################################
myvarsid <- c("womanid","caseid","v005", "v008", "v011", "v012","v201", "v213", "v214", "v225")

myvars <- c(myvarsid,
            paste("bord_0", 1:6, sep = ""), paste("b0_0", 1:6, sep = ""), paste("b3_0", 1:6, sep = ""), 
            paste("b19_0", 1:6, sep = ""), paste("m10_", 1:6, sep = ""))

Data.Name0 <- as.data.frame(Data.Name[myvars])

BIRTHS <- stats::reshape(Data.Name0,
                      direction = "long",
                      varying = list(bord_ = 11:16, b0_ = 17:22, b3_ = 23:28, b19_ = 29:34, m10_ = 35:40),
                      v.names = c("bord", "b0", "b3", "b19", "m10"),
                      timevar = "bidx")

BIRTHS <- BIRTHS[stats::complete.cases(BIRTHS$bord), ]

BIRTHS <- BIRTHS %>% 
  mutate(bord = case_when(
    b0 > 1 ~ bord-b0+1,
    TRUE ~ bord)) %>%
  mutate(interval = case_when(
    suseb19 == 1 ~ b19,
    suseb19 == 0 ~ v008-b3)) %>%
  filter(interval < 60) %>%
  mutate(interval = NULL) %>%
  mutate(age_at_birth = -2 + as.integer((b3-v011)/60)) %>%
  mutate(age_at_birth = case_when(
    age_at_birth < 1 ~ 1,
    TRUE ~ age_at_birth),
    age_at_birth = add_labels(age_at_birth, labels = c("<20"=1, "20-24"=2, "25-29"=3, "30-34"=4, "35-39"=5,"40-44"=6,"45-49"=7))) %>%
  mutate(id = NULL) %>%
  mutate(preg_duration = NA) %>%
  mutate(cmc_preg_delivery = NA) 

# 2. Construct a pregnancies data ##########################################################
myvars <- c("womanid","caseid","v005", "v008", "v011", "v012","v201", "v213", "v214", "v225")

Data.Name0 <- as.data.frame(Data.Name[myvars])
PREGNANCY <- Data.Name0[Data.Name0$v213 ==1,]

PREGNANCY <- PREGNANCY %>% 
  mutate(m10 = v225) %>%
  mutate(bidx = 0) %>%
  mutate(bord = v201+1) %>%
  mutate(preg_duration = v214) %>%
  mutate(preg_duration = case_when(
    v214 > 9  ~ 9,
    TRUE ~ preg_duration)) %>%
  mutate(cmc_preg_delivery = v008+(9-preg_duration)) %>%
  mutate(age_at_birth = -2 + as.integer((cmc_preg_delivery-v011)/60),
    age_at_birth = add_labels(age_at_birth, labels = c("<20"=1, "20-24"=2, "25-29"=3, "30-34"=4, "35-39"=5,"40-44"=6,"45-49"=7))) %>%
  mutate(age_at_birth = case_when(
    age_at_birth > 7  ~ 7,
    TRUE ~ age_at_birth))  %>%
  mutate(b0 = NA) %>%
  mutate(b3 = NA) %>%
  mutate(b19 = NA)

names(BIRTHS)
names(PREGNANCY)

BTH_BRG <- rbind(BIRTHS,PREGNANCY)

BTH_BRG <- BTH_BRG %>% 
  mutate(birth_order = bord) %>%
  mutate(birth_order = case_when(
    bord > 4  ~ 4,
    TRUE ~ birth_order)) %>% 
  mutate(ff_plan_status = m10,
         ff_plan_status = set_label(ff_plan_status, label = "Fertility planning status at birth of child")) %>%
  mutate(wt = v005/1000000)

#  Fertility planning status
table_temp = BTH_BRG %>%
  calc_cro_rpct(
    cell_vars = list(birth_order, age_at_birth, total()),
    col_vars = list(ff_plan_status, total()),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption(" Fertility planning status")

table_temp 

write.xlsx(table_temp, "Tables_FF.xlsx", sheetName = "ff_plan_status",append=TRUE)
