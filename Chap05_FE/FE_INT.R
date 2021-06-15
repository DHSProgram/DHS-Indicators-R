# ******************************************************************************
# Program: 			  FE_INT.R
# Purpose: 		    Code fertility indicators from birth history reflecting birth intervals  
# Data inputs: 		BR survey list
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified: March 12 2021 by Mahmoud Elkasabi
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Variables created in this file:
#
#//BIRTH INTERVALS
#fe_int			"Birth interval of recent non-first births"
#fe_age_int		"Age groups for birth interval table"
#fe_bord			"Birth order"
#fe_pre_sex		"Sex of preceding birth"
#fe_pre_surv		"Survival of preceding birth"
#fe_int_med		"Median number of months since preceding birth"
#################################################################################

BRdata = BRdata %>%
  mutate(wt = v005/1000000)

# BIRTH INTERVALS

#fe_int
BRdata = BRdata %>%
mutate(fe_int = case_when(
  b11 <= 17 ~ 1,
  b11 >= 18 & b11 <= 23 ~ 2,
  b11 >= 24 & b11 <= 35 ~ 3,
  b11 >= 36 & b11 <= 47 ~ 4,
  b11 >= 48 & b11 <= 59 ~ 5,
  b11 >= 60 ~ 6)) %>%
  mutate(fe_int = set_label(fe_int, label = "Birth interval of recent non-first births")) %>%
  mutate(fe_int = factor(fe_int, levels = c(1,2,3,4,5,6), 
                         labels = c("less than 17 months", "18-23 months", "24-35 months", "36-47 months", "48-59 months", "60+ months" )))
  
#fe_age_int
BRdata = BRdata %>%
  mutate(fe_age_int = case_when(
    v013 ==1 ~ 1,
    v013 >=2 & v013 <=3 ~ 2,
    v013 >= 4 & v013 <= 5  ~ 3,
    v013 >= 6 & v013 <= 7  ~ 4)) %>%
  mutate(fe_age_int = set_label(fe_age_int, label = "Age groups for birth interval table")) %>%
  mutate(fe_age_int = factor(fe_age_int, levels = c(1,2,3,4), labels = c("1", "2-3", "4-5", "6-7")))

#fe_bord
BRdata = BRdata %>%
  mutate(fe_bord = case_when(
    b0 < 2 ~ bord,
    b0 > 1 ~ bord - b0 + 1)) %>%
  mutate(fe_bord_cat = case_when(
    fe_bord ==1 ~ 1,
    fe_bord ==2 | fe_bord ==3 ~ 2,
    fe_bord >= 4 & fe_bord <= 6  ~ 4,
    fe_bord >= 7 ~ 7)) %>%
  mutate(fe_bord_cat = set_label(fe_bord_cat, label = "Birth order categories")) %>%
  mutate(fe_bord_cat = factor(fe_bord_cat, levels = c(1,2,4,7), labels = c("1", "2-3", "4-6", "7+")))


#fe_pre_sex & fe_pre_surv
BRdata = BRdata %>%
  arrange(caseid, bord) %>%
  mutate(ID = row_number()) %>%
  mutate(fe_pre_sex = 0) %>%
  mutate(fe_pre_surv = 0)

for (i in 2:(max(BRdata$ID))) {
  
  if (BRdata$caseid[i] == BRdata$caseid[i-1]) {
    BRdata$fe_pre_sex[i] <- BRdata$b4[i-1] 
    BRdata$fe_pre_surv[i] <- BRdata$b5[i-1] 
  }
}

BRdata = BRdata %>%
  filter(b19 <= 59 & !is.na(b11)) %>% 
  mutate(fe_pre_sex = factor(fe_pre_sex, levels = c(1,2), labels = c("Male", "Female"))) %>%
  mutate(fe_pre_sex = set_label(fe_pre_sex, label = "Sex of preceding birth")) %>%
  mutate(fe_pre_surv = factor(fe_pre_surv, levels = c(0,1), labels = c("Not alive", "Alive"))) %>%
  mutate(fe_pre_surv = set_label(fe_pre_surv, label = "Survival of preceding birth")) %>%  
  mutate(time = b11)



################################################################################################################

# TABLES  

# Table 5.5

table_temp = BRdata %>%
  calc_cro_rpct(
    cell_vars = list(fe_age_int, fe_pre_sex, fe_pre_surv, fe_bord_cat, v025, v024, v149, v190, total()),
    col_vars = fe_int,
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below")) %>%
  set_caption("Birth intervals")

table_temp 

write.xlsx(table_temp, "Tables_FE.xlsx", sheetName = "Birth_intervals",append=TRUE)

# Median number of months since preceding birth

# National
brdata2 = BRdata

dummy.median <- brdata2 %>% 
  summarise(weighted_median = weightedMedian(time, wt, na.rm=TRUE))
brdata2$weighted_median <- dummy.median$weighted_median

brdata2$dummy <- ifelse(brdata2$time<brdata2$weighted_median , 1, 0)

dummy.mean <- brdata2 %>% 
  summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
brdata2$sL <- dummy.mean$sL

brdata2$dummy <- ifelse(brdata2$time<=brdata2$weighted_median , 1, 0)

dummy.mean <- brdata2 %>% 
  summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
brdata2$sU <- dummy.mean$sU

brdata2 = brdata2 %>%
  mutate(smedian = round((weighted_median+(0.5-sL)/(sU-sL)),2)) 

medians_total <- brdata2 %>% 
  summarise(MEDIANS = mean(smedian, na.rm=TRUE))


# By subgroups
MEDIANFUN <- function(Class){
  
  brdata2 <- BRdata
  
  brdata2$class <- brdata2[[Class]]
  
  dummy.median <- brdata2 %>% 
    group_by(class) %>%
    summarise(weighted_median = weightedMedian(time, wt, na.rm=TRUE))
  brdata2 <- left_join(brdata2, dummy.median, by = 'class')
  
  brdata2$dummy <- ifelse(brdata2$time<brdata2$weighted_median , 1, 0)
  
  dummy.mean <- brdata2 %>% 
    group_by(class) %>%
    summarise(sL = weighted.mean(dummy, wt, na.rm=TRUE))
  brdata2 <- left_join(brdata2, dummy.mean, by = 'class')
  
  brdata2$dummy <- ifelse(brdata2$time<=brdata2$weighted_median , 1, 0)
  
  dummy.mean <- brdata2 %>% 
    group_by(class) %>%
    summarise(sU = weighted.mean(dummy, wt, na.rm=TRUE))
  brdata2 <- left_join(brdata2, dummy.mean, by = 'class')
  
  brdata2 = brdata2 %>%
    mutate(smedian = round((weighted_median+(0.5-sL)/(sU-sL)),2)) 
  
  medians_subgroups <- brdata2 %>% 
    group_by(class) %>%
    summarise(MEDIANS = mean(smedian, na.rm=TRUE))
  
  medians_subgroups <- as.data.frame(medians_subgroups)
  return(medians_subgroups)
  
}	

M1 <- MEDIANFUN(Class = "fe_age_int" )
M2 <- MEDIANFUN(Class = "fe_pre_sex" )
M3 <- MEDIANFUN(Class = "fe_pre_surv" )
M4 <- MEDIANFUN(Class = "fe_bord_cat" )
M5 <- MEDIANFUN(Class = "v025" )
M6 <- MEDIANFUN(Class = "v024" )
M7 <- MEDIANFUN(Class = "v149" )
M8 <- MEDIANFUN(Class = "v190" )

medians_total <- as.data.frame(medians_total)
medians_total$class <- 0
medians_total$class = factor(medians_total$class, levels = c(0), labels = c("National")) 

fe_int_med <- rbind(medians_total, M1,M2,M3,M4,M5,M6,M7,M8)

write.xlsx(fe_int_med, "Tables_FE.xlsx", sheetName = "fe_int_med",append=TRUE) 
