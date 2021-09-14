# ******************************************************************************
# Program: 			  FE_TFR.R
# Purpose: 		    Code to compute Crude Birth Rate  
# Data inputs: 		IR & PR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified: September 10 2021 by Mahmoud Elkasabi
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Indicators created in this file:
# CBR     "Crude birth rate"
# -----------------------------------------------------------------------------#
#
########################################################################################
# CBR
# TABLE 5.1

IRdata_FERT <- (IRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", "awfactt", "awfactu", "awfactr", "awfacte", "awfactw",
                           paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""), "v106", "v190")])
# National and urban/rural ASFR from IR data
ASFR <- as.data.frame(fert(IRdata_FERT,Indicator="asfr",EverMW = "Yes", AWFact = "awfactt"))
ASFRur <- as.data.frame(fert(IRdata_FERT,Indicator="asfr", Class = "v025",EverMW = "Yes", AWFact = "awfactu"))

# prepare the data
PRdata <- PRdata %>% 
  filter(hv103 == 1) %>%  # select de facto population
  mutate(wt = hv005/1000000) %>%  # create weight
  mutate( agegroup = cut(hv105, breaks= c(0,15,20,25,30,35,40,45,50,999),right = FALSE)) # create age groups

# De facto population counts (national and by urban/rural) from PR data
hh_pop <- PRdata %>% 
  summarise(total = sum(wt, na.rm = TRUE))

hh_popur <- PRdata %>% 
  group_by(hv025) %>%
  summarise(total = sum(wt, na.rm = TRUE))
hh_popur <- as.data.frame(hh_popur)

# De facto women counts (national and by urban/rural) from PR data
# National counts by age groups
women_pop <- PRdata %>% 
  filter(hv105 >= 15 & hv105 <= 49 & hv104 == 2) %>%  # select de facto women 15-49
  group_by(agegroup) %>%
  summarise(total = sum(wt, na.rm = TRUE))

CBR_pop = (women_pop$total/hh_pop$total)*ASFR$ASFR

# Urban/rural counts by age groups
women_popur <- PRdata %>% 
  filter(hv105 >= 15 & hv105 <= 49 & hv104 == 2) %>%  # select de facto women 15-49
  group_by(agegroup, hv025) %>%
  summarise(total = sum(wt, na.rm = TRUE))

women_popur <- as.data.frame(women_popur)
women_popur <- merge(women_popur, hh_popur, by = "hv025")

women_popur$CBR_popur <- (women_popur$total.x/women_popur$total.y) * ASFRur$ASFR[2:15]

CBR_urban = sum(women_popur$CBR_popur[women_popur$hv025==1])
CBR_rural = sum(women_popur$CBR_popur[women_popur$hv025==2])
CBR  = sum(CBR_pop)

CBRres <- as.data.frame(cbind(CBR,CBR_urban,CBR_rural))

write.xlsx(CBRres, "Tables_FE.xlsx", sheetName = "CBR",append=TRUE)
