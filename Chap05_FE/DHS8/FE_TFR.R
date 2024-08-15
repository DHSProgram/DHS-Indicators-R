# ******************************************************************************
# Program: 			  FE_TFR.R
# Purpose: 		    Code to compute fertility rates  
# Data inputs: 		IR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified:  September 10 2021 by Mahmoud Elkasabi
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Indicators created in this file:
# ASFR		"age specific fertility rates"
# TFR			"fertility rates"
# GFR			"general fertility rate"
# -----------------------------------------------------------------------------#
#
##################################################################################
# FERTILITY RATES ################################################################
##################################################################################

IRdata_FERT <- (IRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", "awfactt", "awfactu", "awfactr", "awfacte", "awfactw",
                           paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""), "v106", "v190")])

# TFR, GFR & ASFR
# TABLES 5.1 and 5.2

# Different datasets for period ends: 5-9, 10-14 and 15-19
IRdata_FERT1 <- IRdata_FERT
IRdata_FERT2 <- IRdata_FERT
IRdata_FERT3 <- IRdata_FERT
IRdata_FERT1$v008 <- IRdata_FERT$v008 - 12 * (5)
IRdata_FERT2$v008 <- IRdata_FERT$v008 - 12 * (10)
IRdata_FERT3$v008 <- IRdata_FERT$v008 - 12 * (15)

FERT <- vector("list", 3)

varlist <- c("tfr", "gfr", "asfr")

for (i in seq_along(varlist)) {
  
  resn1 <- as.data.frame(fert(IRdata_FERT,Indicator=varlist[i], EverMW = "Yes", AWFact = "awfactt"))
  resn1$period <- "0-2"
  resn2 <- as.data.frame(fert(IRdata_FERT,Indicator=varlist[i], Period = 60, EverMW = "Yes", AWFact = "awfactt"))
  resn2$period <- "0-4"
  
  resn3 <- as.data.frame(fert(IRdata_FERT1,Indicator=varlist[i], Period = 60, EverMW = "Yes", AWFact = "awfactt"))
  resn3$period <- "5-9"
  resn4 <- as.data.frame(fert(IRdata_FERT2,Indicator=varlist[i], Period = 60, EverMW = "Yes", AWFact = "awfactt"))
  resn4$period <- "10-14"
  resn5 <- as.data.frame(fert(IRdata_FERT3,Indicator=varlist[i], Period = 60, EverMW = "Yes", AWFact = "awfactt"))
  resn5$period <- "15-19"
  
  resc <- as.data.frame(rbind(fert(IRdata_FERT,Indicator=varlist[i], Class="v024", EverMW = "Yes", AWFact = "awfactr"),
                              fert(IRdata_FERT,Indicator=varlist[i], Class="v025", EverMW = "Yes", AWFact = "awfactu"),
                              fert(IRdata_FERT,Indicator=varlist[i], Class="v106", EverMW = "Yes", AWFact = "awfacte"),
                              fert(IRdata_FERT,Indicator=varlist[i], Class="v190", EverMW = "Yes", AWFact = "awfactw")))
  resc$period <- "0-2"
  FERT[[i]] <- rbindlist(list(resn1,resn2,resn3,resn4,resn5,resc), fill = TRUE)
  
}

write.xlsx(FERT[[1]], "Tables_FE.xlsx", sheetName = "TFR",append=TRUE)
write.xlsx(FERT[[2]], "Tables_FE.xlsx", sheetName = "GFR",append=TRUE)
write.xlsx(FERT[[3]], "Tables_FE.xlsx", sheetName = "ASFR",append=TRUE)

########################################################################################