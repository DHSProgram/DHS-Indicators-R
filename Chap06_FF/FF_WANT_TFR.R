# ******************************************************************************
# Program: 		FF_WANT_TFR.R
# Purpose: 		Code to compute to  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: September 09 2021 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ff_want_tfr		"Wanted fertility rates"
# -----------------------------------------------------------------------------#

# Define wanted fertility ########################################################
##################################################################################

IRdata_FERT <- (IRdata[, c("caseid", "v001", "v002", "v003", "v005", "v008", "v011", "v021", "v022",
                           "v024", "v025", "v101", "v106", "v190", "v133", "v201", "v218",
                           "awfactt", "awfactu", "awfactr", "awfacte", "awfactw", "v613",
                           paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""),
                           paste("b5_0", 1:9, sep=""), paste("b5_", 10:20, sep=""),
                           paste("b6_0", 1:9, sep=""), paste("b6_", 10:20, sep=""),
                           paste("b7_0", 1:9, sep=""), paste("b7_", 10:20, sep=""))])

IRdata_FERT <- IRdata_FERT[stats::complete.cases(IRdata_FERT$v613), ]

B3 <- c(paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""))
B5 <- c(paste("b5_0", 1:9, sep=""), paste("b5_", 10:20, sep=""))
B6 <- c(paste("b6_0", 1:9, sep=""), paste("b6_", 10:20, sep=""))
B7 <- c(paste("b7_0", 1:9, sep=""), paste("b7_", 10:20, sep=""))
nlc <- c(paste("nlc_0", 1:9, sep=""), paste("nlc_", 10:20, sep=""))
cmc_of_death <- c(paste("cmc_of_death_0", 1:9, sep=""), paste("cmc_of_death_", 10:20, sep=""))
wanted <- c(paste("wanted_0", 1:9, sep=""), paste("wanted_", 10:20, sep=""))

for (i in seq_along(nlc)) {
  
  IRdata_FERT[[nlc[i]]] = 0
  IRdata_FERT[[cmc_of_death[i]]] = 0
  IRdata_FERT[[B6[i]]][is.na(IRdata_FERT[[B6[i]]])] = 0
  IRdata_FERT[[cmc_of_death[i]]] <- ifelse(as.integer(IRdata_FERT[[B6[i]]]/100)<3, IRdata_FERT[[B3[i]]] + IRdata_FERT[[B7[i]]], IRdata_FERT[[cmc_of_death[i]]])
  IRdata_FERT[[cmc_of_death[i]]] <- ifelse(as.integer(IRdata_FERT[[B6[i]]]/100)==3, IRdata_FERT[[B3[i]]] + IRdata_FERT[[B7[i]]] + 6, IRdata_FERT[[cmc_of_death[i]]])

  IRdata_FERT[[cmc_of_death[i]]][is.na(IRdata_FERT[[cmc_of_death[i]]])] <- 0
  IRdata_FERT[[B3[i]]][is.na(IRdata_FERT[[B3[i]]])] = 0  
  IRdata_FERT[[B5[i]]][is.na(IRdata_FERT[[B5[i]]])] = 0
}  

for (i in (1:19)) {
  
  lii = i + 1
    for (ii in (lii:20)) {
      IRdata_FERT[[nlc[i]]] <- ifelse((IRdata_FERT[[B5[ii]]]==0 & IRdata_FERT[[cmc_of_death[ii]]] >= IRdata_FERT[[B3[i]]] - 9) | (IRdata_FERT[[B5[ii]]]==1), IRdata_FERT[[nlc[i]]] + 1, IRdata_FERT[[nlc[i]]])
    }
  IRdata_FERT[[wanted[i]]] <- 0
  IRdata_FERT[[wanted[i]]] <- ifelse(IRdata_FERT[[nlc[i]]]<IRdata_FERT[["v613"]] & IRdata_FERT[["v613"]] <=98, 1, IRdata_FERT[[wanted[i]]])

  IRdata_FERT[[B3[i]]][IRdata_FERT[[wanted[i]]] == 0] <- 0 
} 


##################################################################################
# FERTILITY RATES ################################################################
##################################################################################

library("DHS.rates")
IRdata_FERT <- (IRdata_FERT[, c("v021", "v022","v024", "v025", "v005", "v008","v011", "awfactt", "awfactu", "awfactr", "awfacte", "awfactw",
                           paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""), "v106", "v190")])

# TFR
FERT <- vector("list", 1)

varlist <- c("tfr")

for (i in seq_along(varlist)) {
  
  resn1 <- as.data.frame(fert(IRdata_FERT,Indicator=varlist[i], EverMW = "Yes", AWFact = "awfactt"))
  resn1$period <- "0-2"

  resc <- as.data.frame(rbind(fert(IRdata_FERT,Indicator=varlist[i], Class="v024", EverMW = "Yes", AWFact = "awfactr"),
                              fert(IRdata_FERT,Indicator=varlist[i], Class="v025", EverMW = "Yes", AWFact = "awfactu"),
                              fert(IRdata_FERT,Indicator=varlist[i], Class="v106", EverMW = "Yes", AWFact = "awfacte"),
                              fert(IRdata_FERT,Indicator=varlist[i], Class="v190", EverMW = "Yes", AWFact = "awfactw")))
  resc$period <- "0-2"
  FERT[[i]] <- rbindlist(list(resn1,resc), fill = TRUE)
  
}

write.xlsx(FERT[[1]], "Tables_FF.xlsx", sheetName = "Wanted_TFR",append=TRUE)


########################################################################################