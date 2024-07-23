# ******************************************************************************
# Program: 			  AM_rates.R
# Purpose: 		    Produce adult and maternal mortality rates   
# Data inputs: 		IR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified:  December 09 2021 by Mahmoud Elkasabi
# ******************************************************************************
# the following indicators are produced ========================================
# Adult mortality - women & men ------------------------------------------------
# ASMR "Age Specific Mortality Rate" (ASMR)"
# AAMR "Age Adjusted Mortality Rate"
# q_15_to_50	(aka 15q30)	"probability of dying between ages 15 and 50"

# Pregnancy related - women-----------------------------------------------------
# ASPRMR "Age Specific Pregnancy Related Mortality Rate"
# AAPRMR "Age Adjusted Pregnancy Related Mortality Rate" 
# PRMRatio "Pregnancy Related Mortality Ratio"
# PMDF_PRMR	"proportions pregnancy related among deaths of females of reproductive age"
# prLTR "lifetime risk of pregnancy-related death"

# Maternal - women--------------------------------------------------------------
# ASMMR "Age Specific Maternal Mortality Rate" 
# AAMMR "Age Adjusted Maternal Mortality Rate"
# MMRatio "Maternal Mortality Ratio"
# PMDF_MMR	"proportions maternal among deaths of females of reproductive age"
# mLTR	"lifetime risk of maternal death"
# ----------------------------------------------------------------------------*/

IRdata_AMORT <- IRdata

TFR7 <- as.data.frame(fert(IRdata_AMORT,Indicator="tfr", Period = 84, EverMW = "Yes", AWFact = "awfactt"))[1]

##################################################################################
# ADULT MORTALITY RATES ##########################################################
##################################################################################

# Note: this part produces pregnancy-related death: mm9 is 2, 3, 5, or 6, and mm16 is not used

ADMORT <- vector("list", 5)

varlist <- c("asmr", "aamr", "asprmr", "aaprmr", "prmr")

for (i in seq_along(varlist)) {
  
  ADMORT[[i]]  <- as.data.frame(admort(IRdata_AMORT,Indicator=varlist[i]))

}

write.xlsx(ADMORT[[1]], "Tables_AM.xlsx", sheetName = "ASMR",append=TRUE)
write.xlsx(ADMORT[[2]], "Tables_AM.xlsx", sheetName = "AAMR",append=TRUE)
write.xlsx(ADMORT[[3]], "Tables_AM.xlsx", sheetName = "ASPRMR",append=TRUE) #pregnancy-related death
write.xlsx(ADMORT[[3]][,1:2], "Tables_AM.xlsx", sheetName = "PMDF_PRMR",append=TRUE)  #pregnancy-related death
write.xlsx(ADMORT[[4]], "Tables_AM.xlsx", sheetName = "AAPRMR",append=TRUE)  #pregnancy-related death
write.xlsx(ADMORT[[5]], "Tables_AM.xlsx", sheetName = "PRMRatio",append=TRUE)  #pregnancy-related death


# probability of dying between ages 15 and 50 ##################################
mx <- c(ADMORT[[1]]$ASMR[1:7]/1000,ADMORT[[1]]$ASMR[8:14]/1000)
q5 <- 5*mx / (1+2.4*mx)
q_15 <- c(1000*(1- (prod(1 -q5[1:7], na.rm = FALSE))),1000*(1- (prod(1 -q5[8:14], na.rm = FALSE))))

RESULTSq <- matrix(0, nrow = 1, ncol = 2)
dimnames(RESULTSq) <- list(NULL, c("q_15_to_50_women", "q_15_to_50_men") )
RESULTSq[1, ] <- c(as.numeric(q_15[1]), as.numeric(q_15[2]))

write.xlsx(RESULTSq, "Tables_AM.xlsx", sheetName = "q_15_to_50",append=TRUE)


# lifetime risk of pregnancy-related death #####################################
prLTR <- 1 - (1- ADMORT[[5]][,2]/100000)^TFR7[1]

RESULTS <- matrix(0, nrow = 1, ncol = 3)
dimnames(RESULTS) <- list(NULL, c("TFR", "PRMRatio", "prLTR") )
RESULTS[1, ] <- c(as.numeric(TFR7), as.numeric(ADMORT[[5]][,2]), as.numeric(prLTR))

write.xlsx(RESULTS, "Tables_AM.xlsx", sheetName = "prLTR",append=TRUE)


################################################################################

# maternal death ###############################################################
# Note: this part produces maternal death: mm9 is 2, 3, or 5,    and mm16=0 
# check if mm16: exist
if ("TRUE" %in% (!("mm16_01" %in% names(IRdata_AMORT))))
  IRdata_AMORT [[paste("mm16_01")]] <- NA
if ("TRUE" %in% all(is.na(IRdata_AMORT$mm16_01)))
{ check <- 0} else { check <- 1}

if (check==1) {
 
  ADMORT2 <- vector("list", 3)
  
  varlist <- c("asmmr", "aammr", "mmr")
  
  for (i in seq_along(varlist)) {
    
    ADMORT2[[i]]  <- as.data.frame(admort(IRdata_AMORT,Indicator=varlist[i]))
    
  }
  
  write.xlsx(ADMORT2[[1]], "Tables_AM.xlsx", sheetName = "ASMMR",append=TRUE) # maternal death
  write.xlsx(ADMORT2[[1]][,1:2], "Tables_AM.xlsx", sheetName = "PMDF_MMR",append=TRUE) # maternal death
  write.xlsx(ADMORT2[[2]], "Tables_AM.xlsx", sheetName = "AAMMR",append=TRUE) # maternal death
  write.xlsx(ADMORT2[[3]], "Tables_AM.xlsx", sheetName = "MMRatio",append=TRUE) # maternal death
  
  # lifetime risk of maternal death
  mLTR <- 1 - (1- ADMORT2[[3]][,2]/100000)^TFR7[1]
  
  RESULTS <- matrix(0, nrow = 1, ncol = 3)
  dimnames(RESULTS) <- list(NULL, c("TFR", "MMRatio", "mLTR") )
  RESULTS[1, ] <- c(as.numeric(TFR7), as.numeric(ADMORT2[[3]][,2]), as.numeric(mLTR))
  
  write.xlsx(RESULTS, "Tables_AM.xlsx", sheetName = "mLTR",append=TRUE)
  
}


