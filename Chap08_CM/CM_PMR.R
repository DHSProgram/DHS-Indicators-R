# ******************************************************************************
# Program: 		CM_PMR.R
# Purpose: 		Code to compute perinatal mortality  
# Data inputs: 		IR survey list
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: December 06 2021 by Mahmoud Elkasabi
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# cm_peri		"Perinatal mortality rate"
# -----------------------------------------------------------------------------#

# open IR dataset
# drop or add variables from this list as needed
IRdata_PMR <- (IRdata[, c("caseid", "v001", "v002", "v003", "v005", "v008", "v011", "v017", "v018", "v021", "v022",
                           "v023","v024", "v025", "v106", "v190", "v231", "v242",
                           paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""),
                           "vcal_1")])

# drop any case without a birth or termination in the calendar, just to speed up the code
IRdata_PMR <- IRdata_PMR[str_detect(trimws(IRdata_PMR[["vcal_1"]]), "B") == TRUE | str_detect(trimws(IRdata_PMR[["vcal_1"]]), "T") == TRUE,]


# find the last pregnancy reported before the calendar - needed for calculation of pregnancy interval
IRdata_PMR[["befcal"]] = 0
B3 <- c(paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""))

for (i in seq_along(B3)) {
  
  IRdata_PMR[["befcal"]] <- ifelse(IRdata_PMR[["befcal"]] == 0 & IRdata_PMR[[B3[i]]] < IRdata_PMR[["v017"]], IRdata_PMR[[B3[i]]], IRdata_PMR[["befcal"]])
  IRdata_PMR[["befcal"]] <- ifelse(is.na(IRdata_PMR[["befcal"]]), 0, IRdata_PMR[["befcal"]])
}  

IRdata_PMR[["befcal"]] <- ifelse(!is.na(IRdata_PMR[["v231"]]) & IRdata_PMR[["v231"]] > IRdata_PMR[["befcal"]] & IRdata_PMR[["v231"]] < IRdata_PMR[["v017"]] , IRdata_PMR[["v231"]], IRdata_PMR[["befcal"]])
IRdata_PMR[["befcal"]] <- ifelse(!is.na(IRdata_PMR[["v242"]]) & IRdata_PMR[["v242"]] > IRdata_PMR[["befcal"]] & IRdata_PMR[["v242"]] < IRdata_PMR[["v017"]] , IRdata_PMR[["v242"]], IRdata_PMR[["befcal"]])


# drop variables no longer needed before reshape
IRdata_PMR <- (IRdata_PMR[, c("caseid", "v001", "v002", "v003", "v005", "v008", "v011", "v017", "v018", "v021", "v022",
                                "v023","v024", "v025", "v106", "v190","vcal_1","befcal")])

# loop through all positions in the calendar and turn them into variables
CMC <- c(paste("cmc_", 1:80, sep=""))
TYPE <- c(paste("type_", 1:80, sep=""))

for (i in (1:80)) {
  
  IRdata_PMR[[CMC[i]]] = IRdata_PMR[["v017"]] + 80 - i
} 
for (i in (1:80)) {
  
  IRdata_PMR[[TYPE[i]]] = NA
  IRdata_PMR[[TYPE[i]]] <- ifelse(str_sub((IRdata_PMR[["vcal_1"]]), i, i) == "B",1, IRdata_PMR[[TYPE[i]]])
  IRdata_PMR[[TYPE[i]]] <- ifelse(str_sub((IRdata_PMR[["vcal_1"]]), i, i) == "T",3, IRdata_PMR[[TYPE[i]]])
  IRdata_PMR[[TYPE[i]]] <- ifelse(str_sub((IRdata_PMR[["vcal_1"]]), i, i+6) == "TPPPPPP",2, IRdata_PMR[[TYPE[i]]])
  
}

INDEX <- c(paste("i_", 1:80, sep=""))
for (i in (1:80)) {
  
  IRdata_PMR[[INDEX[i]]] = 80 - (i-1)
  
}

IRdata_PMR <- as.data.frame(IRdata_PMR)

PMRdata <- stats::reshape(IRdata_PMR,
                          direction = "long",
                          varying = list(cmc_ = 19:98, type_ = 99:178, i_ = 179:258),
                          v.names = c("cmc", "type", "i"),
                          timevar = "caseid")

#PMRdata <- PMRdata[order(PMRdata$id),]

# Set length of calendar to use
PMRdata <- PMRdata %>%
  mutate(type = add_labels(type, labels = c("Birth"=1, "Stillbirth"=2, "Miscarriage/abortion"=3)),
         type = set_label(type, label = "Type of pregnancy")) %>%
  mutate(cmc = set_label(cmc, label = "Century month code of event")) %>%
  mutate(callen = v018 + 59)

# If calendar is aligned right (as in original dataset), use the following:
PMRdata <- PMRdata %>%
  mutate(beg = v018) %>%
  mutate(end = callen)

# If calendar is aligned left (as it is in some datasets), use the following:
#PMRdata <- PMRdata %>%
#  mutate(beg = 1) %>%
#  mutate(end = 60)

PMRdata[["i"]] <- (PMRdata[["i"]] - 81) * -1

# Include only the five year period, and keep only births and stillbirths
PMRdata <- PMRdata %>%
  filter (PMRdata[["i"]] >= PMRdata[["beg"]] & 
                            PMRdata[["i"]] <= PMRdata[["end"]] &
                            (PMRdata[["type"]] ==1  | PMRdata[["type"]] ==2))

# calculate the pregnancy interval
# find the first position before the pregnancy (when it is not a "P")
PMRdata[["j"]] = NA
PMRdata[["j"]] <- ifelse(PMRdata[["type"]] >= 1 & PMRdata[["type"]] <= 3, regexpr("P[^P]", str_sub(PMRdata[["vcal_1"]], PMRdata[["i"]]+1, 81)), PMRdata[["j"]])
PMRdata[["j"]] <- ifelse(PMRdata[["j"]] == -1, 0, PMRdata[["j"]])
PMRdata[["j"]] <- ifelse(PMRdata[["j"]] == 0, 0, PMRdata[["j"]]+1)
PMRdata[["j"]] <- ifelse(PMRdata[["j"]] >0,  PMRdata[["j"]] +  PMRdata[["i"]] , PMRdata[["j"]])
table(PMRdata[["j"]])


# find last pregnancy before the current one - births first, then terminated pregnancies
PMRdata[["lb"]] <- NA
PMRdata[["lb"]] <- ifelse(PMRdata[["j"]] > 0, regexpr("B", str_sub(PMRdata[["vcal_1"]], PMRdata[["j"]], 81)), PMRdata[["lb"]])
PMRdata[["lb"]] <- ifelse(PMRdata[["lb"]] == -1, 0, PMRdata[["lb"]])

PMRdata[["lp"]] <- NA
PMRdata[["lp"]] <- ifelse(PMRdata[["j"]] > 0, regexpr("T", str_sub(PMRdata[["vcal_1"]], PMRdata[["j"]], 81)), PMRdata[["lp"]])
PMRdata[["lp"]] <- ifelse(PMRdata[["lp"]] == -1, 0, PMRdata[["lp"]])

# if the last birth was after the last terminated pregnancy, then use the birth
PMRdata[["lp"]] <- ifelse(PMRdata[["j"]] > 0 & 
                            (PMRdata[["lp"]] == 0 | (PMRdata[["lb"]] > 0 & PMRdata[["lb"]] < PMRdata[["lp"]])),
                          PMRdata[["lb"]],PMRdata[["lp"]])

# correct the offset of lp 
PMRdata[["lp"]] <- ifelse(PMRdata[["j"]] > 0 & PMRdata[["lp"]] > 0,
                          PMRdata[["lp"]] + PMRdata[["j"]] - 1,PMRdata[["lp"]])


# calculate the pregnancy interval if there was a birth or pregnancy in the calendar before the current one (only if type is 1,2,3)
PMRdata[["pregint"]] <- NA
PMRdata[["pregint"]] <- ifelse(PMRdata[["type"]] >= 1 & PMRdata[["type"]] <= 3 & !is.na(PMRdata[["lp"]]) &  PMRdata[["lp"]] != 0,
                               PMRdata[["lp"]] - PMRdata[["j"]],PMRdata[["pregint"]])

# calculate the pregnancy interval if there was a birth or pregnancy before the calendar (but not in the calendar) and before the current pregnancy (only if type is 1,2,3)
PMRdata[["k"]] <- 0
# adjust to exclude the length of the pregnancy - not currently used in DHS
#PMRdata[["k"]] <- ifelse(PMRdata[["j"]] > 0,
#                               PMRdata[["j"]]-PMRdata[["i"]],PMRdata[["k"]])

PMRdata[["pregint"]] <- ifelse(PMRdata[["type"]] >= 1 & PMRdata[["type"]] <= 3 & (is.na(PMRdata[["lp"]]) |  PMRdata[["lp"]] == 0) & PMRdata[["befcal"]] != 0,
                               PMRdata[["cmc"]] - PMRdata[["k"]] - PMRdata[["befcal"]],PMRdata[["pregint"]])

#end of calculation of the pregnancy interval

#sort by case identifiers and century month code of pregnancy end
PMRdata <- PMRdata[order(PMRdata$v001, PMRdata$v002, PMRdata$v003, PMRdata$cmc),]


# merge in birth history variables
# Open birth history

BRdatafile <- BRdata
births <-  read_dta(file.path(datapath,BRdatafile))

births <- (births[, c("v001", "v002", "v003", "bidx", "b3", "b6")])
births[["cmc"]] <- births[["b3"]]
#sort according to ID and CMC of birth
births <- births[order(births$v001, births$v002, births$v003, births$cmc),]

# Reopen the pregnancies files and merge in the twins
CM_PMRdata <- merge(PMRdata, births, by = c("v001", "v002", "v003", "cmc"),all.x = TRUE)

# drop a few mismatches between calendar and birth history to match table
CM_PMRdata <- CM_PMRdata[which(!(CM_PMRdata[["type"]] ==1  & is.na(CM_PMRdata[["bidx"]]))), ]

CM_PMRdata[["stillbirths"]] <- ifelse(CM_PMRdata[["type"]] == 2, 1, 0)
CM_PMRdata[["earlyneonatal"]] <- ifelse(CM_PMRdata[["type"]] == 1 & CM_PMRdata[["b6"]] >= 100 & CM_PMRdata[["b6"]] <= 106 & !is.na(CM_PMRdata[["b6"]]),1, 0)
# Perinatal mortality
CM_PMRdata[["cm_peri"]] <- ifelse(CM_PMRdata[["type"]] == 2 | (CM_PMRdata[["type"]] == 1 & CM_PMRdata[["b6"]] >= 100 & CM_PMRdata[["b6"]] <= 106  & !is.na(CM_PMRdata[["b6"]])), 1000, 0)


# code background variables
# mother's age at birth (years): <20, 20-29, 30-39, 40-49 
CM_PMRdata <- CM_PMRdata %>%
  mutate(months_age=cmc-v011) %>%
  mutate(mo_age_at_birth = case_when(
    months_age >= 0 & months_age <= 239 ~ 1,
    months_age >= 240 & months_age <= 359 ~ 2,
    months_age >= 360 & months_age <= 479 ~ 3,
    months_age >= 480 & months_age <= 600 ~ 4),
    mo_age_at_birth = add_labels(mo_age_at_birth, labels = c("< 20"=1, "20-29"=2, "30-39"=3, "40-49"=4)),
    mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
  mutate(preg_interval = case_when(
    pregint >= 0 & pregint <= 14 ~ 2,
    pregint >= 15 & pregint <= 26 ~ 3,
    pregint >= 27 & pregint <= 38 ~ 4,
    pregint >= 39 & pregint <= 9999 ~ 5,
    TRUE ~ 1),
    preg_interval = add_labels(preg_interval, labels = c("First pregnancy"=1, "< 15"=2, "15-26"=3, "27-38"=4, "39 or more"=5)),
    preg_interval = set_label(preg_interval, label = "Previous pregnancy interval in months"))
