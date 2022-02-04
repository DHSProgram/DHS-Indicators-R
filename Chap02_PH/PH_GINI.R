# /*****************************************************************************************************
# Program: 			  PH_GINI.R
# Purpose: 			  Code to compute the Gini coefficient
# Data inputs: 		PR dataset
# Data outputs:		coded variables
# Author:				  Tom Pullum, translated into R by Mahmoud Elkasabi
# Date last modified: January 31, 2022 by Mahmoud Elkasabi 
# **If you want to add other country specific background characteristics, add this as a new calc_gini function as a relevant Class in lines 139 and 141
# *****************************************************************************************************/

PRdata_gini <- select(PRdata,hv005, hv012, hv024, hv025, hv101, hv270, hv271)

calc_gini <- function(Data.Name, Class = NULL){

  if (is.null(Class)){
    
    Dat <- Data.Name
# Keep one case per household (e.g. hvidx=1 or hv101=1 or hvidx=hv003) in households with at least one de jure member
    Dat <- Dat %>% filter(hv101==1 & hv012>0) %>%
  mutate(cases=hv012*hv005/1000000)   # Each household has hv012 de jure members

Dat$category=1+as.integer(99*(Dat$hv271 - min(Dat$hv271))/(max(Dat$hv271)-min(Dat$hv271)))
MIN <- min(Dat$hv271)

# IMPORTANT! The assets for the household are weighted by hv005 but not multiplied by the number of household members
CASES <- Dat %>% group_by(category) %>% summarise(cases = sum(cases, na.rm=TRUE)) 

Dat$assets <- (Dat$hv271-MIN)*Dat$hv005/1000000

ASSETS <- Dat %>% 
  group_by(category) %>% 
  summarise(assets = sum(assets, na.rm=TRUE)) 

Dat_cat <- merge(ASSETS, CASES, by = c("category"), all.x = TRUE, all.y = TRUE)

# Note: some categories may be empty; best to renumber them; must be certain they are in sequence
Dat_cat <- transform(Dat_cat, category = as.numeric(factor(category)))

Dat_cat$cases_prop= Dat_cat$cases/sum(Dat_cat$cases)
Dat_cat$assets_prop= Dat_cat$assets/sum(Dat_cat$assets)

# Calculate cumulative proportions for cases and assets. 
Dat_cat[["cases_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["cases_prop"]], 0)
Dat_cat[["assets_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["assets_prop"]], 0)
Dat_cat[["term"]] <- ifelse(Dat_cat[["category"]]==1, 
                                     Dat_cat[["cases_prop"]]*Dat_cat[["assets_cumprop"]], 0)

ncats = (2:max(Dat_cat$category))
for (i in (ncats)) {

  Dat_cat[["cases_cumprop"]][i] <- Dat_cat[["cases_cumprop"]][i-1] + Dat_cat[["cases_prop"]][i]
  Dat_cat[["assets_cumprop"]][i] <- Dat_cat[["assets_cumprop"]][i-1] + Dat_cat[["assets_prop"]][i]
  Dat_cat[["term"]][i] <- Dat_cat[["cases_prop"]][i]*(Dat_cat[["assets_cumprop"]][i-1]+Dat_cat[["assets_cumprop"]][i])
  
  }

Gini=abs(1-sum(Dat_cat[["term"]]))

RESULTS <- matrix(0, nrow = 1, ncol = 2)
dimnames(RESULTS) <- list(NULL, c("Class", "Gini"))
RESULTS <- as.data.frame(RESULTS)
RESULTS[1, ] <- c("National", round(Gini, 3))

list(RESULTS)[[1]]

  } else {

  Data.Name[[Class]] <- haven::as_factor(Data.Name[[Class]])
  Data.Name$DomID  <- c(as.numeric(Data.Name[[Class]]))
    
  RESULTS <- matrix(0, nrow = max(as.numeric(Data.Name$DomID)), ncol = 2)
  dimnames(RESULTS) <- list(NULL, c("Class", "Gini"))
  RESULTS <- as.data.frame(RESULTS)
  
  for (j in 1:(max(as.numeric(Data.Name$DomID)))) {
    
    Dat = Data.Name[Data.Name$DomID == j, ]

    Dat <- Dat %>% filter(hv101==1 & hv012>0) %>%
      mutate(cases=hv012*hv005/1000000)   # Each household has hv012 de jure members
    
    Dat$category=1+as.integer(99*(Dat$hv271 - min(Dat$hv271))/(max(Dat$hv271)-min(Dat$hv271)))
    MIN <- min(Dat$hv271)
    
    # IMPORTANT! The assets for the household are weighted by hv005 but not multiplied by the number of household members
    CASES <- Dat %>% group_by(category) %>% summarise(cases = sum(cases, na.rm=TRUE)) 
    
    Dat$assets <- (Dat$hv271-MIN)*Dat$hv005/1000000
    
    ASSETS <- Dat %>% 
      group_by(category) %>% 
      summarise(assets = sum(assets, na.rm=TRUE)) 
    
    Dat_cat <- merge(ASSETS, CASES, by = c("category"), all.x = TRUE, all.y = TRUE)
    
    # Note: some categories may be empty; best to renumber them; must be certain they are in sequence
    Dat_cat <- transform(Dat_cat, category = as.numeric(factor(category)))
    
    Dat_cat$cases_prop= Dat_cat$cases/sum(Dat_cat$cases)
    Dat_cat$assets_prop= Dat_cat$assets/sum(Dat_cat$assets)
    
    # Calculate cumulative proportions for cases and assets. 
    Dat_cat[["cases_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["cases_prop"]], 0)
    Dat_cat[["assets_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["assets_prop"]], 0)
    Dat_cat[["term"]] <- ifelse(Dat_cat[["category"]]==1, 
                                Dat_cat[["cases_prop"]]*Dat_cat[["assets_cumprop"]], 0)
    
    ncats = (2:max(Dat_cat$category))
    for (i in (ncats)) {
      
      Dat_cat[["cases_cumprop"]][i] <- Dat_cat[["cases_cumprop"]][i-1] + Dat_cat[["cases_prop"]][i]
      Dat_cat[["assets_cumprop"]][i] <- Dat_cat[["assets_cumprop"]][i-1] + Dat_cat[["assets_prop"]][i]
      Dat_cat[["term"]][i] <- Dat_cat[["cases_prop"]][i]*(Dat_cat[["assets_cumprop"]][i-1]+Dat_cat[["assets_cumprop"]][i])
      
    }
    
    Gini=abs(1-sum(Dat_cat[["term"]]))
    
    RESULTS[j, ] <- c(attributes(Dat[[Class]])$levels[[j]], round(Gini, 3))
    
  }
  list(RESULTS)[[1]]
    
}
  
}

ph_gini_results <- as.data.frame(rbind(calc_gini(PRdata_gini),
                            calc_gini(PRdata_gini,Class="hv025"),
                            calc_gini(PRdata_gini,Class="hv024")))

write.xlsx(ph_gini_results, "Tables_PH.xlsx", sheetName = "ph_gini_results",append=TRUE)

