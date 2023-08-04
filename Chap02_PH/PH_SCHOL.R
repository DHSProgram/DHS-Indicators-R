# /*****************************************************************************************************
# Program: 			PH_SCHOL.R
# Purpose: 			Code to compute school attendance indicators
# Data inputs: 		BR and PR dataset
# Data outputs:		coded variables
# Author:				Trevor Croft, translated to R by Mahmoud Elkasabi 
# Date last modified: August 4, 2023 by Courtney Allen
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ph_sch_nar_prim			"Primary school net attendance ratio (NAR)" & "Gender parity index for NAR primary"
# ph_sch_nar_sec			"Secondary school net attendance ratio (NAR)"  & "Gender parity index for NAR secondary"
# ph_sch_gar_prim			"Primary school gross attendance ratio (GAR)" & "Gender parity index for GAR primary"
# ph_sch_gar_sec			"Secondary school gross attendance ratio (GAR)" & "Gender parity index for GAR secondary"
# ----------------------------------------------------------------------------*/

# For net attendance rates (NAR) and gross attendance rates (GAR) we need to know the age of children at the start of the school year.
# For this we need to get date of birth from birth history and attach to children's records in the PR file.
# open the birth history data to extract date of birth variables needed.
temp <- BRdata

# keep only the variables we need
temp <- (BRdata[, c("v001", "v002","v003", "b3", "b16")])

# drop if the child in the birth history was not in the household or not alive 
temp <- temp %>%
  filter(b16!=0 & !is.na(b16))

# rename key variables for matching 
temp[["hvidx"]] <- temp[["b16"]]  
temp[["hv001"]] <- temp[["v001"]]
temp[["hv002"]] <- temp[["v002"]]
temp <- (temp[, c("hvidx", "hv001", "hv002", "b3", "b16")])

# sort on key variables 
temp <- temp[order(temp$hv001, temp$hv002, temp$hvidx),]

# if there are some duplicates of line number in household questionnaire, we need to drop the duplicates
temp <- temp[!duplicated(temp[,c("hvidx", "hv001", "hv002")]),]

# use the PR file for household members for the NAR and GAR indicators
BR_PMRdata <- merge(PRdata, temp, by = c("hv001", "hv002", "hvidx"), all.y = FALSE, all.x = TRUE)

# restrict to de facto household members age 5-24, and drop all others
BR_PMRdata <- BR_PMRdata %>%
  filter(hv103==1 & inrange(hv105,5,24))

# produce century month code of start of school year for each state and phase
cmcSch = (school_start_yr - 1900)*12 + school_start_mo

# calculate the age at the start of the school year, using the date of birth from the birth history if we have it
# Impute an age at the beginning of the school year when CMC of birth is unknown
# the random imputation below means that we won't get a perfect match with the report, but it will be close
BR_PMRdata[["xtemp"]] <- ifelse(is.na(BR_PMRdata[["b3"]]), BR_PMRdata[["hv008"]] - (BR_PMRdata[["hv105"]]*12), 0)
BR_PMRdata[["cmctemp"]] <- ifelse(is.na(BR_PMRdata[["b3"]]), BR_PMRdata[["xtemp"]] - as.integer(runif(1, 0, 1)*12), 0)
BR_PMRdata[["school_age"]] <- ifelse(is.na(BR_PMRdata[["b3"]]), 
                                     as.integer((cmcSch - BR_PMRdata[["cmctemp"]])/12), 
                                     as.integer((cmcSch - BR_PMRdata[["b3"]])/12))

# Generate variables for whether the child is in the age group for primary or secondary school
BR_PMRdata[["prim_age"]] <- ifelse(inrange(BR_PMRdata[["school_age"]],age_prim_min,age_prim_max), 1, 0)
BR_PMRdata[["sec_age"]] <- ifelse(inrange(BR_PMRdata[["school_age"]],age_sec_min,age_sec_max),1, 0)

# create the school attendance variables, not restricted by age
BR_PMRdata[["prim"]] <- ifelse(BR_PMRdata[["hv122"]]==1, 1, 0)
BR_PMRdata[["sec"]] <- ifelse(BR_PMRdata[["hv122"]]==2, 1, 0)

# set sample weight
BR_PMRdata[["wt"]] = BR_PMRdata[["hv005"]]/1000000
 
# For NAR we can use this as just regular variables and can tabulate as follows, but can't do this for GAR as the numerator is not a subset of the denominator
# NAR is just the proportion attending primary/secondary school of children in the correct age range, for de facto children 
BR_PMRdata[["nar_prim"]] <- ifelse(BR_PMRdata[["prim_age"]]==1, BR_PMRdata[["prim"]], 0)
BR_PMRdata[["nar_sec"]] <- ifelse(BR_PMRdata[["sec_age"]]==1, BR_PMRdata[["sec"]], 0)

BR_PMRdata <- BR_PMRdata %>%
  mutate(nar_prim = set_label(nar_prim, label = "Primary school net attendance ratio (NAR)"),
         nar_sec = set_label(nar_sec, label = "Secondary school net attendance ratio (NAR)"))


# Program for calculating NAR or GAR
# NAR just uses a mean of one variable
# GAR uses a ratio of two variables
# 
# Program to produce NAR or GAR for background characteristics (including total) for both sex, combined and separately

nar_gar <- function(Rate,Sch,Sch_age){
  
  BR_PMRdata$sch_age <- BR_PMRdata[[Sch_age]]
  wkdata <- BR_PMRdata %>% filter(sch_age==1)
  wkdata$sch <- wkdata[[Sch]]

  if (Rate=="nar"){
    
    # Total
    # NAR primary   - total population
     wkdata0 <- wkdata %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE)) %>% 
       mutate(class="National")%>% mutate(levels=0)%>% select(c(Total,class,levels))
    # NAR primary   - urban/rural
     wkdata1 <- wkdata %>% group_by(hv025) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE)) %>% 
       mutate(class="urban/rural")%>% mutate(levels=hv025)%>% select(c(Total,class,levels))
    # NAR primary   - region
     wkdata2 <- wkdata %>% group_by(hv024) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE))%>% 
       mutate(class="region")%>% mutate(levels=hv024)%>% select(c(Total,class,levels))
    # NAR primary   - wealth index
     wkdata3 <- wkdata %>% group_by(hv270) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE))%>% 
       mutate(class="wealth")%>% mutate(levels=hv270)%>% select(c(Total,class,levels))

    resc_all <- (rbind(wkdata0,wkdata1,wkdata2,wkdata3))
    
    # Male/Female  
    # NAR primary   - total population
     wkdata0 <- wkdata %>% group_by(hv104) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE))%>% 
       mutate(class="National")%>% mutate(levels=0)%>% select(c(hv104,Total,class,levels))
    # NAR primary   - urban/rural
     wkdata1 <- wkdata %>% group_by(hv104,hv025) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE))%>% 
       mutate(class="urban/rural")%>% mutate(levels=hv025)%>% select(c(hv104,Total,class,levels))
    # NAR primary   - region
     wkdata2 <- wkdata %>% group_by(hv104,hv024) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE))%>% 
       mutate(class="region")%>% mutate(levels=hv024)%>% select(c(hv104,Total,class,levels))
    # NAR primary   - wealth index
     wkdata3 <- wkdata %>% group_by(hv104,hv270) %>% summarise(Total = weighted.mean(sch, wt, na.rm=TRUE))%>% 
       mutate(class="wealth")%>% mutate(levels=hv270)%>% select(c(hv104,Total,class,levels))

    resc <- (rbind(wkdata0,wkdata1,wkdata2,wkdata3))

    return(NAR) 
  }  
  else {
    
    dstrata <- wkdata %>%
      as_survey_design(cluster= hv021, strata = hv023, weights = wt)
    
    wkdata0 <- dstrata %>% summarise(Total = survey_ratio(sch,sch_age)) %>%
      mutate(class="National")%>% mutate(levels=0) %>% select(c(Total,class,levels))
    wkdata1 <- dstrata %>% group_by(hv025) %>% summarise(Total = survey_ratio(sch,sch_age))%>% 
      mutate(class="Urban/rural")%>% mutate(levels=hv025) %>% select(c(Total,class,levels))
    wkdata2 <- dstrata %>% group_by(hv024) %>% summarise(Total = survey_ratio(sch,sch_age))%>%
      mutate(class="Region")%>% mutate(levels=hv024)%>% select(c(Total,class,levels))
    wkdata3 <- dstrata %>% group_by(hv270) %>% summarise(Total = survey_ratio(sch,sch_age))%>% 
      mutate(class="Wealth")%>% mutate(levels=hv270) %>% select(c(Total,class,levels))
    
    resc_all <- (rbind(wkdata0,wkdata1,wkdata2,wkdata3))
    resc_all <- resc_all %>% mutate(Total=Total*100)
    
    wkdata0 <- dstrata %>% group_by(hv104) %>% summarise(Total = survey_ratio(sch,sch_age)) %>%
      mutate(class="National")%>% mutate(levels=0) %>% select(c(hv104,Total,class,levels))
    wkdata1 <- dstrata %>% group_by(hv104,hv025) %>% summarise(Total = survey_ratio(sch,sch_age))%>% 
      mutate(class="Urban/rural")%>% mutate(levels=hv025) %>% select(c(hv104,Total,class,levels))
    wkdata2 <- dstrata %>% group_by(hv104,hv024) %>% summarise(Total = survey_ratio(sch,sch_age))%>%
      mutate(class="Region")%>% mutate(levels=hv024)%>% select(c(hv104,Total,class,levels))
    wkdata3 <- dstrata %>% group_by(hv104,hv270) %>% summarise(Total = survey_ratio(sch,sch_age))%>% 
      mutate(class="Wealth")%>% mutate(levels=hv270) %>% select(c(hv104,Total,class,levels))
    
    resc <- (rbind(wkdata0,wkdata1,wkdata2,wkdata3))
    resc <- resc %>% mutate(Total=Total*100)
    
    resc_m <- resc %>% filter(hv104==1) %>% rename(Males=Total)  
    resc_f <- resc %>% filter(hv104==2) %>% rename(Females=Total)  
    
    GAR <- merge(resc_m, resc_f, by = c("class", "levels"), all.y = TRUE, all.x = TRUE)
    GAR <- merge(GAR, resc_all, by = c("class", "levels"), all.y = TRUE, all.x = TRUE)
    GAR <- GAR %>% mutate(GAR_GPI=Females/Males) 
    
    return(GAR) 
  }
  
}	

ph_sch_nar_prim <- nar_gar(Rate="NAR",Sch="prim",Sch_age="prim_age")
ph_sch_nar_sec <- nar_gar(Rate="NAR",Sch="sec",Sch_age="sec_age")
ph_sch_gar_prim <- nar_gar(Rate="GAR",Sch="prim",Sch_age="prim_age")
ph_sch_gar_sec <- nar_gar(Rate="GAR",Sch="sec",Sch_age="sec_age")

