# *****************************************************************************************************
# Program: 			  FP_Need.R
# Purpose: 			  Code contraceptive unmet need, met need, demand satisfied, intention to use
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Trevor Croft
# Date last modified: Mar 17 2021 by Trevor Croft
# *****************************************************************************************************
  
# ----------------------------------------------------------------------------
# Variables created in this file:
# fp_unmet_space  "Unmet need for spacing"
# fp_unmet_limit  "Unmet need for limiting"
# fp_unmet_tot    "Unmet need total"
# fp_met_space    "Met need for spacing"
# fp_met_limit    "Met need for limiting"
# fp_met_tot      "Met need total"
# fp_demand_space "Total demand for spacing"
# fp_demand_limit "Total demand for limiting"
# fp_demand_tot   "Total demand -total"
# fp_demsat_mod   "Demand satisfied by modern methods"
# fp_demsat_any   "Demand satisfied by any method"
# fp_future_use   "Intention of use of contraception in the future among non-users"
# -----------------------------------------------------------------------------

# libraries needed
library(tidyverse)
library(haven)
library(labelled)
library(sjlabelled)
library(sjmisc)
#library(janitor)
#library(survey)  #to create survey design weights

datapath <- "C:/Users/21180/OneDrive - ICF/Data/DHS_Stata"
irdata <- "UGIR7BFL"
srvy <- substr(irdata,1,6)

# read data for now (this will happen in main file, just using code here for temp)
IRdata <- read_dta(paste(datapath,"/",irdata,".dta",sep=""))

v626a_included <- (hasName(IRdata,"v626a") & sum(is.na(IRdata$v626a)) == 0)

# temporarily rename v626a
# IRdata <- IRdata %>%
#   rename(v626a_orig = v626a)

if (!v626a_included) {

# test for existence of variables and add the ones we refer to, to avoid errors because vars do not exist - will be deleted later
vars_to_check <- c("v225", "v302", "v302a", "v375a", "v376", "v3a08d", "v512", "m6_1", "m10_1", "s309b", "s313", "s607c", "s607d")
vars_to_add <- vars_to_check[hasName(IRdata, vars_to_check) == FALSE]
IRdata[,vars_to_add] <- NA

# construct all of the steps to produce the unmet need variable v626a
IRdata <- IRdata %>%
  mutate(
    v626a = 
    ## CONTRACEPTIVE USERS - GROUP 1
    # using to limit if wants no more, sterilized, or declared infecund
    ifelse(v312!=0 & (!is.na(v605) & v605>=5 & v605<=7), 4, 
    # using to space - all other contraceptive users
    ifelse(v312!=0, 3, NA)),
    
    ##PREGNANT or POSTPARTUM AMENORRHEIC (PPA) WOMEN - GROUP 2
    # Determine who should be in Group 2

    # generate time since last birth
    tsinceb = v222,

    # generate time since last period in months from v215
    tsincep = ifelse(v215>=100 & v215<=190, trunc((v215-100)/30),
              ifelse(v215>=200 & v215<=290, trunc((v215-200)/4.3),
              ifelse(v215>=300 & v215<=390, (v215-300),
              ifelse(v215>=400 & v215<=490, (v215-400)*12, NA
              )))),

    # initialize pregnant or postpartum amenorrheic (PPA) women
    pregPPA = (((!is.na(v213) & v213==1) | (!is.na(m6_1) & m6_1==96))
    # For women with missing data or "period not returned" on date of last menstrual period, use information from time since last period
    #	if last period is before last birth in last 5 years
             | ((is.na(m6_1) | m6_1==99 | m6_1==97) & !is.na(tsincep) & !is.na(tsinceb) & tsincep > tsinceb & tsinceb<60)
    #	or if said "before last birth" to time since last period in the last 5 years
             | ((is.na(m6_1) | m6_1==99 | m6_1==97) & !is.na(v225) & v215==995 & !is.na(tsinceb) & tsinceb<60)),
    # select only women who are pregnant or PPA for <24 months
    pregPPA24 = ((!is.na(v213) & v213==1) | (pregPPA & !is.na(tsinceb) & tsinceb<24)),
  
    # Classify based on wantedness of current pregnancy/last birth
    wantedlast = ifelse((is.na(v225) | v225==9) & (is.na(v213) | v213!=1), m10_1, v225),
    # recode 'God's will' (survey-specific response) as not in need for Niger 1992
    wantedlast = ifelse(!is.na(wantedlast) & wantedlast==4 & v000=="NI2", 1, wantedlast), 
    # recode 'not sure' and 'don't know' (survey-specific responses) as unmet need for spacing for Cote D'Ivoire 1994 and Madagascar 1992
    wantedlast = ifelse(!is.na(wantedlast) & (wantedlast==4 | wantedlast==8), 2, wantedlast), 

    v626a = ifelse(is.na(v626a) & pregPPA24,
    # missing=missing
      ifelse(is.na(wantedlast) | wantedlast==9, 99,
    # no unmet need if wanted current pregnancy/last birth then/at that time
      ifelse(wantedlast==1, 7,
    # unmet need for spacing if wanted current pregnancy/last birth later
      ifelse(wantedlast==2, 1, 
    # unmet need for limiting if wanted current pregnancy/last birth not at all
      ifelse(wantedlast==3, 2,
    # close of the 'ifelse' statements
      NA )))), 
    # if v626a already has a value or not postpartum amenorrheic in last 24 months, then keep the value of v626a
      v626a),

    ##DETERMINE FECUNDITY - GROUP 3 (Boxes refer to Figure 2 flowchart in report)
    infec = 
      ##Box 1 - applicable only to currently married
              # married 5+ years ago, no children in past 5 years, 
      ifelse((!is.na(v502) & v502==1 & !is.na(v512) & v512>=5 & (is.na(tsinceb) | tsinceb>59)) & (
                # never used contraception, excluding pregnant and PPA <24 months
                (!is.na(v302 ) & v302 ==0 & !pregPPA24)
                # in DHS VI, v302 replaced by v302a
              | (!is.na(v302a) & v302a==0 & !pregPPA24 & substr(v000,3,3) %in% c("6","7","8"))
                # survey-specific code for Cambodia 2010
              | (!is.na(s313)  & s313 ==0 & !pregPPA24 & v000=="KH5" & (v007==2010 | v007==2011))
                # survey-specific code for Tanzania 2010
              | (!is.na(s309b) & s309b==0 & !pregPPA24 & v000=="TZ5" & (v007==2009 | v007==2010))
                ), 1, # end of Box 1
        
      ##Box 2
            # declared infecund on future desires for children
      ifelse(!is.na(v605) & v605==7, 2, # end of Box 2
             
      ##Box 3
            # menopausal/hysterectomy on reason not using contraception - slightly different recoding in DHS III and IV+
            # DHS IV+ surveys
      ifelse((!is.na(v3a08d) & v3a08d==1 & (substr(v000,3,3) %in% c("4", "5", "6", "7", "8")))
            # DHSIII surveys
          | (!is.na(v375a) & v375a==23 & (substr(v000,3,3) %in% c("3", "T")))
            # special code for hysterectomy for Brazil 1996, Guatemala 1995 and 1998-9  (code 23 = menopausal only)
          | (!is.na(v375a) & v375a==28 & (v000 %in% c("BR3", "GU3")))
            # reason not using did not exist in DHSII, use reason not intending to use in future
          | (!is.na(v376 ) & v376==14 & substr(v000,3,3)=="2")
            # below set of codes are all survey-specific replacements for reason not using contraception.
	        | ((v000=="CI3" & v007==94 & !is.na(v376) & v376==23)   # survey-specific code for Cote D'Ivoire 1994
 	        |  (v000=="GA3" & !is.na(s607d) & s607d==1)             # survey-specific code for Gabon 2000
	        |  (v000=="HT3" & !is.na(v376) & v376==23)              # survey-specific code for Haiti 1994/95
	        |  (v000=="JO4" & !is.na(v376) & (v376==23 | v376==24)) # survey-specific code for Jordan 2002
	        |  (v000=="KK3" & v007==99 & !is.na(s607d) & s607d==1)  # survey-specific code for Kazakhstan 1999
	        |  (v000=="MV5" & !is.na(v376) & v376==23)              # survey-specific code for Maldives 2009
	        |  (v000=="MR3" & !is.na(s607c) & s607c==1)             # survey-specific code for Mauritania 2000
	        |  (v000=="TZ3" & v007==99 & !is.na(s607d) & s607d==1)  # survey-specific code for Tanzania 1999
	        |  (v000=="TR4" & !is.na(v375a) & v375a==23)            # survey-specific code for Turkey 2003
	           ), 3, # end of Box 3
            
	    ##Box 4
	          # Time since last period is >=6 months and not PPA
	    ifelse(!is.na(tsincep) & tsincep>=6 & !pregPPA, 4, # end of Box 4
	           
	    ##Box 5
	          # menopausal/hysterectomy on time since last period
	    ifelse((!is.na(v215) & v215==994)
	          # hysterectomy has different code for some surveys, but in 3 surveys it means "currently pregnant" - Yemen 1991, Turkey 1998, Uganda 1995)
	         | (!is.na(v215) & v215==993 & v000!="TR3" & v000!="UG3" & v000!="YE2")
	          # never menstruated on time since last period, unless had a birth in the last 5 years
	         | (!is.na(v215) & v215==996 & (is.na(tsinceb) | tsinceb>59)), 5, # end of Box 5
	    
	    ##Box 6
	          # time since last birth>= 60 months and last period was before last birth
	    ifelse((!is.na(v215) & v215==995 & !is.na(tsinceb) & tsinceb>=60)
      	    # Never had a birth, but last period reported as before last birth - assume code should have been 994 or 996
	         | (!is.na(v215) & v215==995 & is.na(tsinceb))
	          # exclude pregnant and PP amenorrheic < 24 months
	         | pregPPA24, 6, # end of Box 6
    
      # close of all of the else conditions for the 6 boxes while creating 'infec'
            0 )))))),

    v605 = ifelse(v000=="LS5" & is.na(v605), 4, v605), # survey-specific code for Lesotho 2009, used below
    
    ##NO NEED FOR UNMARRIED WOMEN WHO ARE NOT SEXUALLY ACTIVE
    # determine if sexually active in last 30 days
    # older surveys used code 95 for sex in the last 4 weeks (e.g. Tanzania 1996)
    sexact = (!is.na(v528) & ((v528>=0 & v528<=30) | v528==95)),

    v626a = ifelse(is.na(v626a), # if not already categorized

            # infecund are set to 9 
      ifelse(infec>0, 9,
            # if unmarried and never had sex, assume no need
      ifelse((is.na(v502) | v502!=1) & !is.na(v525) & v525==0, 0, 
            # if unmarried and not sexually active in last 30 days, assume no need
      ifelse((is.na(v502) | v502!=1) & !sexact, 8, 
                    
    ##FECUND WOMEN - GROUP 4
            # wants within 2 years (code 7)
      ifelse(!is.na(v605) & (v605==1
             # survey-specific code: treat 'up to god' as not in need for India (different codes for 1992-3 and 1998-9)
             | (v605==9 & v000=="IA3")
             | (v605==6 & v000=="IA2") 
             ), 7, # not in need
            # wants in 2+ years, wants undecided timing, or unsure if wants (code 1)
      ifelse(!is.na(v605) & v605>=2 & v605<=4, 1, # spacing
            # wants no more (code 2), and missing (code 99) otherwise
      ifelse(!is.na(v605) & v605==5, 2, # limiting
            # the rest 
             99)))))), v626a),

    ##Set unmet need to 98 for unmarried women if survey only included ever-married women or only collected necessary data for married women
    # includes DHS II survey (v605 only asked to married women),
    # Morocco 2003-04, Turkey 1998 (no sexual activity data for unmarried women),
    # Cote D'Ivoire 1994, Haiti 1994-95 (v605 only asked to married women)
	  # India 2005-06 (v605 only asked to ever-married women), Nepal 2006 (v605 not asked to unmarried and "married, guana not performed" women)
	  v626a = ifelse((is.na(v502) | v502!=1) & (v020==1 | substr(v000,3,3)=="2"
	                | v000=="MA4" | v000=="TR2" | (v000=="CI3" & v007==94) | v000=="HT3" | v000=="IA5" | v000=="NP5"), 
	                  98, v626a),
    
    ##Turkey 2003 -  section 6 only used if cluster even, household number even or cluster odd, household number odd
    v626a = ifelse(v000=="TR4" & (v001%%2 != v002%%2), NA, v626a)

   ) # end of the mutate

  # label the unmet need variable and values
  var_label(IRdata$v626a) <- "Unmet need (revised definition)"
  IRdata <- set_labels(IRdata, v626a, labels = c("Never had sex" = 0, 
                       "Unmet need to space" = 1, "Unmet need to limit" = 2,
                       "Met need to space" = 3, "Met need to limit" = 4, 
                       "Not in need" = 7, "No sex in last month" = 8, "Infecund" = 9, 
                       "Unmarried - EM sample or no data" = 98, "Missing" = 99))

  # remove the list of vars added earlier that aren't needed
  IRdata <- IRdata[ , !(names(IRdata) %in% vars_to_add)]

  table(IRdata$v626a, useNA="ifany")

} # end of v626a not included

frq(IRdata$v626a)

IRdata <- IRdata %>%
  mutate(
    fp_unmet_space  = ifelse(v626a==1, 1, 0),
    fp_unmet_limit  = ifelse(v626a==2, 1, 0),
    fp_unmet_tot    = ifelse(v626a==1|v626a==2, 1, 0),
    fp_met_space    = ifelse(v626a==3, 1, 0),
    fp_met_limit    = ifelse(v626a==4, 1, 0),
    fp_met_tot      = ifelse(v626a==3|v626a==4, 1, 0),
    fp_demand_space = ifelse(v626a==1|v626a==3, 1, 0),
    fp_demand_limit = ifelse(v626a==2|v626a==4, 1, 0),
    fp_demand_tot   = ifelse(fp_unmet_tot|fp_met_tot, 1, 0),
    fp_demsat_mod   = ifelse(fp_demand_tot, ifelse(fp_met_tot & v313==3, 1, 0), NA),
    fp_demsat_any   = ifelse(fp_demand_tot, ifelse(fp_met_tot, 1, 0), NA),
  )

# label the variables
var_label(IRdata$fp_unmet_space ) <- "Unmet need for spacing"
var_label(IRdata$fp_unmet_limit ) <- "Unmet need for limiting"
var_label(IRdata$fp_unmet_tot   ) <- "Unmet need fortotal"
var_label(IRdata$fp_met_space   ) <- "Met need for spacing"
var_label(IRdata$fp_met_limit   ) <- "Met need for limiting"
var_label(IRdata$fp_met_tot     ) <- "Met need total"
var_label(IRdata$fp_demand_space) <- "Total demand for spacing"
var_label(IRdata$fp_demand_limit) <- "Total demand for limiting"
var_label(IRdata$fp_demand_tot  ) <- "Total demand - total"
var_label(IRdata$fp_demsat_mod  ) <- "Demand satisfied by modern method"
var_label(IRdata$fp_demsat_any  ) <- "Demand satisfied by any method"

# label the values
yesno <- c("Yes" = 1, "No" = 0)
IRdata <- set_labels(IRdata, fp_unmet_space, fp_unmet_limit, fp_unmet_tot, 
                     fp_met_space, fp_met_limit, fp_met_tot, 
                     fp_demand_space, fp_demand_limit, fp_demand_tot, 
                     fp_demsat_mod, fp_demsat_any, labels = yesno)

# end of unmet need variables


# Future intention to use
IRdata <- IRdata %>%
  mutate(
    fp_future_use = ifelse(v502==1 & v312==0, v362, NA))
var_label(IRdata$fp_future_use) <- "Intention of use of contraception in the future among non-users"
IRdata <- set_labels(IRdata, fp_future_use, 
                     labels = c("use in next 12 months" = 1, "use later" = 2, "unsure about timing" = 3, "unsure about use" = 4, "does not intend to use" = 5, "never had sex" = 6))
