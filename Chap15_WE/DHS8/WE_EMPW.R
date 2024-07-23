# /*****************************************************************************************************
# Program: 			WE_EMPW.R
# Purpose: 			Code to compute decision making and justification of violence among in men and women
# Data inputs: 	IR or MR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Nov 19, 2021 by Shireen Assaf 
# Note:				The indicators below can be computed for men and women. 
# 					  The indicators we_decide_all and we_decide_none have different variable labels for men compared to women. 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# we_decide_health			  "Decides on own health care"
# we_decide_hhpurch			  "Decides on large household purchases"
# we_decide_visits			  "Decides on visits to family or relatives"
# we_decide_health_self		"Decides on own health care either alone or jointly with partner"
# we_decide_hhpurch_self	"Decides on large household purchases either alone or jointly with partner"
# we_decide_visits_self		"Decides on visits to family or relatives either alone or jointly with partner"
# we_decide_all				    "Decides on all three: health, purchases, and visits  either alone or jointly with partner" (for women)
# 							          "Decides on both health and purchases either alone or jointly with partner" (for men)
# we_decide_none				  "Does not decide on any of the three decisions either alone or jointly with partner" (for women)
# 							          "Does not decide on health or purchases either alone or jointly with partner" (for men)
# 	
# we_dvjustify_burn			  "Agree that husband is justified in hitting or beating his wife if she burns food"
# we_dvjustify_argue			"Agree that husband is justified in hitting or beating his wife if she argues with him"
# we_dvjustify_goout			"Agree that husband is justified in hitting or beating his wife if she goes out without telling him"
# we_dvjustify_neglect		"Agree that husband is justified in hitting or beating his wife if she neglects the children"
# we_dvjustify_refusesex	"Agree that husband is justified in hitting or beating his wife if she refuses to have sexual intercourse with him"
# we_dvjustify_onereas		"Agree that husband is justified in hitting or beating his wife for at least one of the reasons"
# 	
# we_justify_refusesex		"Believe a woman is justified to refuse sex with her husband if she knows he's having sex with other women"
# we_justify_cond				  "Believe a women is justified in asking that her husband to use a condom if she knows that he has an STI"
# we_havesay_refusesex		"Can say no to their husband if they do not want to have sexual intercourse"
# we_havesay_condom			  "Can ask their husband to use a condom"
# 	
# we_num_decide				    "Number of decisions made either alone or jointly with husband among women currently in a union"
# we_num_justifydv		  	"Number of reasons for which wife beating is justified among women currently in a union"
# ----------------------------------------------------------------------------*/
# 
# * indicators from IR file
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)
 
# *** Decision making ***
# 
# //Decides on own health
IRdata <- IRdata %>%
  mutate(we_decide_health =
           case_when(v502==1  ~ v743a )) %>%
  set_variable_labels(we_decide_health = "Decides on own health care")

# //Decides on household purchases
IRdata <- IRdata %>%
  mutate(we_decide_hhpurch =
           case_when(v502==1  ~ v743b )) %>%
  set_variable_labels(we_decide_hhpurch = "Decides on large household purchases")

# //Decides on visits
IRdata <- IRdata %>%
  mutate(we_decide_visits =
           case_when(v502==1  ~ v743d )) %>%
  set_variable_labels(we_decide_visits = "Decides on visits to family or relatives")

# //Decides on own health either alone or jointly
IRdata <- IRdata %>%
  mutate(we_decide_health_self =
           case_when(
             v502==1 & v743a <3  ~ 1 ,
             v502==1 & v743a>2 ~ 0)) %>%
  set_value_labels(we_decide_health_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_health_self = "Decides on own health care either alone or jointly with partner")

# //Decides on household purchases either alone or jointly
IRdata <- IRdata %>%
  mutate(we_decide_hhpurch_self =
           case_when(
             v502==1 & v743b <3  ~ 1 ,
             v502==1 & v743b>2 ~ 0)) %>%
  set_value_labels(we_decide_hhpurch_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_hhpurch_self = "Decides on large household purchases either alone or jointly with partner")

# //Decides on visits either alone or jointly
IRdata <- IRdata %>%
  mutate(we_decide_visits_self =
           case_when(
             v502==1 & v743d <3  ~ 1 ,
             v502==1 & v743d>2 ~ 0)) %>%
  set_value_labels(we_decide_visits_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_visits_self = "Decides on visits to family or relatives either alone or jointly with partner")

# //Decides on all three: health, purchases, and visits  either alone or jointly with partner
IRdata <- IRdata %>%
  mutate(we_decide_all =
           case_when(
             v502==1 & v743a <3 & v743b <3 & v743d <3  ~ 1 ,
             v502==1 & (v743a >2 | v743b >2 | v743d >2 ) ~ 0)) %>%
  set_value_labels(we_decide_all = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_all = "Decides on all three: health, purchases, and visits  either alone or jointly with partner")

# //Does not decide on any of the three decisions either alone or jointly with partner
IRdata <- IRdata %>%
  mutate(we_decide_none =
           case_when(
             v502==1 & v743a <3 | v743b <3 | v743d <3  ~ 0 ,
             v502==1 & (v743a >2 & v743b >2 & v743d >2 ) ~ 1)) %>%
  set_value_labels(we_decide_none = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_none = "Does not decide on any of the three decisions either alone or jointly with partner")


# *** Justification of violence ***
# 
# //Justify violence - burned food
IRdata <- IRdata %>%
  mutate(we_dvjustify_burn =
           case_when(
             v744e==1  ~ 1, 
             v744e %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_burn = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_burn = "Agree that husband is justified in hitting or beating his wife if she burns food")

# //Justify violence - argues
IRdata <- IRdata %>%
  mutate(we_dvjustify_argue =
           case_when(
             v744c==1  ~ 1, 
             v744c %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_argue = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_argue = "Agree that husband is justified in hitting or beating his wife if she argues with him")

# //Justify violence - goes out without saying
IRdata <- IRdata %>%
  mutate(we_dvjustify_goout =
           case_when(
             v744a==1  ~ 1, 
             v744a %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_goout = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_goout = "Agree that husband is justified in hitting or beating his wife if she goes out without telling him")

# //Justify violence - neglects children 
IRdata <- IRdata %>%
  mutate(we_dvjustify_neglect =
           case_when(
             v744b==1  ~ 1, 
             v744b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_neglect = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_neglect = "Agree that husband is justified in hitting or beating his wife if she neglects the children")

# //Justify violence - no sex
IRdata <- IRdata %>%
  mutate(we_dvjustify_refusesex =
           case_when(
             v744d==1  ~ 1, 
             v744d %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_refusesex = "Agree that husband is justified in hitting or beating his wife if she refuses to have sexual intercourse with him")

# //Justify violence - at least one reason
IRdata <- IRdata %>%
  mutate(we_dvjustify_onereas =
           ifelse(v744a==1 | v744b==1 | v744c==1 | v744d==1 | v744e==1 , 1,0)) %>%
  set_value_labels(we_dvjustify_onereas = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_onereas = "Agree that husband is justified in hitting or beating his wife for at least one of the reasons")

# //Justify to reuse sex - he's having sex with another woman
IRdata <- IRdata %>%
  mutate(we_justify_refusesex =
           case_when(
             v633b==1  ~ 1, 
             v633b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_justify_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_justify_refusesex = "Believe a woman is justified to refuse sex with her husband if she knows he's having sex with other women")

# //Justify to ask to use condom - he has STI
IRdata <- IRdata %>%
  mutate(we_justify_cond =
           case_when(
             v822==1  ~ 1, 
             v822 %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_justify_cond = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_justify_cond = "Believe a women is justified in asking that her husband to use a condom if she knows that he has an STI")

# //Can refuse sex with husband
IRdata <- IRdata %>%
  mutate(we_havesay_refusesex =
           case_when(
             v502==1 & v850a==1  ~ 1, 
             v502==1 & v850a %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_havesay_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_havesay_refusesex = "Can say no to their husband if they do not want to have sexual intercourse")

# //Can ask husband to use condom
IRdata <- IRdata %>%
  mutate(we_havesay_condom =
           case_when(
             v502==1 & v850b==1  ~ 1, 
             v502==1 & v850b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_havesay_condom = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_havesay_condom = "Can ask their husband to use a condom")

# //Number of decisions wife makes alone or jointly
IRdata <- IRdata %>%
  mutate(v743a2 = ifelse(v743a<3,1,0))  %>%
  mutate(v743b2 = ifelse(v743b<3,1,0))  %>%
  mutate(v743d2 = ifelse(v743d<3,1,0))  %>%
  mutate(decisions= v743a2+v743b2+v743d2) %>%
  mutate(we_num_decide=
          case_when(
             v502==1 & decisions==0  ~ 1, 
             v502==1 & decisions%in% c(1,2)  ~ 2, 
             v502==1 & decisions==3  ~ 3)) %>%
  set_value_labels(we_num_decide = c("3"=3, "1-2" = 2, "0"=1  )) %>%
  set_variable_labels(we_num_decide = "Number of decisions made either alone or jointly with husband among women currently in a union")

#remove temporary vars
IRdata =subset(IRdata, select=-c(decisions,v743a2,v743b2,v743d2))

# //Number of reasons wife beating is justified
IRdata <- IRdata %>%
  mutate(v744a2 = ifelse(v744a==1,1,0))  %>%
  mutate(v744b2 = ifelse(v744b==1,1,0))  %>%
  mutate(v744c2 = ifelse(v744c==1,1,0))  %>%
  mutate(v744d2 = ifelse(v744d==1,1,0))  %>%
  mutate(v744e2 = ifelse(v744e==1,1,0))  %>%
  mutate(reasons= v744a2+v744b2+v744c2+v744d2+v744e2) %>%
  mutate(we_num_justifydv=
           case_when(
             v502==1 & reasons==0  ~ 0, 
             v502==1 & reasons%in% c(1,2)  ~ 1, 
             v502==1 & reasons%in% c(3,4)  ~ 2,
             v502==1 & reasons==5  ~ 3)) %>%
  set_value_labels(we_num_justifydv = c("5"=3, "3-4" = 2, "1-2"=1, "0"=0  )) %>%
  set_variable_labels(we_num_justifydv = "Number of reasons for which wife beating is justified among women currently in a union")

#remove temporary vars
IRdata =subset(IRdata, select=-c(reasons,v744a2,v744b2,v744c2,v744d2,v744e2))

# * indicators from MR file

# 
# *** Decision making ***

# //Decides on own health
MRdata <- MRdata %>%
  mutate(we_decide_health =
           case_when(mv502==1  ~ mv743a )) %>%
  set_variable_labels(we_decide_health = "Decides on own health care")

# //Decides on household purchases
MRdata <- MRdata %>%
  mutate(we_decide_hhpurch =
           case_when(mv502==1  ~ mv743b )) %>%
  set_variable_labels(we_decide_hhpurch = "Decides on large household purchases")

# //Decides on visits
MRdata <- MRdata %>%
  mutate(we_decide_visits =
           case_when(mv502==1  ~ mv743d )) %>%
  set_variable_labels(we_decide_visits = "Decides on visits to family or relatives")

# //Decides on own health either alone or jointly
MRdata <- MRdata %>%
  mutate(we_decide_health_self =
           case_when(
             mv502==1 & mv743a <3  ~ 1 ,
             mv502==1 & mv743a>2 ~ 0)) %>%
  set_value_labels(we_decide_health_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_health_self = "Decides on own health care either alone or jointly with partner")

# //Decides on household purchases either alone or jointly
MRdata <- MRdata %>%
  mutate(we_decide_hhpurch_self =
           case_when(
             mv502==1 & mv743b <3  ~ 1 ,
             mv502==1 & mv743b>2 ~ 0)) %>%
  set_value_labels(we_decide_hhpurch_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_hhpurch_self = "Decides on large household purchases either alone or jointly with partner")

# //Decides on visits either alone or jointly
MRdata <- MRdata %>%
  mutate(we_decide_visits_self =
           case_when(
            mv502==1 & mv743d <3  ~ 1 ,
            mv502==1 & mv743d>2 ~ 0)) %>%
  set_value_labels(we_decide_visits_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_visits_self = "Decides on visits to family or relatives either alone or jointly with partner")

# //Decides on both health and purchases either alone or jointly with partner
MRdata <- MRdata %>%
  mutate(we_decide_all =
           case_when(
             mv502==1 & mv743a <3 & mv743b <3  ~ 1 ,
             mv502==1 & (mv743a >2 | mv743b >2) ~ 0)) %>%
  set_value_labels(we_decide_all = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_all = "Decides on all three: health, purchases, and visits  either alone or jointly with partner")

# //Does not decide on health or purchases either alone or jointly with partner
MRdata <- MRdata %>%
  mutate(we_decide_none =
           case_when(
             mv502==1 & mv743a <3 | mv743b <3  ~ 0 ,
             mv502==1 & (mv743a >2 & mv743b >2 ) ~ 1)) %>%
  set_value_labels(we_decide_none = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_none = "Does not decide on any of the three decisions either alone or jointly with partner")


# *** Justification of violence ***

# //Justify violence - burned food
MRdata <- MRdata %>%
  mutate(we_dvjustify_burn =
           case_when(
             mv744e==1  ~ 1, 
             mv744e %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_burn = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_burn = "Agree that husband is justified in hitting or beating his wife if she burns food")

# //Justify violence - argues
MRdata <- MRdata %>%
  mutate(we_dvjustify_argue =
           case_when(
             mv744c==1  ~ 1, 
             mv744c %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_argue = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_argue = "Agree that husband is justified in hitting or beating his wife if she argues with him")

# //Justify violence - goes out without saying
MRdata <- MRdata %>%
  mutate(we_dvjustify_goout =
           case_when(
             mv744a==1  ~ 1, 
             mv744a %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_goout = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_goout = "Agree that husband is justified in hitting or beating his wife if she goes out without telling him")

# //Justify violence - neglects children 
MRdata <- MRdata %>%
  mutate(we_dvjustify_neglect =
           case_when(
             mv744b==1  ~ 1, 
             mv744b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_neglect = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_neglect = "Agree that husband is justified in hitting or beating his wife if she neglects the children")

# //Justify violence - no sex
MRdata <- MRdata %>%
  mutate(we_dvjustify_refusesex =
           case_when(
             mv744d==1  ~ 1, 
             mv744d %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_refusesex = "Agree that husband is justified in hitting or beating his wife if she refuses to have sexual intercourse with him")

# //Justify violence - at least one reason
MRdata <- MRdata %>%
  mutate(we_dvjustify_onereas =
           ifelse(mv744a==1 | mv744b==1 | mv744c==1 | mv744d==1 | mv744e==1 , 1,0)) %>%
  set_value_labels(we_dvjustify_onereas = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_onereas = "Agree that husband is justified in hitting or beating his wife for at least one of the reasons")

# //Justify to reuse sex - he's having sex with another woman
MRdata <- MRdata %>%
  mutate(we_justify_refusesex =
           case_when(
             mv633b==1  ~ 1, 
             mv633b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_justify_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_justify_refusesex = "Believe a woman is justified to refuse sex with her husband if she knows he's having sex with other women")

# //Justify to ask to use condom - he has STI
MRdata <- MRdata %>%
  mutate(we_justify_cond =
           case_when(
             mv822==1  ~ 1, 
             mv822 %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_justify_cond = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_justify_cond = "Believe a women is justified in asking that her husband to use a condom if she knows that he has an STI")
