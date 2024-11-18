# /*****************************************************************************************************
# Program: 			  FG_GIRLS.R
# Purpose: 			  Code to compute female circumcision indicators among girls 0-14
# Data inputs: 		BR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: September 20, 2022 by Shireen Assaf 
# Note:				This code only uses the BR file. Older surveys may not information about the daughter's circumcision in the BR file. 
# 					  The information may instead be in the IR file. In that case please use the FG_GIRLS_merge.R file. 
#             This code also produces the tables for circumcision indicators among girls age 0-14
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:

# fg_fcircum_gl	"Circumcised among girls age 0-14"	
# fg_age_gl		"Age at circumcision among girls age 0-14"
# fg_who_gl		"Person who performed the circumcision among girls age 0-14"
# fg_sewn_gl		"Female circumcision type is sewn closed among girls age 0-14"
# ----------------------------------------------------------------------------*/
# 
# *select for girls age 0-14 and drop where the mother was not asked if she ever heard of circumcision
BRgirls <- BRdata %>%
  subset(b4==2  & b5==1 & b8<=14)

BRgirls <- BRgirls[!(is.na(BRgirls$g100)),]

# weight variable 
BRgirls <- BRgirls %>%
  mutate(wt = v005/1000000)
# 
# //Circumcised girls 0-14
BRgirls <- BRgirls %>%
  mutate(fg_fcircum_gl = 
           case_when(
             g121==1 ~ 1 ,
             TRUE ~ 0)) %>%
  set_value_labels(fg_fcircum_gl = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fg_fcircum_gl = "Circumcised among girls age 0-14")
 
# //Age circumcision among girls 0-14
BRgirls <- BRgirls %>%
  mutate(fg_age_gl = 
           case_when(
             g121==0 ~ 0,
             g122==0 ~ 1,
             inrange(g122,1,4)  ~ 2,
             inrange(g122,5,9)  ~ 3 ,
             inrange(g122,10,14) ~ 4 ,
             g122==98 | g122==99~ 9, 
             TRUE ~ 0)) %>%
  set_value_labels(fg_age_gl = c("not circumcised"=0,  "<1" = 1, "1-4"=2, "5-9"=3, "10-14"=4, "Don't know/missing"=9 )) %>%
  set_variable_labels(fg_age_gl = "Age at circumcision among girls age 0-14")

# //Person performing the circumcision among girls age 0-14
BRgirls <- BRgirls %>%
mutate(fg_who_gl = 
         case_when(
           g124==21 & g121==1 ~ 1, g124==22 & g121==1 ~ 2, g124==26 & g121==1 ~ 3 ,
           g124==11 & g121==1 ~ 4,  g124==12 & g121==1 ~ 5, g124==16 & g121==1 ~ 6 ,
           g124==96 & g121==1 ~ 7, g107>=98 & g121==1 ~ 9)) %>%
  set_value_labels(fg_who_gl = c("traditional circumciser" = 1, "traditional birth attendant"=2, 
                                 "other traditional agent"=3, "doctor"=4, "nurse/midwife"=5,
                                 "other health professional"=6, "other"=7, "Don't know/missing"=9 )) %>%
  set_variable_labels(fg_who_gl = "Person who performed the circumcision among girls age 0-14")

# //Type of circumcision among girls age 0-14
BRgirls <- BRgirls %>%
  mutate(fg_sewn_gl = 
           case_when(
             g123==1 & g121==1 ~ 1 ,
             g123==0 & g121==1 ~ 0,
             g123>=8 & g121==1 ~ 9)) %>%
  set_value_labels(fg_sewn_gl = c("Yes" = 1, "No"=0, "Dont know/missing"=9)) %>%
  set_variable_labels(fg_sewn_gl = "Female circumcision type is sewn closed among girls age 0-14")

# **************************************************************************************************
# **************************************************************************************************
# Produce Tables_Circum_gl excel file which contains the tables for the indicators of female circumcision among girls age 0-14

# // age groups for girls 
BRgirls <- BRgirls %>%
  mutate(age5 = case_when(b8<5~ 1, b8>=5 & b8<10~ 2, b8>=10~ 3)) %>%
  set_value_labels(age5 = c("0-4"=1, "5-9"=2, "10-14"=3)) %>%
  set_variable_labels(age5 = "Age of child in categories")

# //Prevalence of circumcision and age of circumcision
table_temp <-  BRgirls %>% 
  cross_rpct(
    cell_vars = list( age5, total()),
    col_vars = list(fg_age_gl),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Age of circumcision by current age")
write.xlsx(table_temp, here(chap, "Tables_Circum_gl.xls"), sheetName = "AgeFC_gl", append=TRUE)

# **************************************************************************************************
# //Prevalence of circumcision by mother's background characteristics
# Circumcised mother
BRgirls <- BRgirls %>%
  mutate(fg_fcircum_wm = 
           case_when(
             g102==1 ~ 1 ,
             g102==0 | g100==0 | g100==1 ~ 0)) %>%
  set_value_labels(fg_fcircum_wm = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(fg_fcircum_wm = "Circumcised among women age 15-49")

# ***** Among girls age 0-4 *****
table_temp <-  BRgirls %>% 
  filter(age5==1) %>%
  cross_rpct(
    cell_vars = list( v025, v024, v106, fg_fcircum_wm, v190, total()),
    col_vars = list(fg_fcircum_gl),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Female circumcision for girls age 0-4")
write.xlsx(table_temp, here(chap, "Tables_Circum_gl.xls"), sheetName = "FC_Age0_4", append=TRUE)


# ***** Among girls age 5-9 *****
table_temp <-  BRgirls %>% 
  filter(age5==2) %>%
  cross_rpct(
    cell_vars = list( v025, v024, v106, fg_fcircum_wm, v190, total()),
    col_vars = list(fg_fcircum_gl),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Female circumcision for girls age 5-9")
write.xlsx(table_temp, here(chap, "Tables_Circum_gl.xls"), sheetName = "FC_Age5_9", append=TRUE)

# ***** Among girls age 10-14 *****
table_temp <-  BRgirls %>% 
  filter(age5==3) %>%
  cross_rpct(
    cell_vars = list( v025, v024, v106, fg_fcircum_wm, v190, total()),
    col_vars = list(fg_fcircum_gl),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Female circumcision for girls age 10-14")
write.xlsx(table_temp, here(chap, "Tables_Circum_gl.xls"), sheetName = "FC_Age10_14", append=TRUE)

# ***** Among girls age 0-14 : Total *****
table_temp <-  BRgirls %>% 
  cross_rpct(
    cell_vars = list( v025, v024, v106, fg_fcircum_wm, v190, total()),
    col_vars = list(fg_fcircum_gl),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Female circumcision for girls Total: 0-14")
write.xlsx(table_temp, here(chap, "Tables_Circum_gl.xls"), sheetName = "FC_Age0_14", append=TRUE)

# **************************************************************************************************
# //Person performing the circumcision among women girls 0-14 and type of circumcision
table_temp <-  BRgirls %>% 
  cross_rpct(
    cell_vars = list(age5, total()),
    col_vars = list(fg_who_gl),
    weight = wt,
    total_label = "Weighted N",
    total_statistic = "w_cases",
    total_row_position = c("below"),
    expss_digits(digits=1)) %>%
  set_caption("Person who performed FC")
write.xlsx(table_temp, here(chap, "Tables_Circum_gl.xls"), sheetName = "Person_gl", append=TRUE)

