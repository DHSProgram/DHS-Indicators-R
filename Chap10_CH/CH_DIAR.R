# /*****************************************************************************************************
# Program: 			  CH_DIAR.R
# Purpose: 			  Code diarrhea variables.
# Data inputs: 		KR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: Aug 2 2022 by Shireen Assaf 
# Notes:				      Check notes for diarrhea care and treatment variables which are country specific.
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_diar				    "Diarrhea in the 2 weeks before the survey"
# ch_diar_care		  "Advice or treatment sought for diarrhea"
# 
# ch_diar_liq			  "Amount of liquids given for child with diarrhea"
# ch_diar_food		  "Amount of food given for child with diarrhea"
# 
# ch_diar_ors			  "Given oral rehydration salts for diarrhea"
# ch_diar_rhf			  "Given recommended homemade fluids for diarrhea"
# ch_diar_ors_rhf		"Given either ORS or RHF for diarrhea"
# ch_diar_zinc		  "Given zinc for diarrhea"
# ch_diar_zinc_ors	"Given zinc and ORS for diarrhea"
# ch_diar_ors_fluid	"Given ORS or increased fluids for diarrhea"
# ch_diar_ort			  "Given oral rehydration treatment and increased liquids for diarrhea"
# ch_diar_ort_feed	"Given ORT and continued feeding for diarrhea"
# ch_diar_antib		  "Given antibiotic drugs for diarrhea"
# ch_diar_antim		  "Given antimotility drugs for diarrhea"
# ch_diar_intra		  "Given Intravenous solution for diarrhea"
# ch_diar_other		  "Given home remedy or other treatment  for diarrhea"
# ch_diar_notrt		  "No treatment for diarrhea"
# 
# ch_diar_govh 		    "Diarrhea treatment sought from government hospital among children with diarrhea"
# ch_diar_govh_trt 	  "Diarrhea treatment sought from government hospital among children with diarrhea that sought treatment"
# ch_diar_govh_ors 	  "Diarrhea treatment sought from government hospital among children with diarrhea that received ORS"
# ch_diar_govcent 	  "Diarrhea treatment sought from government health center among children with diarrhea"
# ch_diar_govcent_trt "Diarrhea treatment sought from government health center among children with diarrhea that sought treatment"
# ch_diar_govcent_ors "Diarrhea treatment sought from government health center among children with diarrhea that received ORS"
# ch_diar_pclinc 		  "Diarrhea treatment sought from private hospital/clinic among children with diarrhea"
# ch_diar_pclinc_trt 	"Diarrhea treatment sought from private hospital/clinic among children with diarrhea that sought treatment"
# ch_diar_pclinc_ors 	"Diarrhea treatment sought from private hospital/clinic among children with diarrhea that received ORS"
# ch_diar_pdoc 		    "Diarrhea treatment sought from private doctor among children with diarrhea"
# ch_diar_pdoc_trt 	  "Diarrhea treatment sought from private doctor among children with diarrhea that sought treatment"
# ch_diar_pdoc_ors 	  "Diarrhea treatment sought from private doctor among children with diarrhea that received ORS"
# ch_diar_pharm 		  "Diarrhea treatment sought from a pharmacy among children with diarrhea"
# ch_diar_pharm_trt 	"Diarrhea treatment sought from a pharmacy among children with diarrhea that sought treatment"
# ch_diar_pharm_ors 	"Diarrhea treatment sought from a pharmacy among children with diarrhea that received ORS"
# ----------------------------------------------------------------------------*/

# weight variable 
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# //Diarrhea symptoms
KRdata <- KRdata %>%
  mutate(ch_diar = 
           case_when(
             (h11==1 | h11==2) & b5==1 ~ 1,
             b5==1 ~ 0  )) %>%
  set_value_labels(ch_diar = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar = "Diarrhea in the 2 weeks before the survey")

# //Diarrhea treatment	
# This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
# The code below only excludes traditional practitioner (usually h12t). 
# The variable for traditional healer may be different for different surveys (you can check this checking all the h12* variables). 
# Some surveys also exclude pharmacies, shop, or other sources.
# If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
# h12k from the code below.

KRdata <- KRdata %>%
  mutate(ch_diar_care =
           case_when(
             ch_diar==1 & 
               (h12a == 1 | h12b == 1 | h12c == 1 | h12d == 1 | h12e == 1 | h12f == 1 |
                h12g == 1 | h12h == 1 | h12i == 1 | h12j == 1 | h12k == 1 | h12l == 1 |
                h12m == 1 | h12n == 1 | h12o == 1 | h12p == 1 | h12q == 1 | h12r == 1 |
                h12s == 1 |             h12u == 1 | h12v == 1 | h12w == 1 | h12x == 1 )  ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_care = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_care = "Advice or treatment sought for diarrhea")

# //Liquid intake
KRdata <- KRdata %>%
  mutate(ch_diar_liq =
           case_when(
             ch_diar==1 & h38==5  ~ 1 ,
             ch_diar==1 & h38==4  ~ 2 ,
             ch_diar==1 & h38==3  ~ 3 ,
             ch_diar==1 & h38==2  ~ 4 ,
             ch_diar==1 & h38==0  ~ 5 ,
             ch_diar==1 & (h38==8 | h38==9) ~ 9)) %>%
  set_value_labels(ch_diar_liq = c("More" = 1, "Same as usual"=2, "Somewhat less"=3, "Much less"=4, 
                                   "None"=5, "Don't know/missing"=9 )) %>%
  set_variable_labels(ch_diar_liq = "Amount of liquids given for child with diarrhea")

# //Food intake
KRdata <- KRdata %>%
  mutate(ch_diar_food =
           case_when(
             ch_diar==1 & h39==5  ~ 1 ,
             ch_diar==1 & h39==4  ~ 2 ,
             ch_diar==1 & h39==3  ~ 3 ,
             ch_diar==1 & h39==2  ~ 4 ,
             ch_diar==1 & h39==0  ~ 5 ,
             ch_diar==1 & h39==1  ~ 6 ,
             ch_diar==1 & (h39==8 | h39==9) ~ 9)) %>%
  set_value_labels(ch_diar_food = c("More" = 1, "Same as usual"=2, "Somewhat less"=3, "Much less"=4, 
                                    "None"=5, "Never gave food"=6, "Don't know/missing"=9 )) %>%
  set_variable_labels(ch_diar_food = "Amount of food given for child with diarrhea")


# //ORS
KRdata <- KRdata %>%
  mutate(ch_diar_ors =
           case_when(
             ch_diar==1 & (h13==1 | h13==2 | h13b==1)  ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ors = "Given oral rehydration salts for diarrhea")

# //RHF
KRdata <- KRdata %>%
  mutate(ch_diar_rhf =
           case_when(
             ch_diar==1 & (h14==1 | h14==2) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_rhf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_rhf = "Given recommended homemade fluids for diarrhea")

# //ORS or RHF
KRdata <- KRdata %>%
  mutate(ch_diar_ors_rhf =
           case_when(
             ch_diar==1 & (h13==1 | h13==2 | h13b==1 | h14==1 | h14==2) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ors_rhf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ors_rhf = "Given either ORS or RHF for diarrhea")

# //Zinc
KRdata <- KRdata %>%
  mutate(ch_diar_zinc =
           case_when(
             ch_diar==1 & h15e==1 ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_zinc = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_zinc = "Given zinc for diarrhea")

# //Zinc and ORS
KRdata <- KRdata %>%
  mutate(ch_diar_zinc_ors =
           case_when(
             ch_diar==1 & ((h13==1 | h13==2 | h13b==1) & h15e==1) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_zinc_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_zinc_ors = "Given zinc and ORS for diarrhea")

# //ORS or increased liquids
KRdata <- KRdata %>%
  mutate(ch_diar_ors_fluid =
           case_when(
             ch_diar==1 & (h13==1 | h13==2 | h13b==1 | h38==5) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ors_fluid = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ors_fluid = "Given ORS or increased fluids for diarrhea")

# //ORT or increased liquids
KRdata <- KRdata %>%
  mutate(ch_diar_ort =
           case_when(
             ch_diar==1 & (h13==1 | h13==2 | h14==1 | h14==2 | h38==5) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ort = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ort = "Given oral rehydration treatment or increased liquids for diarrhea")

# //ORT and continued feeding
KRdata <- KRdata %>%
  mutate(ch_diar_ort_feed =
           case_when(
             ch_diar==1 & ((h13==1 | h13==2 | h13b==1 | h14==1 | h14==2 | h38==5)&(h39>=3 & h39<=5)) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ort_feed = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ort_feed = "Given ORT and continued feeding for diarrhea")

# //Antibiotics
KRdata <- KRdata %>%
  mutate(ch_diar_antib =
           case_when(
             ch_diar==1 & (h15==1 | h15b==1) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_antib = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_antib = "Given antibiotic drugs for diarrhea")

# //Antimotility drugs
KRdata <- KRdata %>%
  mutate(ch_diar_antim =
           case_when(
             ch_diar==1 & h15a==1~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_antim = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_antim = "Given antimotility drugs for diarrhea")

# //Intravenous solution
KRdata <- KRdata %>%
  mutate(ch_diar_intra =
           case_when(
             ch_diar==1 & h15c==1~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_intra = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_intra = "Given Intravenous solution for diarrhea")

# //Home remedy or other treatment
KRdata <- KRdata %>%
  mutate(ch_diar_other =
           case_when(
             ch_diar==1 & h15d==1 | h15f==1 | h15g==1 | h15h==1 | h15i==1 | h15j==1 | h15k==1 | h15l==1 | h15m==1 | h20==1 ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_other = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_other = "Given home remedy or other treatment for diarrhea")

# //No treatment
KRdata <- KRdata %>%
  mutate(ch_diar_notrt =
           case_when(
             ch_diar==1 &  (ch_diar_ors==1 | ch_diar_rhf==1 | ch_diar_ors_rhf==1 | ch_diar_zinc==1 | ch_diar_zinc_ors==1 | ch_diar_ors_fluid==1 | 
             ch_diar_ort==1 | ch_diar_ort_feed==1 | ch_diar_antib==1 | ch_diar_antim==1 | ch_diar_intra==1 | ch_diar_other==1) ~ 0 ,
             ch_diar==1 & h21a==1 ~ 1 )) %>%
  set_value_labels(ch_diar_notrt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_notrt = "No treatment for diarrhea")

# Diarrhea treatment by source (among children with diarrhea symptoms)
# Three population bases: 1. among children with diarrhea, 2. among children with diarrhea that sought treatment
#                         3. among children with diarrhea that received ORS
# This is country specific and needs to be checked to produce the specific source of interest. 
# Some sources are coded below and the same logic can be used to code other sources. h12a-z indicates the source.
# 
# //Diarrhea treatment in government hospital
KRdata <- KRdata %>%
  mutate(ch_diar_govh = 
           case_when(
             ch_diar==1 & h12a==1  & b5==1 ~ 1 ,
             ch_diar==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_govh = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_govh = "Diarrhea treatment sought from government hospital among children with diarrhea")

KRdata <- KRdata %>%
  mutate(ch_diar_govh_trt = 
           case_when(
             ch_diar_care==1 & h12a==1 & b5==1 ~ 1 ,
             ch_diar_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_govh_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_govh_trt = "Diarrhea treatment sought from government hospital among children with diarrhea that sought treatment")

KRdata <- KRdata %>%
  mutate(ch_diar_govh_ors = 
           case_when(
             ch_diar_ors==1 & h12a==1 & b5==1 ~ 1 ,
             ch_diar_ors==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_govh_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_govh_ors = "Diarrhea treatment sought from government hospital among children with diarrhea that received ORS")

# //Diarrhea treatment in government health center
KRdata <- KRdata %>%
  mutate(ch_diar_govcent = 
           case_when(
             ch_diar==1 & h12b==1  & b5==1 ~ 1 ,
             ch_diar==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_govcent = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_govcent = "Diarrhea treatment sought from government health center among children with diarrhea")

KRdata <- KRdata %>%
  mutate(ch_diar_govcent_trt = 
           case_when(
             ch_diar_care==1 & h12b==1 & b5==1 ~ 1 ,
             ch_diar_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_govcent_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_govcent_trt = "Diarrhea treatment sought from government health center among children with diarrhea that sought treatment")

KRdata <- KRdata %>%
  mutate(ch_diar_govcent_ors = 
           case_when(
             ch_diar_ors==1 & h12b==1 & b5==1 ~ 1 ,
             ch_diar_ors==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_govcent_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_govcent_ors = "Diarrhea treatment sought from government health center among children with diarrhea that received ORS")

# //Diarrhea treatment from a private hospital/clinic
KRdata <- KRdata %>%
  mutate(ch_diar_pclinc = 
           case_when(
             ch_diar==1 & h12j==1  & b5==1 ~ 1 ,
             ch_diar==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pclinc = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pclinc = "Diarrhea treatment sought from private hospital/clinic among children with diarrhea")

KRdata <- KRdata %>%
  mutate(ch_diar_pclinc_trt = 
           case_when(
             ch_diar_care==1 & h12j==1 & b5==1 ~ 1 ,
             ch_diar_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pclinc_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pclinc_trt = "Diarrhea treatment sought from private hospital/clinic among children with diarrhea that sought treatment")

KRdata <- KRdata %>%
  mutate(ch_diar_pclinc_ors = 
           case_when(
             ch_diar_ors==1 & h12j==1 & b5==1 ~ 1 ,
             ch_diar_ors==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pclinc_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pclinc_ors = "Diarrhea treatment sought from private hospital/clinic among children with diarrhea that received ORS")

# //Diarrhea treatment from a private doctor
KRdata <- KRdata %>%
  mutate(ch_diar_pdoc = 
           case_when(
             ch_diar==1 & h12l==1  & b5==1 ~ 1 ,
             ch_diar==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pdoc = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pdoc = "Diarrhea treatment sought from private doctor among children with diarrhea")

KRdata <- KRdata %>%
  mutate(ch_diar_pdoc_trt = 
           case_when(
             ch_diar_care==1 & h12l==1 & b5==1 ~ 1 ,
             ch_diar_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pdoc_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pdoc_trt = "Diarrhea treatment sought from private doctor among children with diarrhea that sought treatment")

KRdata <- KRdata %>%
  mutate(ch_diar_pdoc_ors = 
           case_when(
             ch_diar_ors==1 & h12l==1 & b5==1 ~ 1 ,
             ch_diar_ors==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pdoc_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pdoc_ors = "Diarrhea treatment sought from private doctor among children with diarrhea that received ORS")

# //Diarrhea treatment from a pharmacy
KRdata <- KRdata %>%
  mutate(ch_diar_pharm = 
           case_when(
             ch_diar==1 & h12k==1  & b5==1 ~ 1 ,
             ch_diar==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pharm = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pharm = "Diarrhea treatment sought from pharmacy among children with diarrhea")

KRdata <- KRdata %>%
  mutate(ch_diar_pharm_trt = 
           case_when(
             ch_diar_care==1 & h12k==1 & b5==1 ~ 1 ,
             ch_diar_care==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pharm_trt = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pharm_trt = "Diarrhea treatment sought from pharmacy among children with diarrhea that sought treatment")

KRdata <- KRdata %>%
  mutate(ch_diar_pharm_ors = 
           case_when(
             ch_diar_ors==1 & h12k==1 & b5==1 ~ 1 ,
             ch_diar_ors==1 & b5==1 ~ 0 )) %>%
  set_value_labels(ch_diar_pharm_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_pharm_ors = "Diarrhea treatment sought from pharmacy among children with diarrhea that received ORS")
