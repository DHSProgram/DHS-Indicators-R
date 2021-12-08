# ******************************************************************************
# Program: 	HV_backgroundvars.R
# Purpose: 		Compute the background variables needed for the HV_tables 
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# ******************************************************************************
# Generate background variable that are not standard variables in the date files
# These variables are used for the tabulations of HIV prevalence in the final reports
# -----------------------------------------------------------------------------#

#employed in the last 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(empl = case_when(
    v731==0  ~ 0,
    v731 %in% (1:3) ~ 1,
    v731 %in% (8:9) ~ 99),
    empl = add_labels(empl, labels = c("Not employed"=0, "Employed"=1)),
    empl = set_label(empl, label = "Employment in the past 12 months"))  %>%
    replace_with_na(replace = list(empl = c(99)))
  
#polygamy for women
IRMRARmerge <- IRMRARmerge %>%
  mutate(poly_w = case_when(
    v502!=1  ~ 0,
    v502==1 & v505==0 ~ 1,
    v502==1 & v505>0  ~ 2),
    poly_w = add_labels(poly_w, labels = c("Not currently in union"=0, "In non-polygynous union"=1, "In polygynous union"=2)),
    poly_w = set_label(poly_w, label = "Type of union"))

#polygamy for men
IRMRARmerge <- IRMRARmerge %>%
  mutate(poly_m = case_when(
    v502!=1  ~ 0,
    v502==1 & v035==1 ~ 1,
    v502==1 & v035>1  ~ 2),
    poly_m = add_labels(poly_m, labels = c("Not currently in union"=0, "In non-polygynous union"=1, "In polygynous union"=2)),
    poly_m = set_label(poly_m, label = "Type of union"))

#polygamy for total
IRMRARmerge <- IRMRARmerge %>%
  mutate(poly_t = case_when(
    v502!=1  ~ 0,
    poly_w==2 & poly_m==2  ~ 2,
    TRUE ~ 1),
    poly_t = add_labels(poly_t, labels = c("Not currently in union"=0, "In non-polygynous union"=1, "In polygynous union"=2)),
    poly_t = set_label(poly_t, label = "Type of union"))

#Times slept away from home in the past 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(timeaway = case_when(
    v167==0  ~ 0,
    v167 %in% (1:2) ~ 1,
    v167 %in% (3:4) ~ 2,
    v167 %in% (5:90) ~ 3,
    v167 %in% (98:99) ~ 99),
    timeaway = add_labels(timeaway, labels = c("None"=0, "1-2"=1, "3-4"=2, "5+"=3)),
    timeaway = set_label(timeaway, label = "Times slept away from home in past 12 months"))%>%
  replace_with_na(replace = list(timeaway = c(99)))

#Time away in the past 12 months for more than 1 month
IRMRARmerge <- IRMRARmerge %>%
  mutate(timeaway12m = case_when(
    v167>0 & v168==1 ~ 1,
    v167>0 & v168==0 ~ 2,
    v167==0 ~ 3),
    timeaway12m = add_labels(timeaway12m, labels = c("Away for more than 1 month at a time"=1, "Away only for less than 1 month at a time"=2, "Not away"=3)),
    timeaway12m = set_label(timeaway12m, label = "Time away in the past 12 months"))

#currently pregnant
IRMRARmerge <- IRMRARmerge %>%
  mutate(preg = case_when(
    v213==0  ~ 0,
    v213==1 | v213==8  ~ 1,
    v213 ==9 ~ 99),
    preg = add_labels(preg, labels = c("No"=0, "Yes"=1)),
    preg = set_label(preg, label = "Currently pregnant"))%>%
  replace_with_na(replace = list(preg = c(99)))

#ANC for last birth in the past 3 years
#need age of most recent child to limit to 3 years

# age of child. If b19_01 is not available in the data use v008 - b3_01
if ("TRUE" %in% (!("b19_01" %in% names(IRMRARmerge))))
  IRMRARmerge [[paste("b19_01")]] <- NA
if ("TRUE" %in% all(is.na(IRMRARmerge $b19_01)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  IRMRARmerge <- IRMRARmerge %>%
    mutate(age = b19_01)
} else {
  IRMRARmerge <- IRMRARmerge %>%
    mutate(age = v008 - b3_01)
}


#ANC for last birth in past 3 years
IRMRARmerge <- IRMRARmerge %>%
  mutate(ancplace = case_when(
    m2n_1==1 | age>=36 ~ 0,
    m14_1>0 & age<=36 & (m57e_1==1 | m57f_1==1 | m57g_1==1 | m57h_1==1 | m57i_1==1 | m57j_1==1 | m57k_1==1 | m57l_1==1) ~ 1,
    m14_1>0 & age<=36 & (m57a_1==1 | m57b_1==1 | m57c_1==1 | m57d_1==1 | m57m_1==1 | m57n_1==1 | m57o_1==1 | m57p_1==1 | m57q_1==1 | m57r_1==1 | m57s_1==1 | m57t_1==1 | m57u_1==1 | m57v_1==1 | m57x_1==1) ~ 2),
    ancplace = add_labels(ancplace, labels = c("No ANC/No birth n past 3 years"=0, "ANC provided by public sector"=1, "ANC provided by other than public sector"=2)),
    ancplace = set_label(ancplace, label = "ANC for last birth in past 3 years"))

#age at first sex
IRMRARmerge <- IRMRARmerge %>%
  mutate(agesex = case_when(
    v531 %in% (1:15) ~ 1,
    v531 %in% (16:17) ~ 2,
    v531 %in% (18:19) ~ 3,
    v531 %in% (20:96) ~ 4,
    v531 %in% (97:99) ~ 99),
    agesex = add_labels(agesex, labels = c("<16"=1, "16-17"=2, "18/19"=3, "20+"=4)),
    agesex = set_label(agesex, label = "Age at first sexual intercourse"))%>%
  replace_with_na(replace = list(agesex = c(99)))

#Number of lifetime partners
IRMRARmerge <- IRMRARmerge %>%
  mutate(numprtnr = case_when(
    v836 %in% (1) ~ 1,
    v836 %in% (2) ~ 2,
    v836 %in% (3:4) ~ 3,
    v836 %in% (5:9) ~ 4,
    v836 %in% (10:95) ~ 5,
    v836 %in% (98:99) ~ 99),
    numprtnr = add_labels(numprtnr, labels = c("1"=1, "2"=2, "3-4"=3, "5-9"=4, "10+"=5)),
    numprtnr = set_label(numprtnr, label = "Number of lifetime partners"))%>%
  replace_with_na(replace = list(numprtnr = c(99)))


#Multiple sexual partners in the past 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(multisex = case_when(
    v766b==0 ~ 0,
    (v527 %in% (100:251) | v527 %in% (300:311)) & v766b==1 ~ 1,
    (v527 %in% (100:251) | v527 %in% (300:311)) & v766b %in% (2:99) ~ 2,
    v531==0 ~ 99),
    multisex = add_labels(multisex, labels = c("0"=0,"1"=1, "2+"=2)),
    multisex = set_label(multisex, label = "Multiple sexual partners in past 12 months"))%>%
  replace_with_na(replace = list(multisex = c(99)))

#Non-marital, non-cohabiting partner in the past 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(prtnrcohab = case_when(
    v766a==0 ~ 0,
    v766a==1 ~ 1,
    v766a %in% (2:95) ~ 2,
    v766a %in% (98:99) ~ 99),
    prtnrcohab = add_labels(prtnrcohab, labels = c("0"=0,"1"=1, "2+"=2)),
    prtnrcohab = set_label(prtnrcohab, label = "Multiple sexual partners in past 12 months"))%>%
  replace_with_na(replace = list(prtnrcohab = c(99)))

#Condom use at last sexual intercourse in past 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(condomuse = case_when(
    v766b==0 ~ 0,
    v761==0 ~ 1,
    v761==1 ~ 2,
    v531==0 ~ 99),
    condomuse = add_labels(condomuse, labels = c("No sex in past 12 months"=0,"Did not use condom"=1, "Used condom"=2)),
    condomuse = set_label(condomuse, label = "Condom use at last sexual intercourse in the past 12 months"))%>%
  replace_with_na(replace = list(condomuse = c(99)))

#Paid for sex in the past 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(paidsex = v793) %>%
  mutate(paidsex = case_when(
    v793a==0 ~ 2,
    is.na(v793) | v531==0 ~ 99),
    paidsex = add_labels(paidsex, labels = c("No"=0,"Yes-Used condom"=1, "Yes-Did not use condom"=2)),
    paidsex = set_label(paidsex, label = "Paid for sexual intercourse in the past 12 months"))%>%
  replace_with_na(replace = list(paidsex = c(99)))

#STI in the past 12 months
IRMRARmerge <- IRMRARmerge %>%
  mutate(sti12m = case_when(
    !(v531==0 | v531==99 | is.na(v531)) & (v763a==1 | v763b==1 | v763c==1) ~ 1,
    !(v531==0 | v531==99 | is.na(v531)) & !(v763a==1 | v763b==1 | v763c==1) ~ 0,
    (v531==0 | v531==99 | is.na(v531)) ~ 99),
    sti12m = add_labels(sti12m, labels = c("No"=0,"Yes-Used condom"=1, "Yes-Did not use condom"=2)),
    sti12m = set_label(sti12m, label = "Had an STI or STI symptoms in the past 12 months"))%>%
  replace_with_na(replace = list(sti12m = c(99)))

#Had prior HIV test and whether they received results
IRMRARmerge <- IRMRARmerge %>%
  mutate(test_prior = case_when(
    v781==1 & v828==1 ~ 1,
    v781==1 & v828==0 ~ 2,
    v781==0 ~ 3,
    v531==0 ~ 99),
    test_prior = add_labels(test_prior, labels = c("Tested and received results"=1, "Tested and did not receive results"=2, "Never tested"=3)),
    test_prior = set_label(test_prior, label = "Prior HIV testing status and whether received test result"))%>%
  replace_with_na(replace = list(test_prior = c(99)))


#new age variable among young
IRMRARmerge <- IRMRARmerge %>%
  mutate(age_yng = case_when(
    v012 %in% (15:17) ~ 1,
    v012 %in% (18:19) ~ 2,
    v012 %in% (20:22) ~ 3,
    v012 %in% (23:24) ~ 4,
    TRUE ~ 99),
    age_yng = add_labels(age_yng, labels = c("15-17"=1, "18-19"=2, "20-22"=3, "23-24"=4)),
    age_yng = set_label(age_yng, label = "Age at first sexual intercourse"))%>%
  replace_with_na(replace = list(age_yng = c(99)))
