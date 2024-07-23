# ******************************************************************************
# Program: 			  HV_tables_MR.R
# Purpose: 		    produce tables for indicators
# Data outputs:		tables in excel sheets
# Author:				  Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# ******************************************************************************
#Note this do file will produce the following tables in excel:

#2.	Tables_prev_wm:		Contains the tables for HIV prevalence for women
#3.	Tables_prev_mn:		Contains the tables for HIV prevalence for men
#4.	Tables_prev_tot:	Contains the tables for HIV prevalence for total
#5.	Tables_circum:		Contains the tables for HIV prevalence by male circumcision


# use HIV weight
IRMRARmerge <- IRMRARmerge %>%
    mutate(wt=hiv05/1000000)    

################################################################################  
# HIV prevalence
  # among women
table_temp <-  IRMRARmerge %>% 
  filter(sex==2) %>%
  calc_cro_rpct(
    cell_vars = list(v013, total()),
    col_vars = list(hv_hiv_pos,hv_hiv1_pos,hv_hiv2_pos,hv_hiv1or2_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among women")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prev_wm",append=TRUE)

# among men
table_temp <-  IRMRARmerge %>% 
  filter(sex==1) %>%
  calc_cro_rpct(
    cell_vars = list(v013, total()),
    col_vars = list(hv_hiv_pos,hv_hiv1_pos,hv_hiv2_pos,hv_hiv1or2_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among men")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prev_mn",append=TRUE)

# among all
table_temp <-  IRMRARmerge %>% 
  calc_cro_rpct(
    cell_vars = list(v013, total()),
    col_vars = list(hv_hiv_pos,hv_hiv1_pos,hv_hiv2_pos,hv_hiv1or2_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among all")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prev_all",append=TRUE)


################################################################################  
# HIV prevalence - by background variables
# among women
table_temp <-  IRMRARmerge %>% 
  filter(sex==2) %>%
  calc_cro_rpct(
    cell_vars = list(v131,v130,empl,v025,v024,v106,v190,v501,poly_w,timeaway,timeaway12m,preg,ancplace,agesex,numprtnr,multisex,prtnrcohab,condomuse,sti12m,test_prior, total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among women - by background variables")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_back_wm",append=TRUE)

# among men
table_temp <-  IRMRARmerge %>% 
  filter(sex==1) %>%
  calc_cro_rpct(
    cell_vars = list(v131,v130,empl,v025,v024,v106,v190,v501,poly_m,timeaway,timeaway12m,agesex,numprtnr,multisex,prtnrcohab,condomuse,paidsex,sti12m,test_prior, total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among men - by background variables")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_back_mn",append=TRUE)

# among all
table_temp <-  IRMRARmerge %>% 
  calc_cro_rpct(
    cell_vars = list(v131,v130,empl,v025,v024,v106,v190,v501,poly_t,timeaway,timeaway12m,agesex,numprtnr,multisex,prtnrcohab,condomuse,sti12m,test_prior, total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among all - by background variables")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_back_all",append=TRUE)

################################################################################  
# HIV prevalence among young 15-24 - by background variables
# among women
table_temp <-  IRMRARmerge %>% 
  filter(v013<3) %>%
  filter(sex==2) %>%
  calc_cro_rpct(
    cell_vars = list(v131,v130,empl,v025,v024,v106,v190,v501,poly_w,timeaway,timeaway12m,preg,ancplace,agesex,numprtnr,multisex,prtnrcohab,condomuse,sti12m,test_prior, total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among young women - by background variables")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_1524_wm",append=TRUE)

# among men
table_temp <-  IRMRARmerge %>% 
  filter(v013<3) %>%
  filter(sex==1) %>%
  calc_cro_rpct(
    cell_vars = list(v131,v130,empl,v025,v024,v106,v190,v501,poly_m,timeaway,timeaway12m,agesex,numprtnr,multisex,prtnrcohab,condomuse,paidsex,sti12m,test_prior, total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among young men - by background variables")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_1524_mn",append=TRUE)

# among all
table_temp <-  IRMRARmerge %>% 
  filter(v013<3) %>%
  calc_cro_rpct(
    cell_vars = list(v131,v130,empl,v025,v024,v106,v190,v501,poly_t,timeaway,timeaway12m,agesex,numprtnr,multisex,prtnrcohab,condomuse,sti12m,test_prior, total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among young population - by background variables")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_1524_all",append=TRUE)

################################################################################  
# Prior HIV testing by current HIV status
# among women
table_temp <-  IRMRARmerge %>% 
  filter(sex==2) %>%
  calc_cro_cpct(
    cell_vars = list(hv_pos_ever_test, hv_pos_12m_test, hv_pos_more12m_test, hv_pos_ever_noresult, hv_pos_nottested,total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Prior HIV testing among women")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prtst_wm",append=TRUE)

# among men
table_temp <-  IRMRARmerge %>% 
  filter(sex==1) %>%
  calc_cro_cpct(
    cell_vars = list(hv_pos_ever_test, hv_pos_12m_test, hv_pos_more12m_test, hv_pos_ever_noresult, hv_pos_nottested,total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Prior HIV testing among men")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prtst_mn",append=TRUE)

# among all
table_temp <-  IRMRARmerge %>% 
  calc_cro_cpct(
    cell_vars = list(hv_pos_ever_test, hv_pos_12m_test, hv_pos_more12m_test, hv_pos_ever_noresult, hv_pos_nottested,total()),
    col_vars = list(hv_hiv_pos),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("Prior HIV testing among all")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prtst_all",append=TRUE)


#HIV prevalence by male circumcision
###############################################################################

### Circumcised by health professional
# check if hv_hiv_circum_skilled exists
if ("TRUE" %in% (!("hv_hiv_circum_skilled" %in% names(IRMRARmerge))))
  IRMRARmerge [[paste("hv_hiv_circum_skilled")]] <- NA
if ("TRUE" %in% all(is.na(IRMRARmerge$hv_hiv_circum_skilled)))
{ check <- 0} else { check <- 1}

if (check==1) {

  # Circumcised by health professional
  table_temp <-  IRMRARmerge %>% 
    filter(sex==1 & hv_hiv_circum_skilled ==1) %>%
    calc_cro_rpct(
      cell_vars = list(v013,v131,v130,v025,v024,v106,v190, total()),
      col_vars = list(hv_hiv_pos),
      weight = wt,
      total_label = "weighted N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("HIV prevalence among men - Circumcised by health professional")
  
  write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_circum1_mn",append=TRUE)
  
}

### Circumcised bytraditional/other
# check if hv_hiv_circum_trad exists
if ("TRUE" %in% (!("hv_hiv_circum_trad" %in% names(IRMRARmerge))))
  IRMRARmerge [[paste("hv_hiv_circum_trad")]] <- NA
if ("TRUE" %in% all(is.na(IRMRARmerge$hv_hiv_circum_trad)))
{ check <- 0} else { check <- 1}

if (check==1) {
  
  # Circumcised by traditional/other
  table_temp <-  IRMRARmerge %>% 
    filter(sex==1 & hv_hiv_circum_trad ==1) %>%
    calc_cro_rpct(
      cell_vars = list(v013,v131,v130,v025,v024,v106,v190, total()),
      col_vars = list(hv_hiv_pos),
      weight = wt,
      total_label = "weighted N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("HIV prevalence among men - Circumcised by traditional/other")
  
  write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_circum2_mn",append=TRUE)
  
}

### All circumcised
# check if hv_hiv_circum_pos exists
if ("TRUE" %in% (!("hv_hiv_circum_pos" %in% names(IRMRARmerge))))
  IRMRARmerge [[paste("hv_hiv_circum_pos")]] <- NA
if ("TRUE" %in% all(is.na(IRMRARmerge$hv_hiv_circum_pos)))
{ check <- 0} else { check <- 1}

if (check==1) {
  
  # All circumcised
  table_temp <-  IRMRARmerge %>% 
    filter(sex==1 & hv_hiv_circum_pos ==1) %>%
    calc_cro_rpct(
      cell_vars = list(v013,v131,v130,v025,v024,v106,v190, total()),
      col_vars = list(hv_hiv_pos),
      weight = wt,
      total_label = "weighted N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("HIV prevalence among men - All circumcised")
  
  write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_circum3_mn",append=TRUE)
  
}

### Uncircumcised
# check if hv_hiv_uncircum_pos exists
if ("TRUE" %in% (!("hv_hiv_uncircum_pos" %in% names(IRMRARmerge))))
  IRMRARmerge [[paste("hv_hiv_uncircum_pos")]] <- NA
if ("TRUE" %in% all(is.na(IRMRARmerge$hv_hiv_uncircum_pos)))
{ check <- 0} else { check <- 1}

if (check==1) {
  
  # Uncircumcised
  table_temp <-  IRMRARmerge %>% 
    filter(sex==1 & hv_hiv_circum_pos ==0) %>%
    calc_cro_rpct(
      cell_vars = list(v013,v131,v130,v025,v024,v106,v190, total()),
      col_vars = list(hv_hiv_pos),
      weight = wt,
      total_label = "weighted N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("HIV prevalence among men - Uncircumcised")
  
  write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_circum4_mn",append=TRUE)
  
}
