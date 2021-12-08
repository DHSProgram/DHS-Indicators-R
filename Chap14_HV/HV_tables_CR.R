# ******************************************************************************
# Program: 			  HV_tables_CR.R
# Purpose: 		    produce tables for indicators
# Data outputs:		tables in excel sheets
# Author:				  Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# ******************************************************************************
#Note this do file will produce the following tables in excel:
#6.	Tables_prev_cpl:	Contains the tables for HIV prevalence for couples
################################################################################  
  
# use HIV weight
  CRARmerge <- CRARmerge %>%
    mutate(wt=hiv05/1000000)    

################################################################################  
# HIV prevalence among couples
  # among women
table_temp <-  CRARmerge %>% 
  calc_cro_rpct(
    cell_vars = list(v013, mv013, v025, v024, v106, mv106, v190, total()),
    col_vars = list(hv_couple_hiv_status),
    weight = wt,
    total_label = "weighted N",
    total_statistic = "w_cases",
    expss_digits(digits=1)) %>%   
  set_caption("HIV prevalence among couples")

write.xlsx(table_temp, "Tables_HV.xlsx", sheetName = "hv_prev_cp",append=TRUE)