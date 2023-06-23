# ******************************************************************************
# Program: 			  CM_tables2.R
# Purpose: 		    produce tables for high risk birth and high risk fertility behavior
# Author:				  Mahmoud Elkasabi
# Date last modified: September 24 2021 by Mahmoud Elkasabi
# ******************************************************************************
########################################################################################################################
# High risk fertility indicators among women 
########################################################################################################################

table_temp = IRdata_RISK %>%
  tab_cells(cm_riskw_none, cm_riskw_unavoid, cm_riskw_any_avoid, cm_riskw_u18, cm_riskw_o34, cm_riskw_interval, cm_riskw_order, 
            cm_riskw_any_single, cm_riskw_mult1, cm_riskw_mult2, cm_riskw_mult3, cm_riskw_mult4, cm_riskw_mult5, cm_riskw_any_mult,
            cm_riskw_u18_avoid, cm_riskw_o34_avoid, cm_riskw_interval_avoid, cm_riskw_order_avoid) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("High risk fertility indicators among women ")

table_temp

write.xlsx(table_temp, "Tables_child_mort.xlsx", sheetName = "CM_high_risk_wm",append=TRUE)


########################################################################################################################
# High risk fertility indicators among births 
########################################################################################################################

table_temp = KRdata_RISK %>%
  tab_cells(cm_riskb_none, cm_riskb_unavoid, cm_riskb_any_avoid, cm_riskb_u18, cm_riskb_o34, cm_riskb_interval, cm_riskb_order,
              cm_riskb_any_single, cm_riskb_mult1, cm_riskb_mult2, cm_riskb_mult3, cm_riskb_mult4, cm_riskb_mult5, cm_riskb_any_mult,
              cm_riskb_u18_avoid, cm_riskb_o34_avoid, cm_riskb_interval_avoid, cm_riskb_order_avoid) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("High risk births and risk among births")

table_temp

write.xlsx(table_temp, "Tables_child_mort.xlsx", sheetName = "CM_high_risk_bth",append=TRUE)

########################################################################################################################
# High risk births and risk ratios
########################################################################################################################

table_temp <- KRdata_RISK %>%  
  group_by(cm_riskb_none) %>% 
  summarise(weighted.mean(b5, wt)) %>% 
  filter(cm_riskb_none==1)

denom <- 1 - table_temp[["weighted.mean(b5, wt)"]]

lcat <- c("cm_riskb_none", "cm_riskb_unavoid", "cm_riskb_any_avoid", "cm_riskb_u18", "cm_riskb_o34", "cm_riskb_interval", "cm_riskb_order",
          "cm_riskb_any_single", "cm_riskb_mult1", "cm_riskb_mult2", "cm_riskb_mult3", "cm_riskb_mult4", "cm_riskb_mult5", "cm_riskb_any_mult",
          "cm_riskb_u18_avoid", "cm_riskb_o34_avoid", "cm_riskb_interval_avoid", "cm_riskb_order_avoid")

ratio <- rep(NA, length(lcat))

for (i in seq_along(lcat)) {

  table_temp <- KRdata_RISK[KRdata_RISK[[lcat[i]]] ==1,]
  ratio[i] <- (1 - weighted.mean(table_temp[["b5"]], table_temp[["wt"]]))/denom
  
} 


RATIOS <- cbind(lcat,ratio)

write.xlsx(RATIOS, "Tables_child_mort.xlsx", sheetName = "CM_high_risk_ratio",append=TRUE)

