# # /*****************************************************************************************************
# # Program: 			NT_SALT.R
# # Purpose: 			Code to compute salt indicators in households
# # Data inputs: 	HR dataset
# # Data outputs:	coded variables
# # Author:				Shireen Assaf
# # Date last modified: December 3, 2021 by Shireen Assaf 
# # *****************************************************************************************************/
# # 
# # /*----------------------------------------------------------------------------
# # Variables created in this file:
# # 
# # nt_salt_any	"Salt among all households"
# # nt_salt_iod	"Households with iodized salt"
# # 
# # ----------------------------------------------------------------------------*/
# 
 HRdata <- HRdata %>%
   mutate(wt = hv005/1000000)
 
# //Salt among all households
 HRdata <- HRdata %>%
   mutate(nt_salt_any =
             case_when(
              hv234a <2  ~ 1 ,
              hv234a ==6 ~ 2,
              hv234a ==3 ~ 3)) %>%
   set_value_labels(nt_salt_any = c("With salt tested" = 1, "With salt but not tested"=2, "No salt in household"=3  )) %>%
   set_variable_labels(nt_salt_any = "Salt among all households")
 
# //Have iodized salt
 HRdata <- HRdata %>%
   mutate(nt_salt_iod =
            case_when(
              hv234a ==1 & hv234a<3  ~ 1 ,
              hv234a ==0 & hv234a<3 ~ 0)) %>%
   set_value_labels(nt_salt_iod = c("Yes" = 1, "No"=0  )) %>%
   set_variable_labels(nt_salt_iod = "Households with iodized salt")
 
