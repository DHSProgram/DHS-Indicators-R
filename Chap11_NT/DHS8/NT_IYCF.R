# /*****************************************************************************************************
# Program: 			NT_IYCF.R
# Purpose: 			Code to compute infant and child feeding indicators
# Data inputs: 	KR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Dec 21, 2021 by Shireen Assaf 
# Note:			See note on line 133 on how to add country specific foods.	
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# nt_bf_status		"Breastfeeding status for last-born child under 2 years"
# nt_ebf				  "Exclusively breastfed - last-born under 6 months"
# nt_predo_bf			"Predominantly breastfed - last-born under 6 months"
# nt_ageapp_bf		"Age-appropriately breastfed - last-born under 2 years"
# nt_food_bf			"Introduced to solid, semi-solid, or soft foods - last-born 6-8 months"
# 	
# nt_bf_curr			"Currently breastfeeding - last-born under 2 years"
# nt_bf_cont_1yr	"Continuing breastfeeding at 1 year (12-15 months) - last-born under 2 years"
# nt_bf_cont_2yr	"Continuing breastfeeding at 2 year (20-23 months) - last-born under 2 years"
# 	
# nt_formula			"Child given infant formula in day/night before survey - last-born under 2 years"
# nt_milk				  "Child given other milk in day/night before survey- last-born under 2 years"
# nt_liquids			"Child given other liquids in day/night before survey- last-born under 2 years"
# nt_bbyfood			"Child given fortified baby food in day/night before survey- last-born under 2 years"
# nt_grains			  "Child given grains in day/night before survey- last-born under 2 years"
# nt_vita				  "Child given vitamin A rich food in day/night before survey- last-born under 2 years"
# nt_frtveg		  	"Child given other fruits or vegetables in day/night before survey- last-born under 2 years"
# nt_root				  "Child given roots or tubers in day/night before survey- last-born under 2 years"
# nt_nuts				  "Child given legumes or nuts in day/night before survey- last-born under 2 years"
# nt_meatfish			"Child given meat, fish, shellfish, or poultry in day/night before survey- last-born under 2 years"
# nt_eggs				  "Child given eggs in day/night before survey- last-born under 2 years"
# nt_dairy			  "Child given cheese, yogurt, or other milk products in day/night before survey- last-born under 2 years"
# nt_solids		  	"Child given any solid or semisolid food in day/night before survey- last-born under 2 years"
# 
# nt_fed_milk			"Child given milk or milk products- last-born 6-23 months"
# nt_mdd				  "Child with minimum dietary diversity- last-born 6-23 months"
# nt_mmf				  "Child with minimum meal frequency- last-born 6-23 months"
# nt_mad				  "Child with minimum acceptable diet- last-born 6-23 months"
# 
# nt_ch_micro_vaf		"Youngest children age 6-23 mos living with mother given Vit A rich food"
# nt_ch_micro_irf		"Youngest children age 6-23 mos living with mother given iron rich food"
# 
# ----------------------------------------------------------------------------*/

KRiycf <- KRiycf %>%
  mutate(wt = v005/1000000)

# *** Breastfeeding and complemenatry feeding ***
# 
# //currently breastfed
KRiycf <- KRiycf %>%
  mutate(nt_bf_curr =
           case_when(
              m4==95  ~ 1 ,
              m4 %in% c(93,94,98,99) ~ 0)) %>%
  set_value_labels(nt_bf_curr = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_bf_curr = "Currently breastfeeding - last-born under 2 years")

# //breastfeeding status
KRiycf <- KRiycf %>%
  mutate(water  = case_when(v409==1  ~ 1 , v409!=1 ~ 0)) %>%
  mutate(liquids= case_when(v409a==1 | v410==1 | v410a==1 | v412c==1 | v413==1 | v413a==1 | v413b==1 | v413c==1 | v413d==1  ~ 1 , 
                            v409a!=1 | v410!=1 | v410a!=1 | v412c!=1 | v413!=1 | v413a!=1 | v413b!=1 | v413c!=1 | v413d!=1 ~ 0)) %>%
  mutate(milk   = case_when(v411==1 | v411a==1 ~ 1 , v411!=1 | v411a!=1 ~ 0)) %>%
  mutate(solids = case_when(v414a==1 | v414b==1 | v414c==1 | v414d==1 | v414e==1 | v414f==1 | v414g==1 | v414h==1 | v414i==1 | 
                            v414j==1 | v414k==1 | v414l==1 | v414m==1 | v414n==1 | v414o==1 | v414p==1 | v414q==1 | v414r==1 | 
                            v414s==1 | v414t==1 | v414u==1 | v414v==1 | v414w==1 | v412a==1 | v412b==1 | m39a==1 ~ 1 ,
                            v414a!=1 | v414b!=1 | v414c!=1 | v414d!=1 | v414e!=1 | v414f!=1 | v414g!=1 | v414h!=1 | v414i!=1 | 
                            v414j!=1 | v414k!=1 | v414l!=1 | v414m!=1 | v414n!=1 | v414o!=1 | v414p!=1 | v414q!=1 | v414r!=1 | 
                            v414s!=1 | v414t!=1 | v414u!=1 | v414v!=1 | v414w!=1 | v412a!=1 | v412b!=1 | m39a!=1~ 0) ) %>%
  mutate(nt_bf_status = case_when(nt_bf_curr==0 ~ 0, solids==1 ~ 5, milk==1 ~ 4, liquids==1 ~3, water==1 ~2, TRUE~1 )) %>%
  set_value_labels(nt_bf_status = c("not bf"=0, "exclusively bf"=1, "bf & plain water"=2, "bf & non-milk liquids"=3, "bf & other milk"=4, "bf & complemenatry foods"=5 )) %>%
  set_variable_labels(nt_bf_status = "Breastfeeding status for last-born child under 2 years")

# //exclusively breastfed
KRiycf <- KRiycf %>%
  mutate(nt_ebf =
           case_when(
             age<6 & nt_bf_status==1  ~ 1 ,
             age<6 & nt_bf_status!=1 ~ 0)) %>%
  set_value_labels(nt_ebf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ebf = "Exclusively breastfed - last-born under 6 months")

# //predominantly breastfeeding
KRiycf <- KRiycf %>%
  mutate(nt_predo_bf =
           case_when(
             age<6 & nt_bf_status %in%c(1,2,3)  ~ 1 ,
             age<6 & nt_bf_status %in%c(0,4,5)  ~ 0)) %>%
  set_value_labels(nt_predo_bf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_predo_bf = "Predominantly breastfed - last-born under 6 months")

# //age appropriate breastfeeding
KRiycf <- KRiycf %>%
  mutate(nt_ageapp_bf =
           case_when(
             nt_ebf==1 | nt_bf_status==5 & inrange(age,6,23)  ~ 1,
             TRUE ~ 0 )) %>%
  set_value_labels(nt_ageapp_bf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ageapp_bf = "Age-appropriately breastfed - last-born under 2 years")

# //introduced to food
KRiycf <- KRiycf %>%
  mutate(nt_food_bf =
           case_when(
             solids==1 & inrange(age,6,8)  ~ 1,
             solids==0 & inrange(age,6,8) ~ 0 )) %>%
  set_value_labels(nt_food_bf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_food_bf = "Introduced to solid, semi-solid, or soft foods - last-born 6-8 months")

# //continuing breastfeeding at 1 year
KRiycf <- KRiycf %>%
  mutate(nt_bf_cont_1yr =
           case_when(
             m4==95 & inrange(age,12,15)  ~ 1,
             m4!=95 & inrange(age,12,15) ~ 0 )) %>%
  set_value_labels(nt_bf_cont_1yr = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_bf_cont_1yr = "Continuing breastfeeding at 1 year (12-15 months) - last-born under 2 years")
 
# //continuing breastfeeding at 2 years
KRiycf <- KRiycf %>%
  mutate(nt_bf_cont_2yr =
           case_when(
             m4==95 & inrange(age,20,23)  ~ 1,
             m4!=95 & inrange(age,20,23) ~ 0 )) %>%
  set_value_labels(nt_bf_cont_2yr = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_bf_cont_2yr = "Continuing breastfeeding at 2 year (20-23 months) - last-born under 2 years")

# *** Foods consumed ***
KRiycf <- KRiycf %>%
  # country specific foods. These can be added to the foods below based on the survey. See example for nt_root & nt_meatfish below
  mutate(food1  = case_when(v414a==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(food2  = case_when(v414b==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(food3  = case_when(v414c==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(food4  = case_when(v414d==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(nt_formula  = case_when(v411a==1  ~ 1 , v411a!=1~ 0)) %>% # Given formula
  mutate(nt_milk  = case_when(v411==1  ~ 1 , v411!=1~ 0)) %>% # Given other milk
  mutate(nt_liquids= case_when(v410==1 | v412c==1 | v413==1  ~ 1 , v410!=1 | v412c!=1 | v413!=1  ~ 0)) %>% # Given other liquids
  mutate(nt_bbyfood  = case_when(v412a==1  ~ 1 , v412a!=1~ 0)) %>% # Given fortified baby food
  mutate(nt_grains  = case_when(v412a==1 | v414e==1 ~ 1 , v412a!=1 | v414e!=1 ~ 0)) %>% # Given grains
  mutate(nt_vita = case_when(v414i==1 | v414j==1 | v414k==1 ~ 1 , v414i!=1 | v414j!=1 | v414k!=1 ~ 0)) %>% # Given Vit A rich foods
  mutate(nt_frtveg  = case_when(v414l==1  ~ 1 , v414l!=1~ 0)) %>% # Given other fruits or vegetables
  mutate(nt_root  = case_when(   # Given roots and tubers  
    (v000 == "UG7" & (v414f==1 | food1==1)) | (v000 != "UG7" & v414f==1) ~ 1, 
    (v000 == "UG7" & (v414f!=1 | food1!=1)) | (v000 != "UG7" & v414f!=1) ~ 0)) %>%
  mutate(nt_nuts  = case_when(v414o==1  ~ 1 , v414o!=1~ 0)) %>% # Given nuts or legumes
  mutate(nt_meatfish  = case_when(   # Given meat, fish, shellfish, or poultry  
    (v000 == "UG7" & (v414h==1 |v414m==1 |v414n==1| food2==1)) | (v000 != "UG7" & (v414h==1 | v414m==1 | v414n==1)) ~ 1, 
    (v000 == "UG7" & !(v414h==1 |v414m==1 |v414n==1| food2==1)) | (v000 != "UG7" & !(v414h==1 | v414m==1 | v414n==1)) ~ 0)) %>%
  mutate(nt_eggs  = case_when(v414g==1  ~ 1 , v414g!=1~ 0)) %>% # Given eggs
  mutate(nt_dairy  = case_when(v414p==1 | v414v==1 ~ 1 , v414p!=1 | v414v!=1 ~ 0)) %>% # Given dairy
  mutate(nt_solids = case_when( nt_bbyfood==1 | nt_grains==1 | nt_vita==1 | nt_frtveg==1 | nt_root==1 | nt_nuts==1 | nt_meatfish==1 | 
                                nt_eggs==1 | nt_dairy==1 | v414s==1 ~ 1 ,
                                nt_bbyfood!=1 | nt_grains!=1 | nt_vita!=1 | nt_frtveg!=1 | nt_root!=1 | nt_nuts!=1 | nt_meatfish!=1 | 
                                nt_eggs!=1 | nt_dairy!=1 | v414s!=1 ~ 0) ) %>%
  #add labels
  set_value_labels(nt_formula = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_formula = "Child given infant formula in day/night before survey - last-born under 2 years") %>%
  set_value_labels(nt_milk = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_milk = "Child given other milk in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_liquids = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_liquids = "Child given other liquids in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_bbyfood = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_bbyfood = "Child given fortified baby food in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_grains = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_grains = "Child given grains in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_vita = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_vita = "Child given vitamin A rich food in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_frtveg = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_frtveg = "Child given other fruits or vegetables in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_root = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_root = "Child given roots or tubers in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_nuts = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_nuts = "Child given legumes or nuts in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_meatfish = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_meatfish = "Child given meat, fish, shellfish, or poultry in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_eggs = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_eggs = "Child given eggs in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_dairy = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_dairy = "Child given cheese, yogurt, or other milk products in day/night before survey- last-born under 2 years") %>%
  set_value_labels(nt_solids = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_solids = "Child given any solid or semisolid food in day/night before survey- last-born under 2 years") 
  

# *** Minimum feeding indicators ***
# //Fed milk or milk products
KRiycf <- KRiycf %>%
  mutate(v469e_ = v469e) %>%
  replace_with_na(replace = list(v469e_ = c(8)))%>%
  mutate(v469f_ = v469f) %>%
  replace_with_na(replace = list(v469f_ = c(8)))%>%
  mutate(v469x_ = v469x) %>%
  replace_with_na(replace = list(v469x_ = c(8)))%>%
  mutate(v469e_ = coalesce(v469e_, 0),
         v469f_ = coalesce(v469f_, 0),
         v469x_ = coalesce(v469x_, 0))%>%
  mutate(totmilkf= v469e_ + v469f_ + v469x_) %>%
  mutate(nt_fed_milk  = 
           case_when(totmilkf>=2 | m4==95 & inrange(age,6,23) ~ 1 , 
                     totmilkf <2 | m4!=95 & inrange(age,6,23) ~ 0)) %>%
  set_value_labels(nt_fed_milk = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_fed_milk = "Child given milk or milk products- last-born 6-23 months")

# //Min dietary diversity
KRiycf <- KRiycf %>%
  # 1. breastmilk
  mutate(group1 = case_when(m4==95  ~ 1 ,  m4!=95 ~ 0)) %>% 
  #2. infant formula, milk other than breast milk, cheese or yogurt or other milk products
  mutate(group2 = case_when(nt_formula==1 | nt_milk==1 | nt_dairy==1  ~ 1 , nt_formula!=1 | nt_milk!=1 | nt_dairy!=1 ~ 0)) %>%
  #3. foods made from grains, roots, tubers, and bananas/plantains, including porridge and fortified baby food from grains
  mutate(group3  = case_when(nt_grains==1 | nt_root==1 | nt_bbyfood==1 ~ 1 , nt_grains!=1 | nt_root!=1 | nt_bbyfood!=1 ~ 0)) %>%
  #4. vitamin A-rich fruits and vegetables
  mutate(group4  = case_when(nt_vita==1  ~ 1 , nt_vita!=1 ~ 0)) %>%
  #5. other fruits and vegetables
  mutate(group5  = case_when(nt_frtveg==1 ~ 1 , nt_frtveg!=1~ 0)) %>% 
  #6. eggs
  mutate(group6  = case_when(nt_eggs==1 ~ 1 , nt_eggs!=1~ 0)) %>% 
  #7. meat, poultry, fish, and shellfish (and organ meats)
  mutate(group7  = case_when(nt_meatfish==1 ~ 1 , nt_meatfish!=1~ 0)) %>% 
  #8. legumes and nuts
  mutate(group8  = case_when(nt_nuts==1 ~ 1 , nt_nuts!=1~ 0)) %>% 
  #add the food groups
  mutate(foodsum  = group1+group2+group3+group4+group5+group6+group7+group8) %>% 
  mutate(nt_mdd  = case_when(inrange(age,6,23) & foodsum<5 ~ 0 , inrange(age,6,23) & foodsum>=5~ 1)) %>% 
  #older surveys are 4 out of 7 food groups so the foodsum would add group2-group8 and the recode the sum for 4+ as yes
  set_value_labels(nt_mdd = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mdd = "Child with minimum dietary diversity, 5 out of 8 food groups- last-born 6-23 months")

# //Min meal frequency
KRiycf <- KRiycf %>%
  mutate(feedings = case_when(m39>0 & m39<8 ~ totmilkf + m39 )) %>%
  mutate(nt_mmf  = 
           if_else(inrange(age,6,23) & (m4==95 & inrange(m39,2,7) & inrange(age,6,8)) | (m4==95 & inrange(m39,3,7) & inrange(age,9,23)) |
                  (m4!=95 & feedings>=4 & inrange(age,6,23)), 1, 0 )) 
KRiycf[["nt_mmf"]] <- ifelse(KRiycf[["age"]]<6, NA, KRiycf[["nt_mmf"]])
KRiycf <- KRiycf %>%
  set_value_labels(nt_mmf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mmf = "Child with minimum meal frequency- last-born 6-23 months")

# //Min acceptable diet
KRiycf <- KRiycf %>%
  mutate(foodsum2 = nt_grains+nt_root+nt_nuts+nt_meatfish+nt_vita+nt_frtveg+nt_eggs) %>%
  mutate(nt_mad  = 
           if_else((m4==95 & nt_mdd==1 & nt_mmf==1) | (m4!=95 & foodsum2>=4 & nt_mmf==1 & totmilkf>=2), 1, 0 )) 
KRiycf[["nt_mad"]] <- ifelse(KRiycf[["age"]]<6, NA, KRiycf[["nt_mad"]])
KRiycf <- KRiycf %>%
  set_value_labels(nt_mad = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_mad = "Child with minimum acceptable diet- last-born 6-23 months")

# //Consumed Vit A rich food
KRiycf <- KRiycf %>%
  mutate(nt_ch_micro_vaf  = 
           if_else(v414g==1 | v414h==1 | v414i==1 | v414j==1 | v414k==1 | v414m==1 | v414n==1, 1, 0 )) 
KRiycf[["nt_ch_micro_vaf"]] <- ifelse(KRiycf[["age"]]<6 | KRiycf[["age"]]>23  , NA, KRiycf[["nt_ch_micro_vaf"]])  
KRiycf <- KRiycf %>%
  set_value_labels(nt_ch_micro_vaf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_vaf = "Youngest children age 6-23 mos living with mother given Vit A rich food")

# //Consumed iron rich food
KRiycf <- KRiycf %>%
  mutate(nt_ch_micro_irf  = 
           if_else(v414g==1 | v414h==1 | v414m==1 | v414n==1, 1, 0 )) 
KRiycf[["nt_ch_micro_irf"]] <- ifelse(KRiycf[["age"]]<6 | KRiycf[["age"]]>23  , NA, KRiycf[["nt_ch_micro_irf"]])  
KRiycf <- KRiycf %>%
  set_value_labels(nt_ch_micro_irf = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_micro_irf = "Youngest children age 6-23 mos living with mother given iron rich food")
