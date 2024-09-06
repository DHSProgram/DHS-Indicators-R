# ******************************************************************************
# Program: 		ML_NETS_use.R - No changes in DHS8
# Purpose: 		Code for source of net use 
# Data inputs: 		PR survey list
# Data outputs:		coded variables and tables
# Author:		Cameron Taylor - translated to R by Mahmoud Elkasabi
# Date last modified: August 02, 2024 by Courtney Allen for DHS8 update
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ml_slept_net	"Slept under any mosquito net last night"
# ml_slept_itn	"Slept under an ITN last night"
# -----------------------------------------------------------------------------#

# Categorizing nets
PRdata <- PRdata %>%
  mutate(ml_netcat = case_when(
    hml12==0  ~ 0,
    hml12==1|hml12==2  ~ 1,
    hml12==3 ~ 2)) %>%
  set_value_labels(ml_netcat = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_netcat="Mosquito net categorization")

# Slept under any mosquito net last night
PRdata <- PRdata %>%
  mutate(ml_slept_net = case_when(
    ml_netcat==1|ml_netcat==2  ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_slept_net = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_slept_net =  "Slept under any mosquito net last night")

# Slept under an ITN last night
PRdata <- PRdata %>%
  mutate(ml_slept_itn = case_when(
    ml_netcat==1  ~ 1,
    TRUE ~ 0)) %>%
  set_value_labels(ml_slept_itn = c("Yes"= 1, "No" = 0)) %>%
  set_variable_labels(ml_slept_itn = "Slept under an ITN last night")