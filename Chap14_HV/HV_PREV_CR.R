# ******************************************************************************
# Program: 		HV_PREV_CR.R
# Purpose: 		Code for HIV prevalence among couples 
# Data inputs: 		merged AR and CR survey list
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 06, 2021 by Mahmoud Elkasabi
# Note:	This is using the merged file CRARmerge 
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# hv_couple_hiv_status	"HIV status for couples living in the same household both of whom had HIV test in survey"
# -----------------------------------------------------------------------------#

#HIV status for couples
CRARmerge <- CRARmerge %>%
  mutate(hv_couple_hiv_status = case_when(
    (w_hiv03!=1 & w_hiv03!=3) & (m_hiv03!=1 & m_hiv03!=3) ~ 1,
    (w_hiv03!=1 & w_hiv03!=3) & (m_hiv03==1 | m_hiv03==3) ~ 2,
    (w_hiv03==1 | w_hiv03==3) & (m_hiv03!=1 & m_hiv03!=3) ~ 3,
    (w_hiv03==1 | w_hiv03==3) & (m_hiv03==1 | m_hiv03==3) ~ 4,
    (!(w_hiv03==0 | w_hiv03==1 | w_hiv03==3)) & (m_hiv03==0 | m_hiv03==1 | m_hiv03==3) ~ 99),
    hv_couple_hiv_status = add_labels(hv_couple_hiv_status, labels = c("Both HIV negative"=1, "Man HIV positive, woman HIV negative"=2, "Woman HIV positive, man HIV negative"=3, "Both HIV positive"=4)),
    hv_couple_hiv_status = set_label(hv_couple_hiv_status, label = "HIV status for couples living in the same household both of whom had HIV test in survey"))%>%
  replace_with_na(replace = list(hv_couple_hiv_status = c(99)))

