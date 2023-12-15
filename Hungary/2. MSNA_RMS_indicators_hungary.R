## Clear environment, if needed
rm(list = ls())
setwd("/Users/irmasirutyte/Desktop/MSNA Composite/MSNA_updated_Hungary")


## Libraries

library(robotoolbox) # This loads koboloadeR package
library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(remotes)
library(dm)
library(janitor)
library(dplyr)
library(DiagrammeR)
library(Hmisc)
library(xlsx)
library(writexl)
library(expss)



sheet_names = excel_sheets("Data/unhcr_msna_hh_datafile_fin_HU_01122023.xlsx") # get sheet names
sheet_names # print sheet names

df_hh <- read_excel("Data/unhcr_msna_hh_datafile_fin_HU_01122023.xlsx")

df_hh <-df_hh %>% 
  rename_all(~stringr::str_replace(.,"^@",""))

# Read Sheet 2
df_ind <- read_excel("Data/unhcr_msna_ind_datafile_fin_HU_01122023.xlsx")


df_ind <-df_ind %>% 
  rename_all(~stringr::str_replace(.,"index","_index"))


df_ind <-df_ind %>% 
  rename_all(~stringr::str_replace(.,"parent__index","_parent_index"))


# ------------------------------------------------------------------------------
# # 2.3 Core impact indicator
# Proportion of PoC with access to health services
# ------------------------------------------------------------------------------

# RMS	S7	S7: Access to health services
# RMS	HACC01	1. During the past 30 days, has ${name_individual} consulted a health practitioner, dentist, traditional healer, or pharmacist, or visited a health center?
# RMS	HACC02	2. For what reason(s) did ${name_individual} seek consultation?
# RMS	HACC02_other	If other please specify
# RMS	HACC03	3. During the past 30 days, has ${name_individual} needed health services that’s/he could not have access to?
# RMS	HACC04	4. Why has ${name_individual} been unable to access a medical treatment in the past 30 days?
# RMS	HACC04_other	If other please specify

# MSNA	Access	Access
# MSNA	H1_SS_HLTH_PBLM	In the last month (or since arrival in case less than 30 days since arrival), did this person in your household have a health problem and need to access health care?
# MSNA	H2_SS_HLTH_CHRONIC_ILL	Does the person have a chronic illness?
# MSNA	H3_SS_HLTH_OBTAIN_CARE	Was the person able to obtain the needed health care?
# MSNA	H4_SM_HLTH_ACC_BARRIER	What was the main reason this person was unable to access health care?
# MSNA	H4_TXT_HLTH_ACC_BARRIER_OTHER	Other barriers, please specify

# exclude
# H4_SM_HLTH_ACC_BARRIER_wanted_to_wait_and_see_if_the_problem_go_better
# H4_SM_HLTH_ACC_BARRIER_do_not_trust_local_provider
# H4_SM_HLTH_ACC_BARRIER_fear_or_distrust_of_HW_EXAM_treatment
# H4_SM_HLTH_ACC_BARRIER_other 


# Numerator: Population who have received the asked for health services in the previous 30 days
# Denominator: Total population who have asked for health services in the previous 30 days

df_ind <- df_ind %>% # Those who were able to access healthcare
  mutate(health_access = case_when(
    H3_SS_HLTH_OBTAIN_CARE == "yes"  ~ 1,
    H3_SS_HLTH_OBTAIN_CARE == "DoNotKnow" ~ NA_real_,
    H3_SS_HLTH_OBTAIN_CARE == "PreferNotAnswer" ~ NA_real_,
    TRUE ~ 0)
  ) 

df_ind <- df_ind %>% # Those who needed and asked to access healthcare
  mutate(health_need = case_when(
    H1_SS_HLTH_PBLM == "yes" ~ 1,
   (H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & (H4_SM_HLTH_ACC_BARRIER_wanted_to_wait_and_see_if_the_problem_go_better == "1" | H4_SM_HLTH_ACC_BARRIER_other =="1" )) ~ 0, 
    H1_SS_HLTH_PBLM == "DoNotKnow" ~ NA_real_,
    H1_SS_HLTH_PBLM == "PreferNotAnswer" ~ NA_real_,
    TRUE ~ 0)
  ) 

df_ind <- df_ind %>% ## Add up who needed services ( both who accessed and did not access)
  mutate(impact2_3_health = health_access / health_need 
  ) %>%
  mutate(impact2_3_health = case_when(
    impact2_3_health == 1 ~ 1, 
    impact2_3_health == 0.5 ~ 0,
    impact2_3_health == 0 ~ 0, 
    TRUE ~ NA_real_ )
  ) %>%
  mutate(impact2_3_health = labelled(impact2_3_health,
                                     labels =c(
                                       "Yes"= 1,
                                       "No"= 0
                                     ),
                                     label="Proportion of PoC with access to health services"))


table(df_ind$impact2_3_health)

write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 3.2a Core impact indicator	
# Proportion of Persons of Concern enrolled in primary education (NET ENROLLEMENT RATE) 
# ------------------------------------------------------------------------------

# Education	Education
# E1_SS_ATT_EDU	Is/was your child enrolled and attending formal education (school/kindergarten/nursery) in the Czech Republic in 2022/2023?
# E2_SM_RES_NO_EDU	What are the reasons your child does not attend school/kindergarten/nursery in the Czech Republic?
# E2_SM_RES_NO_EDU_OTH	If other: please specify
# E3_SS_ENROLL	Will you enroll this child in school/kindergarten/nursery in the Czech Republic for next year, 2023/2024?
# E4_SM_ATT_FUT	What kind of programs is this child attending/will be attending in school in 2023/2024?
# E4_SM_ATT_FUT_OTH	If other: please specify
# E5_SS_EARLY_EDU	Is your child attending early childhood education and care services in the Czech Republic?
# E6_SS_DIST_LER	"Was this child accessing Ukrainian distance learning (AOS-All Ukrainian School Online or other platforms) regularly in 2022/2023?

# This means they were doing some distance learning activities at least 4 days per week, for at least 3 hours per day e.g. listening to radio/TV broadcasts, textbook learning, online learning."
# E7_SS_CONT_DIST_LER	Will they enroll for Ukrainian distance learning in 2023/2024?

df_ind <- df_ind %>%
  mutate(edu_primary = case_when(
    (E1_SS_ATT_EDU == "yes") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ 1,  # attending school in person or remote  
    (E1_SS_ATT_EDU == "no") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ 0,                     # not attending school at all  
    (E1_SS_ATT_EDU == "PreferNotAnswer") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ NA_real_,
    TRUE ~ NA_real_)  # default case, added to handle other cases not covered above
  ) %>%
  mutate(age_primary = case_when(
    DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11 ~ 1,
    TRUE ~ NA_real_) 
  ) %>%
  mutate(impact3_2a_primary_edu_enrol_rate = sum(edu_primary, na.rm = TRUE) / sum(age_primary, na.rm = TRUE)) %>%
    mutate(impact3_2a_primary_edu_enrol_rate = round(impact3_2a_primary_edu_enrol_rate, 2)) %>%
    mutate(impact3_2a_primary_edu_enrol_rate = labelled(impact3_2a_primary_edu_enrol_rate,
                               labels = c("Yes" = 1, 
                                          "No" = 0),
                               label = "Proportion of persons of concern enrolled in primary education"))


table(df_ind$impact3_2a_primary_edu_enrol_rate)


write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names = TRUE)


# ------------------------------------------------------------------------------
# 3.2b Core impact indicator
# Proportion of Persons of Concern enrolled in secondary education (NET ENROLLEMENT RATE) 
# ------------------------------------------------------------------------------

# Education	Education
# E1_SS_ATT_EDU	Is/was your child enrolled and attending formal education (school/kindergarten/nursery) in the Czech Republic in 2022/2023?
# E2_SM_RES_NO_EDU	What are the reasons your child does not attend school/kindergarten/nursery in the Czech Republic?
# E2_SM_RES_NO_EDU_OTH	If other: please specify
# E3_SS_ENROLL	Will you enroll this child in school/kindergarten/nursery in the Czech Republic for next year, 2023/2024?
# E4_SM_ATT_FUT	What kind of programs is this child attending/will be attending in school in 2023/2024?
# E4_SM_ATT_FUT_OTH	If other: please specify
# E5_SS_EARLY_EDU	Is your child attending early childhood education and care services in the Czech Republic?
# E6_SS_DIST_LER	"Was this child accessing Ukrainian distance learning (AOS-All Ukrainian School Online or other platforms) regularly in 2022/2023?

# This means they were doing some distance learning activities at least 4 days per week, for at least 3 hours per day e.g. listening to radio/TV broadcasts, textbook learning, online learning."
# E7_SS_CONT_DIST_LER	Will they enrol for Ukrainian distance learning in 2023/2024?


df_ind <- df_ind %>%
  mutate(edu_primary = case_when(
    (E1_SS_ATT_EDU == "yes") & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ 1,  # attending in person
     E1_SS_ATT_EDU == "no" & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ 0,     # not attending education at all  
    (E1_SS_ATT_EDU == "PreferNotAnswer") & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ NA_real_,
    TRUE ~ NA_real_)  # default case, added to handle other cases not covered above
  ) %>%
  mutate(age_primary = case_when(
    DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16 ~ 1,
    TRUE ~ NA_real_)
  ) %>%
  mutate(impact3_2b_secondary_edu_enrol_rate = sum(edu_primary, na.rm = TRUE) / sum(age_primary, na.rm = TRUE)) %>%
  mutate(impact3_2b_secondary_edu_enrol_rate = round(impact3_2b_secondary_edu_enrol_rate, 2)) %>%
  mutate(impact3_2b_secondary_edu_enrol_rate = labelled(impact3_2b_secondary_edu_enrol_rate,
                               labels = c("Yes" = 1, 
                                          "No" = 0),
                               label = "Proportion of persons of concern enrolled in secondary education"))


table(df_ind$impact3_2b_secondary_edu_enrol_rate)

write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 3.3 Core impact indicator
# Proportion of population that feel safe walking alone in their neighbourhood
# ------------------------------------------------------------------------------

# PRT06_SS_SAFETY_LVL	- How safe do you feel walking alone in your area/neighbourhood after dark?

df_hh <- df_hh %>%
  mutate(impact3_3_safety_walking = case_when(
    PRT06_SS_SAFETY_LVL =="very_safe" | PRT06_SS_SAFETY_LVL =="fairly_safe" ~ 1,
    PRT06_SS_SAFETY_LVL == "bit_unsafe" | PRT06_SS_SAFETY_LVL == "very_unsafe" ~ 0 , 
    TRUE ~ NA_real_)
  ) %>% 
  mutate(impact3_3_safety_walking = labelled(impact3_3_safety_walking,
                                             labels = c(
                                               "Yes"= 1,
                                               "No"= 0
                                             ),
                                             label="Proportion of population that feel safe walking alone in their neighbourhood"))

table(df_hh$impact3_3_safety_walking)


write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 1.2 Core outcome indicator
# Proportion of children under 5 years of age whose births have been registered with a civil authority
# ------------------------------------------------------------------------------

#	CP1_SS_BTH_REG - 	Has this child's birth been registered with civil authorities? (In Ukraine, Czech Republic or other country)
#	PRT01_SS_ID	- Does this person have an ID document (national ID, and/or passport and/or birth certificate)?

## Children who have a birth certificate

# df_ind <- df_ind %>%
#  mutate(birthCertificate=case_when(
#    (PRT01_SS_ID =="yes" & DR.11_NUM_AGE < 5) ~ 1,
#    ((PRT01_SS_ID =="no" | PRT01_SS_ID =="has_id_no_pos") & DR.11_NUM_AGE < 5) ~ 0,
#    TRUE ~ NA_real_)
#  ) %>%
#  mutate(birthCertificate=labelled(birthCertificate,
#                                   labels=c(
#                                     'Yes'= 1,
#                                     'No'= 0
#                                   ),
#                                   label="Children under 5 with an id document"))


## Children who have been registered with civil authorities

df_ind <- df_ind %>%
  mutate(birthRegistered = case_when(
    CP1_SS_BTH_REG == "yes" & DR.11_NUM_AGE < 5 ~ 1, # should we include those whose registration is in progress?
    (CP1_SS_BTH_REG %in% c("no", "in_progress") & DR.11_NUM_AGE < 5) ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate(outcome1_2_children_registered = labelled(birthRegistered,
                                  labels=c(
                                    'Yes'= 1,
                                    'No'= 0
                                  ),
                                  label = "Children under 5 birth registered with civil authorities"))


## If the birth is registered or child has a birth certificate

df_ind <- df_ind %>%
  mutate(outcome1_2_children_registered = case_when(
    ((birthRegistered==1 ) & DR.11_NUM_AGE < 5) ~ 1, 
    ((birthRegistered==0 ) & DR.11_NUM_AGE < 5) ~ 0)
  ) %>%
  mutate(outcome1_2_children_registered=labelled(outcome1_2_children_registered,
                             labels=c(
                               'Yes'= 1,
                               'No'= 0
                             ),
                             label = "Proportion of children under 5 years of age whose births have been registered with a civil authority"))


table(df_ind$outcome1_2_children_registered)


write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 1.3 Core outcome indicator	
# Proportion of Persons of Concern with legally recognized identity documents or credentials
# ------------------------------------------------------------------------------

#	PRT01_SS_ID	- Does this person have an ID document (national ID, and/or passport and/or birth certificate)?

df_ind <- df_ind %>%
  mutate(outcome1_3_legal_documents = case_when(
    PRT01_SS_ID == "yes" ~ 1,
    PRT01_SS_ID == "no" | PRT01_SS_ID == "has_id_no_pos"  ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate(outcome1_3_legal_documents = labelled(outcome1_3_legal_documents,
                             labels = c(
                               'Yes'= 1,
                                'No'= 0
                             ),
                             label = "Proportion of Persons of Concern with legally recognized identity documents or credentials"))

table(df_ind$outcome1_3_legal_documents)


write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 4.1 Core outcome indicator	
# Proportion of Persons of Concern who know where to access available GBV services
# ------------------------------------------------------------------------------

# GBV01_NOTE	If someone in your community is subject to gender-based violence and asks for your help, would you be able to tell this person about the following services in this area?
# GBV01a_SS_HLTH	Health services
# GBV01b_SS_PSY	Psycho-social services
# GBV01c_SS_SAFE	Safety and security services? (police, safe shelters)
# GBV01d_SS_HELPLINE	Specific helpline to call and request a service? - DO NOT INCLUDE
# GBV01e_SS_LEGAL	Legal assistance

# In this case, "Do not know" and all the responses to all GBV related questions is "no" - then GBV indicator No 

df_hh <- df_hh %>%
  mutate(outcome4_1_GBV = case_when(
    GBV01a_SS_HLTH == "yes" |  GBV01b_SS_PSY == "yes" |  GBV01c_SS_SAFE == "yes" |  GBV01e_SS_LEGAL == "yes" ~ 1,
    GBV01a_SS_HLTH == "PreferNotAnswer" & GBV01b_SS_PSY == "PreferNotAnswer" &  GBV01c_SS_SAFE == "PreferNotAnswer" &  GBV01e_SS_LEGAL == "PreferNotAnswer" ~ NA_real_,   
    
    TRUE ~ 0)
  ) %>%
  mutate(outcome4_1_GBV = labelled(outcome4_1_GBV,
                             labels=c(
                               'Yes'= 1,
                               "No"= 0
                             ),
                             label = "Proportion of PoC who know where to access available GBV services"
  ))

table(df_hh$outcome4_1_GBV)


write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 13.1 Core outcome indicator
# Proportion of Persons of Concern with an account at a bank or other financial institution or with a mobile-money-service provider
# ------------------------------------------------------------------------------

## L12_SS_HV_BNK_ACC	- Do you currently have a bank account or account at a formal financial institution in the Czech Republic, either by yourself or with someone else?

df_hh <- df_hh %>%
  mutate( 
    outcome13_1_bank_account = case_when(
      L12_SS_HV_BNK_ACC == "yes" ~ 1,
      L12_SS_HV_BNK_ACC == "no" ~ 0,
      TRUE ~ NA_real_)
  ) %>%
  mutate(outcome13_1_bank_account = labelled(outcome13_1_bank_account,
                                labels = c(
                                  "Yes" = 1,
                                   "No" = 0
                                ),
                                label = "PoC with an account at a bank or other financial institution or with a mobile-money service provider"))

table(df_hh$outcome13_1_bank_account)

write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 13.2 Core outcome indicator	
# Proportion of Persons of Concern who self-report positive changes in their income compared to previous year
# ------------------------------------------------------------------------------

# L13_SS_AFF_GOODS	- Compared to your first months in the Czech Republic, do you think you can now afford more goods and services, the same, or fewer goods and services?
# Only calculate as positive if they responded 'more' 

df_hh <- df_hh %>%
  mutate(outcome13_2_income =case_when(
    L13_SS_AFF_GOODS == "more" ~ 1,
    L13_SS_AFF_GOODS == "same" | L13_SS_AFF_GOODS == "fewer" ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate(outcome13_2_income = labelled(outcome13_2_income,
                                labels = c(
                                  "Yes" = 1,
                                   "No" = 0
                                ),
                                label = "Proportion of PoC who self-report positive changes in their income compared to previous year"))


table(df_hh$outcome13_2_income)


write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)

# ------------------------------------------------------------------------------
# 13.3 Core outcome indicator	
# Proportion of Persons of Concern (working age) who are unemployed
# ------------------------------------------------------------------------------

## Numerator: Those of working age who were not in employment, looked for employment in the past 30 days and were available to take up employment
## Denominator: Those of working age in labour force

# SE2_SS_WORK	 - During the past 7 days, did this person work for someone else for pay, for one or more hours?
# SE3_SS_BUSINESS	 - During the past 7 days, did this person run or do any kind of business, farming, or other activity to generate income?
# SE4_SS_FAM_BUSINESS	-  During the past 7 days, did this person help in a family business or farm?
# SE5_SS_HELP_FAM_BUSINESS	- Even though this person did not work, during the past 7 days, did he/she have a business or a helper job in a family business/farm from which he/she was temporarily absent?
# SE6_SS_TRY_FIND_JOB	- During the last 30 days, did this person do anything to find a paid job or try to start a business?
# SE7_SS_START_WORK_IN_2_WKS- Could this person start working within the next two weeks if he/she was offered a job??

df_ind <- df_ind %>%
  mutate(employed = case_when(
    SE2_SS_WORK	== "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE3_SS_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE4_SS_FAM_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE5_SS_HELP_FAM_BUSINESS == "yes"& (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE2_SS_WORK	== "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE3_SS_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE4_SS_FAM_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE5_SS_HELP_FAM_BUSINESS == "no"& (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate(unemployed = case_when(
    (employed == 0 & SE6_SS_TRY_FIND_JOB == "yes" & SE7_SS_START_WORK_IN_2_WKS == "yes") & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate(labour_force = case_when(
    (employed==1 | unemployed==1) & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1)
  ) %>%
  mutate(outcome13_3_unemployment = unemployed/labour_force)


table(df_ind$outcome13_3_unemployment)

write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names=TRUE)

# ------------------------------------------------------------------------------
# 16.2 Core outcome indicator	
# Proportion of Persons of Concern covered by social protection floors/systems
# ------------------------------------------------------------------------------

# SE2.11b_SM_BEN_HST	Which social protection benefits do you receive from the Czech government?
# cash_benefits
# disability_grant
# unemployment_grant
# child_family_grant
# other_source - where to include "Other sources" ?
# dont_know
# prefer_not_to_answer

# If PoC has covered by at least one of the social protection floors/systems
library(openxlsx)
library(sjlabelled)

df_hh <- df_hh %>%
  mutate(outcome16_2_social_protection = case_when(
    SE2.11b_SM_BEN_HST %in% c("cash_benefits", "disability_grant", "unemployment_grant", "child_family_grant", "other_source") ~ 1,
    SE2.11b_SM_BEN_HST %in% c("prefer_not_to_answer", "dont_know") ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(outcome16_2_social_protection = labelled(outcome16_2_social_protection,
                                                  labels = c('Yes' = 1, 
                                                             'No' = 0),
                                                  label = "Proportion of Persons of Concern covered by social protection floors/systems"))


table(df_hh$outcome16_2_social_protection)


write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 9.1 Core outcome indicator - PROXY!!!! NOT THE ACTUAL INDICATOR
# Proportion of Persons of Concern living in habitable and affordable housing
# ------------------------------------------------------------------------------

# We only have the questions for calculating the crowding
# SHL02_NUM_ROOMS - number of rooms
# DR8_NUM_HH_SIZE - HH size
# SHL07_SM_LIV_COND - What issue, if any, are you facing in terms of living conditions in your accommodation?
# SHL07_SM_LIV_COND_no_issues	
# SHL07_SM_LIV_COND_unable_to_cook_store_food	
# SHL07_SM_LIV_COND_lack_of_showers_toilets	
# SHL07_SM_LIV_COND_lack_of_hot_water	
# SHL07_SM_LIV_COND_do_not_feel_protected	
# SHL07_SM_LIV_COND_insufficient_privacy	
# SHL07_SM_LIV_COND_unable_to_keep_warm_cool	
# SHL07_SM_LIV_COND_unclean_space	    ( # should be 1)
# SHL07_SM_LIV_COND_inaccessible_by_transportation	(# should be 1)
# SHL07_SM_LIV_COND_disposal_of_waste_system	
# SHL07_SM_LIV_COND_inaccessible_to_disabled	
# SHL07_SM_LIV_COND_insufficient_sleeping_materials	
# SHL07_SM_LIV_COND_dont_know	
# SHL07_SM_LIV_COND_prefer_not_to_say


df_hh <- df_hh %>%
  
  mutate(crowding = DR8_NUM_HH_SIZE / SHL02_NUM_ROOMS) %>%
  mutate(sufficient_dwel_1 =
           case_when(
             crowding <= 3 ~ 1,
             TRUE ~ 0
           )
  )

df_hh <- df_hh %>%
  mutate(sufficient_dwel_2 = case_when(
    SHL07_SM_LIV_COND_no_issues == 1 ~ 1,
    
    SHL07_SM_LIV_COND_unclean_space == 1 &
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 ~ 1,
    
    SHL07_SM_LIV_COND_inaccessible_by_transportation == 1 & 
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_unclean_space == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 ~ 1,
    
    SHL07_SM_LIV_COND_unclean_space == 1 & 
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 &
      # Additional conditions to make sure no other issues are selected
      SHL07_SM_LIV_COND_no_issues == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 ~ 1,
    
    SHL07_SM_LIV_COND_dont_know == 1 | SHL07_SM_LIV_COND_prefer_not_to_say == 1 ~ NA_real_,
    
    TRUE ~ 0
  ))

df_hh <- df_hh %>%
  
  mutate(outcome9_1_housing = case_when(
    sufficient_dwel_1 == 0 | sufficient_dwel_2 == 0  ~ 0, 
    sufficient_dwel_1 == 1 & sufficient_dwel_2 == 1  ~ 1, 
    TRUE ~ NA_real_ )
  ) %>%
  mutate(outcome9_1_housing = labelled(outcome9_1_housing,
                                       labels = c(
                                         "Yes" = 1,
                                         "No" = 0
                                       ),
                                       label = "Proportion of PoCs living in habitable and affordable housing"))


table(df_hh$outcome9_1_housing)



write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names = TRUE)



# ------------------------------------------------------------------------------
# 10.1 Core Outcome Indicator	
# Proportion of children aged 9 months to five years who have received measles vaccination
# ------------------------------------------------------------------------------

# H5.1_SS_HLTH_VACCINE_MEASLES	Has this child/children aged 9 months - 5 years ever received a measles-containing vaccine?
# H5.2_SS_HLTH_VACCINE_MEASLES_2DOSE	Did [child name] receive a second dose?
# H6_SS_HLTH_VACCINE_POLIO	How many polio vaccine doses has this child received in total?
# yesnoext	yes
# yesnoext	no
# yesnoext	DoNotKnow
# yesnoext	PreferNotAnswer

# Turn into numeric
# df_ind$H5.1_SS_HLTH_VACCINE_MEASLES <- labelled_chr2dbl(df_ind$H5.1_SS_HLTH_VACCINE_MEASLES)


df_ind <- df_ind %>%
  mutate(outcome10_1_measles = case_when(
    H5.1_SS_HLTH_VACCINE_MEASLES == "yes" ~ 1, 
    H5.1_SS_HLTH_VACCINE_MEASLES == "no"  | H5.1_SS_HLTH_VACCINE_MEASLES == "DoNotKnow" | H5.1_SS_HLTH_VACCINE_MEASLES == "PreferNotAnswer" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(outcome10_1_measles = labelled(outcome10_1_measles,
                                        labels = c(
                                          "Yes" = 1,
                                          "No" = 0
                                        ),
                                        label = "Proportion of children aged 9 months to five years who have received measles vaccination*"))



table(df_ind$outcome10_1_measles)


### SAME LOGIC FOR POLIO 


df_ind <- df_ind %>%
  mutate(outcome10_1_polio = case_when(
    H6_SS_HLTH_VACCINE_POLIO == "1_dose" | H6_SS_HLTH_VACCINE_POLIO == "2_doses" | H6_SS_HLTH_VACCINE_POLIO == "3_doses" | H6_SS_HLTH_VACCINE_POLIO == "4_doses" ~ 1, 
    H6_SS_HLTH_VACCINE_POLIO == "0_none"  | H6_SS_HLTH_VACCINE_POLIO == "DoNotKnow" | H6_SS_HLTH_VACCINE_POLIO == "PreferNotAnswer" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(outcome10_1_polio = labelled(outcome10_1_polio,
                                      labels = c(
                                        "Yes" = 1,
                                        "No" = 0
                                      ),
                                      label = "Proportion of children aged 9 months to five years who have received polio vaccination*"))



table(df_ind$outcome10_1_polio)


## -----------------------------------------------------------------------------
# Age category adults only

df_hh$DR7.3_NUM_RESP_AGE <- as.numeric(as.character(df_hh$DR7.3_NUM_RESP_AGE))

df_hh$resp_age_cat <- cut(df_hh$DR7.3_NUM_RESP_AGE,
                          breaks = c(17, 34, 60, Inf),
                          labels = c("18-34", "35-60", "60+"))

table(df_hh$resp_age_cat)


# Age category including children

df_ind$DR.11_NUM_AGE <- as.numeric(as.character(df_ind$DR.11_NUM_AGE)) 

df_ind$age_cat <- cut(df_ind$DR.11_NUM_AGE,
                    breaks = c(-1, 4, 17, 34, 60, Inf),
                    labels = c("0-4", "5-17", "18-34", "35-60", "60+"))


table(df_ind$age_cat)


# Select all the indicators in one data set:
# Individual level: 
# table(df_ind$impact2_3_health)
# table(df_ind$impact3_2a)
# table(df_ind$impact3_2b)
# table(df_ind$outcome1_2_children_registered)
# table(df_ind$outcome1_3_legal_documents)
# table(df_ind$outcome13_3_unemployment)
# Household level:
# table(df_hh$impact3_3_safety_walking)
# table(df_hh$outcome4_1_GBV)
# table(df_hh$outcome13_1_bank_account)
# table(df_hh$outcome13_2_income)
# table(df_hh$outcome16_2_social_protection)

view(df_hh)

df_hh_export <- df_hh %>%
  select("_index", "DR7.2_SS_RESP_GEN", "DR7.3_NUM_RESP_AGE" , "resp_age_cat",'impact3_3_safety_walking',"outcome4_1_GBV","outcome13_1_bank_account","outcome13_2_income","outcome16_2_social_protection", "outcome9_1_housing", "crowding") 

view(df_hh)

write.xlsx(df_hh_export, "RMS/household_level_indicators_hungary.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


df_ind_export <- df_ind %>%
  select("_parent_index", "_index", "DR.12_SS_GEN","DR.11_NUM_AGE","age_cat", "DR.13_SS_REL","impact2_3_health","impact3_2a_primary_edu_enrol_rate","impact3_2b_secondary_edu_enrol_rate",
         "outcome1_2_children_registered","outcome1_3_legal_documents","outcome13_3_unemployment","outcome10_1_polio", "outcome10_1_measles") 



write.xlsx(df_ind_export, "RMS/individual_level_indicators_hungary.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


# import the MSNA VAM dataset, select only the final indicators and merge to the above data

df_hh_vam <- read_excel("VAM/hh_indicators_hungary.xlsx")
df_ind_vam <- read_excel("VAM/ind_indicators_hungary.xlsx")


df_hh_full <- left_join(df_hh_export, df_hh_vam,
                  by = c("_index" = "_index"))

# write.xlsx(df_hh_full, "Combined/hh_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


df_ind_full <- left_join(df_ind_export, df_ind_vam,
                        by = c("_index" = "_index",
                               "_parent_index" = "_parent_index"))


view(df_ind_full)

# write.xlsx(df_ind_full, "Combined/ind_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


# ------------------------------------------------------------------------------
# ADD OTHER INDICATORS 
# ------------------------------------------------------------------------------

### 1. Disability on household level

df_ind_full$disability <- as.numeric(as.character(df_ind_full$disability))

df_ind_full <- df_ind_full %>%
 rename("parent_index" = "_parent_index")

# Create a pivot table-like summary using dplyr to sum the 'disability' variable by "_parent_index"

view(df_ind_full)
pivot_table_summary <- df_ind_full %>%
  group_by(parent_index) %>%
  summarise(sum_hh_disability = sum(disability))


print(pivot_table_summary)

pivot_table_summary <- pivot_table_summary %>%
  mutate(disability_dummy = ifelse(sum_hh_disability > 0, 1, 0)) %>%
  mutate(disability_dummy = factor(disability_dummy,
                                   labels = c('HH without disability', 
                                              'HH with disability'),
                                   levels = c(0, 1)))


print(pivot_table_summary)

### 2. Households with children

# Create a dummy of individual level: 

df_ind_full$DR.11_NUM_AGE <- as.numeric(as.character(df_ind_full$DR.11_NUM_AGE))

#  df_ind <- df_ind %>%
#  mutate(employed = case_when(
#  SE2_SS_WORK	== "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,


df_ind_full <- df_ind_full %>%
    mutate(tag_child = case_when(
     DR.11_NUM_AGE < 18 ~ 1, 
     TRUE ~ NA_real_))

     
table(df_ind_full$tag_child)

df_ind_full$tag_child <- as.numeric(as.character(df_ind_full$tag_child))

     
pivot_table_summary_child <- df_ind_full %>%
  group_by(parent_index) %>%
  summarise(sum_hh_children = sum(tag_child, na.rm = TRUE))

print(pivot_table_summary_child)

pivot_table_summary_child <- pivot_table_summary_child %>%
  mutate(child_dummy = ifelse(sum_hh_children > 0, 1, 0)) %>%
  mutate(child_dummy = factor(child_dummy,
                              labels = c('HH without children', 
                                              'HH with children'),
                              levels = c(0, 1)))

print(pivot_table_summary_child)




# df_ind_full <- left_join(df_ind_full, pivot_table_summary,
#                         by = c("parent_index" = "parent_index"))

# df_ind_full <- left_join(df_ind_full, pivot_table_summary_child,
#                          by = c("parent_index" = "parent_index"))


pivot_table_summary <- pivot_table_summary %>%
  rename("_index" = "parent_index")

pivot_table_summary_child <- pivot_table_summary_child %>%
  rename("_index" = "parent_index")

df_hh_full <- left_join(df_hh_full, pivot_table_summary,
                         by = c("_index" = "_index"))

df_hh_full <- left_join(df_hh_full, pivot_table_summary_child,
                        by = c("_index" = "_index"))

view(df_hh_full)


#write.xlsx(df_hh_full, "Combined/hh_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


 df_ind_full <- df_ind_full %>%
  rename("_parent_index" = "parent_index")


#write.xlsx(df_ind_full, "Combined/ind_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)

# ------------------------------------------------------------------------------
# FINAL EXPORT
# ------------------------------------------------------------------------------

write.xlsx(df_hh_full, "Combined/household_combined_indicators_hungary.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


write.xlsx(df_ind_full, "Combined/individual_combined_indicators_hungary.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


# ------------------------------------------------------------------------------
# Connect the two datasets / indicators 


df_hh_full_table <- df_hh_full %>% 
  cross_cpct(
    cell_vars = list(impact3_3_safety_walking, outcome4_1_GBV, outcome13_1_bank_account, outcome13_2_income, outcome16_2_social_protection),
    col_vars = list(total(), DR7.2_SS_RESP_GEN, resp_age_cat, disability_dummy)
  ) %>%
  mutate_if(is.numeric, round, digits = 2)


# Individual level: 
# table(df_ind$impact2_3_health)
# table(df_ind$impact3_2a)
# table(df_ind$impact3_2b)
# table(df_ind$outcome1_2_children_registered)
# table(df_ind$outcome1_3_legal_documents)
# table(df_ind$outcome13_3_unemployment)


write.xlsx(df_hh_full_table, "Combined/hh_table.xlsx", colNames = TRUE, rowNames = TRUE)


df_ind_full_table <- df_ind_full %>% 
  cross_cpct(
    cell_vars = list(impact2_3_health, impact3_2a_primary_edu_enrol_rate, impact3_2b_secondary_edu_enrol_rate, outcome1_2_children_registered, outcome1_3_legal_documents, outcome13_3_unemployment),
    col_vars = list(total(), disability, age_cat)
    ) %>% 
  mutate_if(is.numeric, round, digits = 2)


write.xlsx(df_ind_full_table, "Combined/ind_table.xlsx", colNames = TRUE, rowNames =TRUE)
  

      
  