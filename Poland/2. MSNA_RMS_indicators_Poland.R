
## Clear environment, if needed
rm(list = ls())
setwd("/Users/irmasirutyte/Desktop/MSNA Composite/MSNA_updated_poland")


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


sheet_names = excel_sheets("Data/xml-dataset-multi-sectoral-needs-assessment-poland-2023_23.11.2023.xlsx") # get sheet names
sheet_names # print sheet names

# Read Sheet 1
df_hh <- read_excel("Data/xml-dataset-multi-sectoral-needs-assessment-poland-2023_23.11.2023.xlsx", sheet = "HH")
# View(df_hh)
colnames(df_hh) <- gsub("/", "_", colnames(df_hh)) # Replace "/" with "_" in columns 

# Read Sheet 2
df_ind <- read_excel("Data/xml-dataset-multi-sectoral-needs-assessment-poland-2023_23.11.2023.xlsx", sheet = "people")
# View(df_ind)

colnames(df_ind) <- gsub("/", "_", colnames(df_ind))# Replace "/" with "_" in columns


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


class(df_ind$H4_SM_HLTH_ACC_BARRIER_wanted_to_wait_and_see_if_the_problem_go_better)


df_ind <- df_ind %>% # Those who were able to access healthcare
  mutate(health_access = case_when(
    H3_SS_HLTH_OBTAIN_CARE == "yes"  ~ 1,
    H3_SS_HLTH_OBTAIN_CARE == "DoNotKnow" ~ NA_real_,
    H3_SS_HLTH_OBTAIN_CARE == "PreferNotAnswer" ~ NA_real_,
    TRUE ~ 0)
  ) 

df_ind <- df_ind %>% # Those who needed and asked to access healthcare - remove those who didn't try to access healthcare for some reasons
  mutate(health_need = case_when(
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "wanted_to_wait_and_see_if_the_problem_go_better" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "do_not_trust_local_provider" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "fear_or_distrust_of_HW_EXAM_treatment" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "other" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" ~ 1,
    H1_SS_HLTH_PBLM == "DoNotKnow" ~ NA_real_,
    H1_SS_HLTH_PBLM == "PreferNotAnswer" ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(impact2_3_health = health_access / health_need) %>%
  mutate(impact2_3_health = labelled(impact2_3_health,
                                     labels = c (
                                       "Yes"= 1,
                                        "No"= 0
                                     ),
                                     label="Proportion of PoC with access to health services"))


table(df_ind$impact2_3_health)


round(prop.table(table(df_ind$impact2_3_health)), 3)


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
    (E1_SS_ATT_EDU == "enrolledandattended") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ 1,  # attending school in person
    (E1_SS_ATT_EDU == "enrolledanddropped") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ 0,  # dropped so not attending 
    (E1_SS_ATT_EDU == "no") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ 0,                  # not attending school at all  
    (E1_SS_ATT_EDU == "pnta") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 11) ~ NA_real_,
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

# Poland specific: 
# enrollment	enrolledandattended
# enrollment	enrolledanddropped
# enrollment	no
# enrollment	pnta
# enrollment	donotknow

df_ind <- df_ind %>%
  mutate(edu_primary = case_when(
    (E1_SS_ATT_EDU == "enrolledandattended") & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ 1,  # attending in person
    (E1_SS_ATT_EDU == "enrolledanddropped") & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ 0,  # dropped so not attending in person
     E1_SS_ATT_EDU == "no" & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ 0,     # not attending education at all  
    (E1_SS_ATT_EDU == "pnta") & (DR.11_NUM_AGE > 10 & DR.11_NUM_AGE < 16) ~ NA_real_,
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
# Proportion of population that feel safe walking alone in their neighborhood
# ------------------------------------------------------------------------------

# PRT06_SS_SAFETY_LVL	- How safe do you feel walking alone in your area/neighborhood after dark?

df_hh <- df_hh %>%
  mutate(impact3_3_safety_walking = case_when(
    PRT06_SS_SAFETY_LVL =="very_safe" | PRT06_SS_SAFETY_LVL == "fairly_safe" ~ 1,
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

round(prop.table(table(df_hh$impact3_3_safety_walking)), 2)


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




### ISSUES WITH POLAND DATASET - ALL VALUES FOR THIS COLUMN ARE MISSING CP1_SS_BTH_REG

## Children who have been registered with civil authorities
# 
# df_ind <- df_ind %>%
#   mutate(birthRegistered = case_when(
#     CP1_SS_BTH_REG == "yes" & DR.11_NUM_AGE < 5 ~ 1, # should we include those whose registration is in progress?
#     (CP1_SS_BTH_REG %in% c("no", "in_progress") & DR.11_NUM_AGE < 5) ~ 0,
#     TRUE ~ NA_real_)
#   ) %>%
#   mutate(outcome1_2_children_registered = labelled(birthRegistered,
#                                   labels=c(
#                                     'Yes'= 1,
#                                     'No'= 0
#                                   ),
#                                   label = "Children under 5 birth registered with civil authorities"))
# 
# 
# ## If the birth is registered or child has a birth certificate
# 
# df_ind <- df_ind %>%
#   mutate(outcome1_2_children_registered = case_when(
#     ((birthRegistered==1 ) & DR.11_NUM_AGE < 5) ~ 1, 
#     ((birthRegistered==0 ) & DR.11_NUM_AGE < 5) ~ 0)
#   ) %>%
#   mutate(outcome1_2_children_registered=labelled(outcome1_2_children_registered,
#                              labels=c(
#                                'Yes'= 1,
#                                'No'= 0
#                              ),
#                              label = "Proportion of children under 5 years of age whose births have been registered with a civil authority"))
# 
# 
# table(df_ind$outcome1_2_children_registered)
# 
# round(prop.table(table(df_ind$outcome1_2_children_registered)), 2)
# 
# write_xlsx(df_ind, "RMS/final_individual_indicators.xlsx", col_names=TRUE)

# ------------------------------------------------------------------------------
# 1.3 Core outcome indicator	
# Proportion of Persons of Concern with legally recognized identity documents or credentials
# ------------------------------------------------------------------------------

#	PRT01_SS_ID	- Does this person have an ID document (national ID, and/or passport and/or birth certificate)?

# Poland - specific responses: 
# cert	Birth certificate (age<18)
# idornpassport	ID (age 14+)/National passport (age 16+)
# biopassport	Non biometric passport for foreign travel
# nonbiopassport	Biometric passport for foreign travel
# none	None
# donotknow	Do not know
# pnta	Prefer not to answer
# other	Other


df_ind <- df_ind %>%
  mutate(outcome1_3_legal_documents = case_when(
    PRT01_SS_ID != "none" ~ 1,
    PRT01_SS_ID == "none"  ~ 0,
    PRT01_SS_ID == "pnta" ~ NA_real_,
    PRT01_SS_ID == "other" ~ NA_real_,
    PRT01_SS_ID == "donotknow" ~ NA_real_,
    TRUE ~ 1)
  ) %>%
  mutate(outcome1_3_legal_documents = labelled(outcome1_3_legal_documents,
                             labels = c(
                               'Yes'= 1,
                                'No'= 0
                             ),
                             label = "Proportion of Persons of Concern with legally recognized identity documents or credentials"))

table(df_ind$outcome1_3_legal_documents)

round(prop.table(table(df_ind$outcome1_3_legal_documents)), 3)


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


#### POLAND REMOVED THESE QUESTIONS

# df_hh <- df_hh %>%
#   mutate(outcome4_1_GBV = case_when(
#     GBV01a_SS_HLTH == "yes" |  GBV01b_SS_PSY == "yes" |  GBV01c_SS_SAFE == "yes" |  GBV01e_SS_LEGAL == "yes" ~ 1,
#     GBV01a_SS_HLTH == "PreferNotAnswer" & GBV01b_SS_PSY == "PreferNotAnswer" &  GBV01c_SS_SAFE == "PreferNotAnswer" &  GBV01e_SS_LEGAL == "PreferNotAnswer" ~ NA_real_,    
#     TRUE ~ 0)
#   ) %>%
#   mutate(outcome4_1_GBV = labelled(outcome4_1_GBV,
#                              labels=c(
#                                'Yes'= 1,
#                                "No"= 0
#                              ),
#                              label = "Proportion of PoC who know where to access available GBV services"
#   ))
# 
# table(df_hh$outcome4_1_GBV)
# 
# round(prop.table(table(df_hh$outcome4_1_GBV)), 2)
# 
# 
# write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)
# 

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

round(prop.table(table(df_hh$outcome13_1_bank_account)), 2)

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

round(prop.table(table(df_hh$outcome13_2_income)), 2)


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
    SE2_SS_WORK == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE3_SS_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE4_SS_FAM_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
   # SE5_SS_HELP_FAM_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,   # Poland doesn't have this question
    SE2_SS_WORK == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE3_SS_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE4_SS_FAM_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
   # SE5_SS_HELP_FAM_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,     # Poland doesn't have this question
    SE2_SS_WORK == "PreferNotAnswer" | SE3_SS_BUSINESS == "PreferNotAnswer" | SE4_SS_FAM_BUSINESS == "PreferNotAnswer" ~ NA_real_,
    TRUE ~ NA_real_
  )) %>%
  mutate(unemployed = case_when(
    employed == 0 & SE6_SS_TRY_FIND_JOB == "yes" & SE7_SS_START_WORK_IN_2_WKS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    employed == 1 & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    is.na(employed) | is.na(SE6_SS_TRY_FIND_JOB) | is.na(SE7_SS_START_WORK_IN_2_WKS) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(labour_force = case_when(
    (employed == 1 | unemployed == 1) & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    is.na(employed) | is.na(unemployed) ~ NA_real_,
    TRUE ~ 0
  ))

round(prop.table(table(df_ind$labour_force)), 2)

unemployed_sum <- sum(df_ind$unemployed, na.rm = TRUE)
labour_force_total <- sum(df_ind$labour_force, na.rm = TRUE)

df_ind <- df_ind %>%
  mutate(outcome13_3_unemployment = unemployed_sum / labour_force_total)

mean_outcome13_3_unemployment <- mean(df_ind$outcome13_3_unemployment, na.rm = TRUE)
print(mean_outcome13_3_unemployment)

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

round(prop.table(table(df_hh$outcome16_2_social_protection)), 2)


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
# SHL07_SM_LIV_COND_unclean_space	
# SHL07_SM_LIV_COND_inaccessible_by_transportation	
# SHL07_SM_LIV_COND_disposal_of_waste_system	
# SHL07_SM_LIV_COND_inaccessible_to_disabled	
# SHL07_SM_LIV_COND_insufficient_sleeping_materials	
# SHL07_SM_LIV_COND_dont_know	
# SHL07_SM_LIV_COND_prefer_not_to_say

# Poland specific option, but will be ignired in a
# not_enough_space	Do not have enough space


df_hh <- df_hh %>%
  
  mutate(crowding = DR8_NUM_HH_SIZE / SHL02_NUM_ROOMS
  ) %>%
  mutate(sufficient_dwel_1 =
           case_when( ## if crowding <= 3, not overcrowded 
           crowding <= 3 ~ 1, 
           TRUE ~ 0)
  )

round(prop.table(table(df_hh$sufficient_dwel_1)), 2)

df_hh %>% group_by(sufficient_dwel_1) %>% filter(!is.na(sufficient_dwel_1)) %>% 
  summarise(n = sum(Weight)) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

## classify as habitable when adequate shelter

df_hh <- df_hh %>%
  mutate(sufficient_dwel_2 = case_when(
    SHL07_SM_LIV_COND_no_issues == 1 |
      (SHL07_SM_LIV_COND_unclean_space == 1 | SHL07_SM_LIV_COND_inaccessible_by_transportation == 1) &
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_unclean_space == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 ~ 1,
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

round(prop.table(table(df_hh$outcome9_1_housing)), 2)

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
    H5.1_SS_HLTH_VACCINE_MEASLES == "no"  ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(outcome10_1_measles = labelled(outcome10_1_measles,
                                 labels = c(
                                   "Yes" = 1,
                                    "No" = 0
                                 ),
                                 label = "Proportion of children aged 9 months to five years who have received measles vaccination*"))



table(df_ind$outcome10_1_measles)

round(prop.table(table(df_ind$outcome10_1_measles)), 2)


### SAME LOGIC FOR POLIO 


df_ind <- df_ind %>%
  mutate(outcome10_1_polio = case_when(
    H6_SS_HLTH_VACCINE_POLIO == "1_dose" | H6_SS_HLTH_VACCINE_POLIO == "2_doses" | H6_SS_HLTH_VACCINE_POLIO == "3_doses" | H6_SS_HLTH_VACCINE_POLIO == "4_doses" ~ 1, 
    H6_SS_HLTH_VACCINE_POLIO == "0_none" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(outcome10_1_polio = labelled(outcome10_1_polio,
                                        labels = c(
                                          "Yes" = 1,
                                           "No" = 0
                                        ),
                                        label = "Proportion of children aged 9 months to five years who have received polio vaccination*"))



table(df_ind$outcome10_1_polio)

round(prop.table(table(df_ind$outcome10_1_polio)), 2)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# OTHER INDICATORS FOR FACTBOOK !!!
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# not all these nationalities included in Czech but they will be useful when copy-pasting for other countries

df_hh <- df_hh %>% 
  mutate(nationality = case_when(
    DR1_SM_NAT %in% c("bulgarian", "moldovan", "romanian", "russian") ~ "other",
    DR1_SM_NAT %in% c("moldovan ukrainian", "polish ukrainian", "russian ukrainian", "ukrainian bulgarian",
                      "ukrainian hungarian", "ukrainian moldovan", "ukrainian other", "ukrainian polish",
                      "ukrainian romanian", "ukrainian russian", "ukrainian russian polish", "ukrainian slovakian") ~ "ukrainian + other",
    DR1_SM_NAT == "ukrainian" ~ "ukrainian",
    TRUE ~ NA_character_
  ))


result_nationality <- df_hh %>% filter(!is.na(nationality)) %>% 
  group_by(nationality) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = prop.table(Count) * 100)

print(result_nationality)

# ------------------------------------------------------------------------------
#  RURAL vs URBAN  --- POLAND DOESN'T HAVE THIS QUESTION
# ------------------------------------------------------------------------------
# # Percentage of households living in rural areas vs urban areas
# column_name <- 'SHL01.2_SS_URB_RURAL'
# 
# # Exclude non-response and create a new dataset
# df_hh_rural <- df_hh[df_hh[[column_name]] != "do_not_know"]
# 
# # Use table() to tabulate response options
# tabulated_data <- table(df_hh_rural[[column_name]])
# 
# # Calculate the percentage of total for each category
# percentage_data <- prop.table(tabulated_data) * 100
# 
# # Combine the tabulated data and percentage data into a data frame
# result_rural_urban <- data.frame(
#   ResponseCategory = names(tabulated_data),
#   Count = as.numeric(tabulated_data),
#   Percentage = as.numeric(percentage_data)
# )
# 
# # Print or display the result
# print(result_rural_urban)

# ------------------------------------------------------------------------------
#-- Average Household Size (DR8_NUM_HH_SIZE) ----
# ------------------------------------------------------------------------------

# Assuming df_hh_filtered is your data frame
min_value <- min(df_hh$DR8_NUM_HH_SIZE)
max_value <- max(df_hh$DR8_NUM_HH_SIZE) # max is 11 so we are leaving it as it is

cat("Minimum value:", min_value, "\n")
cat("Maximum value:", max_value, "\n")

df_hh %>% filter(!is.na(DR8_NUM_HH_SIZE)) %>% summarise(mean = mean(DR8_NUM_HH_SIZE))

df_hh %>% filter(!is.na(DR8_NUM_HH_SIZE)) %>% summarise(mean = weighted.mean(DR8_NUM_HH_SIZE,Weight))

# ------------------------------------------------------------------------------
# % of children reported to attend early childhood education and care services in host country -- POLAND DOESN'T HAVE THIS QUESTION
# ------------------------------------------------------------------------------
# 2 to 5 years old 

# # Column name for education attendance
# column_name <- 'E5_SS_EARLY_EDU'
# 
# # Exclude "PreferNotAnswer" response and create a new dataset
# df_ind_early <- df_ind[!(df_ind[[column_name]] %in% c("prefer_not_to_answer", "dont_know","PreferNotAnswer","not_sure","DoNotKnow")), ]
# 
# # Use table() to tabulate response options
# tabulated_data <- table(df_ind_early[[column_name]])
# 
# # Calculate the percentage of total for each category
# percentage_data <- prop.table(tabulated_data) * 100
# 
# # Combine the tabulated data and percentage data into a data frame
# attending_early_education <- data.frame(
#   ResponseCategory = names(tabulated_data),
#   Count = as.numeric(tabulated_data),
#   Percentage = as.numeric(percentage_data)
# )
# 
# # Print or display the result
# print(attending_early_education)
# 
# # View the original df_ind dataset
# view(df_ind)

# ------------------------------------------------------------------------------
# % of school-aged children accessing Ukrainian distance learning
# ------------------------------------------------------------------------------

unique_responses <- unique(df_ind$E6_SS_DIST_LER)
 print(unique_responses)

  df_ind <- df_ind %>%
   mutate(distant_learning_grouped = case_when(
     E6_SS_DIST_LER == "no"  ~ 0,
     E6_SS_DIST_LER %in% paste0("grade_", 1:12) & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 16) ~ 1,
     E6_SS_DIST_LER == "PreferNotAnswer"  ~ NA_real_ ,
     TRUE ~ NA_real_
   )) %>%
   mutate(distant_learning_grouped = labelled(distant_learning_grouped,
                                             labels = c(
                                                "Yes" = 1,
                                                "No" = 0
                                              ),
                                              label = "Accessing Ukrainian distant learning"))


 round(prop.table(table(df_ind$distant_learning_grouped)), 2)

 # ------------------------------------------------------------------------------
 # attending both distant learning and school in host country
 # ------------------------------------------------------------------------------

 df_ind <- df_ind %>%
   mutate(attending_both_education = case_when(
     (distant_learning_grouped == 1 & E1_SS_ATT_EDU == "enrolledandattended") & (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 16) ~ 1,
     (DR.11_NUM_AGE > 5 & DR.11_NUM_AGE < 16) ~ 0,
     TRUE ~ NA_real_
   ))

 table(df_ind$attending_both_education)

 round(prop.table(table(df_ind$attending_both_education)), 2)

write_xlsx(df_ind, "check_education.xlsx", col_names = TRUE)

# ------------------------------------------------------------------------------
# PREFERRED INFORMATION CHANNELS TOP 3 - DOES NOT EXIST FOR POLAND
# ------------------------------------------------------------------------------
# AAP.3_SM_PRF_INFO_CHNL
# 
# 
# df_hh %>%
#   select(starts_with("AAP.3_SM_PRF_INFO_CHNL")) %>%
#   mutate(across(everything(), as.character)) %>%
#   pivot_longer(cols = everything(),
#                names_to = "variable",
#                values_to = "answer") %>%
#   group_by(variable) %>%
#   summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
#   arrange(desc(per))


# ------------------------------------------------------------------------------
# Preferred means of providing feedback to aid providers about the quality, quantity and approriateness of aid
# ------------------------------------------------------------------------------

df_hh_filtered_feedback <- filter(df_hh, AAP.5_SM_TOP_NEEDS_no_needs  != 1 & AAP.4_SM_PRF_FEEDBACK_prefer_not_to_answer!= 1 & AAP.4_SM_PRF_FEEDBACK_do_not_know !=1 )

df_hh_filtered_feedback %>%
  select(starts_with("AAP.4_SM_PRF_FEEDBACK")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "answer") %>%
  group_by(variable) %>%
  summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
  arrange(desc(per))

# ------------------------------------------------------------------------------
# Top three most commonly reported priority needs, by % of HHs per type of priority need reported
# ------------------------------------------------------------------------------

df_hh_filtered_needs <- filter(df_hh, AAP.5_SM_TOP_NEEDS_no_needs  != 1)

df_hh_filtered_needs %>%
  select(starts_with("AAP.5_SM_TOP_NEEDS")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "answer") %>%
  group_by(variable) %>%
  summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
  arrange(desc(per))


# ------------------------------------------------------------------------------
# % of respondents reporting awareness of protection services in the area they are residing - DOES NOT EXIST FOR POLAND
# ------------------------------------------------------------------------------
# 
# df_hh <- df_hh %>%
#   mutate(protection_services_awareness = case_when(
#     PRT03_SM_AVL_SRV == "none_of_the_above" | PRT03_SM_AVL_SRV == "dont_know" ~ 0,
#     PRT03_SM_AVL_SRV != "none_of_the_above" & PRT03_SM_AVL_SRV != "dont_know" ~ 1,
#     TRUE ~ NA_real_
#   ))
# 
# table(df_hh$protection_services_awareness)
# 
# round(prop.table(table(df_hh$protection_services_awareness)), 2)


#write_xlsx(df_hh, "check_awareness.xlsx", col_names = TRUE)

# ------------------------------------------------------------------------------
# Major barriers for accessing services on GBV - DOES NOT EXIST FOR POLAND
# ------------------------------------------------------------------------------

# GBV02_SM_GBV_BARR - multi-response question
# remove GBV02_SM_GBV_BARR_no_need_to_check
# 
# #df_hh_filtered_gbv <- filter(df_hh, GBV02_SM_GBV_BARR_no_need_to_check != 1)
# 
# df_hh %>%
#   select(starts_with("GBV02_SM_GBV_BARR")) %>%
#   mutate(across(everything(), as.character)) %>%
#   pivot_longer(cols = everything(),
#                names_to = "variable",
#                values_to = "answer") %>%
#   group_by(variable) %>%
#   summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
#   arrange(desc(per))

# ------------------------------------------------------------------------------
# Top 3 - % of HH members by main difficulty finding work in host country
# ------------------------------------------------------------------------------

# SE12_SM_EMP_BARR

df_hh_filtered_difficulty_work <- filter(df_ind, SE12_SM_EMP_BARR_none  != 1 & SE12_SM_EMP_BARR_prefer_not_to_answer != 1)


df_hh_filtered_difficulty_work %>%
  select(starts_with("SE12_SM_EMP_BARR")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "answer") %>%
  group_by(variable) %>%
  summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
  arrange(desc(per))


# ------------------------------------------------------------------------------
# % of HH members employed formally (with contract) 
# ------------------------------------------------------------------------------

# !!!!!!! CHECK IF OUT OF TOTAL HOUSEHOLDS OR THOSE EMPLOYED ??? 


column_name <- 'SE11_SS_CONTRACT'

# Exclude "PreferNotAnswer" or "dont_know" responses
df_ind_contract <- df_ind[!(df_ind[[column_name]] %in% c("prefer_not_to_answer", "dont_know")), ]

# Use table() to tabulate response options
tabulated_data <- table(df_ind_contract[[column_name]])

# Calculate the percentage of total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
work_contract <- data.frame(ResponseCategory = names(tabulated_data),
                            Count = as.numeric(tabulated_data),
                            Percentage = as.numeric(percentage_data))

# Print or display the result
print(work_contract)

# ------------------------------------------------------------------------------
# Top 3 - Main areas of support required for socio-economic inclusion
# ------------------------------------------------------------------------------

# SE1_SM_SUP_SRV - multi-response
# EXCLUDE prefer_not_to_answer AND none 

df_hh_filtered_economic_inclusion <- filter(df_hh, SE1_SM_SUP_SRV_none != 1 & SE1_SM_SUP_SRV_prefer_not_to_answer != 1 & SE1_SM_SUP_SRV_dont_know!= 1)

df_hh_filtered_economic_inclusion %>%
   select(starts_with("SE1_SM_SUP_SRV")) %>%
   mutate(across(everything(), as.character)) %>%
   pivot_longer(cols = everything(),
                names_to = "variable",
                values_to = "answer") %>%
   group_by(variable) %>%
   summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
   arrange(desc(per))


# ------------------------------------------------------------------------------
# Top 3 - % of HHs household members by self-reported barriers to accessing health care in the last 30 days
# ------------------------------------------------------------------------------

# H4_SM_HLTH_ACC_BARRIER - DOES NOT HAVE "DONT KNOW"
# H4_SM_HLTH_ACC_BARRIER_pnta remove 

df_hh_filtered_health_barrier <- filter(df_ind, H4_SM_HLTH_ACC_BARRIER_pnta != 1)

df_hh_filtered_health_barrier %>%
  select(starts_with("H4_SM_HLTH_ACC_BARRIER")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "answer") %>%
  group_by(variable) %>%
  summarise(per = round(sum(answer == "1", na.rm = TRUE) / sum(!is.na(answer)), 3)) %>%
  arrange(desc(per))


# ------------------------------------------------------------------------------
# % of HH members by highest education level achieved
# ------------------------------------------------------------------------------
# SE1_SS_EDU_LVL

# group master + phd + grand_phd

df_ind <- df_ind %>%
  mutate(education_level = case_when(
    SE1_SS_EDU_LVL %in% c("master", "phd", "grand_phd") ~ "Master and above",
    SE1_SS_EDU_LVL %in% c("no_edu", "pri_edu", "sec_edu") ~ "Lower education",
    TRUE ~ as.character(SE1_SS_EDU_LVL)
  ))

# Assuming your column name is "SE1_SS_EDU_LVL"
column_name <- "education_level"

# Exclude "PreferNotAnswer" responses
df_education <- df_ind[df_ind[[column_name]] != "PreferNotAnswer", ]

# Use table() to tabulate response options
tabulated_data <- table(df_education[[column_name]])

# Calculate the percentage of the total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
result_df <- data.frame(ResponseCategory = names(tabulated_data),
                        Count = as.numeric(tabulated_data),
                        Percentage = as.numeric(percentage_data))

# Print or display the result
print(result_df)

# ------------------------------------------------------------------------------
# % of HHs with children, who do not belong to the nuclear family/families in the HH
# ------------------------------------------------------------------------------

# Use this CP2_SS_BLG_NF (all minus "yes_nuclear_family" and exclude non-response )

# Use unique() on a specific column
unique_values_column1 <- unique(df_ind$CP2_SS_BLG_NF)
# Print the unique values from column1
print(unique_values_column1)

# create tag - yes_nuclear_family
df_ind <- df_ind %>%
  mutate(children_no_nuclear_family = case_when(
    CP2_SS_BLG_NF == "yes_nuclear_family" ~ 0, # is part of nuclear family
    CP2_SS_BLG_NF != "yes_nuclear_family" & !is.na(CP2_SS_BLG_NF) & CP2_SS_BLG_NF != "DoNotKnow" & CP2_SS_BLG_NF != "PreferNotAnswer" ~ 1,  # do not belong to the nuclear family (and not NA)
    TRUE ~ NA_real_
  ))

table(df_ind$children_no_nuclear_family)


# Exclude "PreferNotAnswer" response
df_ind_filtered <- df_ind[df_ind$children_no_nuclear_family != "DoNotKnow", ]

# Use table() to tabulate response options
tabulated_data <- table(df_ind_filtered$children_no_nuclear_family)

# Calculate the percentage of total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
result_children_no_family <- data.frame(
  ResponseCategory = names(tabulated_data),
  Count = as.numeric(tabulated_data),
  Percentage = as.numeric(percentage_data)
)

# Print or display the result
print(result_children_no_family)

# ------------------------------------------------------------------------------
# % of HHs who would report inappropriate behaviour from an aid worker
# ------------------------------------------------------------------------------

# PSEA3_SS_BHV_RPT
# yes
# no
# DoNotKnow
# PreferNotAnswer

column_name <- "PSEA3_SS_BHV_RPT"

# Exclude "PreferNotAnswer" response
df_hh_filtered <- df_hh[!(df_hh[[column_name]] %in% c("DoNotKnow", "PreferNotAnswer")), ]

# Use table() to tabulate response options
tabulated_data <- table(df_hh_filtered[[column_name]])

# Calculate the percentage of total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
result_aid_worker_inappropriate_behavior <- data.frame(
  ResponseCategory = names(tabulated_data),
  Count = as.numeric(tabulated_data),
  Percentage = as.numeric(percentage_data)
)

# Print or display the result
print(result_aid_worker_inappropriate_behavior)

# ------------------------------------------------------------------------------
# % % of HHs by accommodation arrangement 
# ------------------------------------------------------------------------------

# Accommodation type: SHL01_SS_ACCOM_TYP

# grouped 
# df_ind <- df_ind %>%
#   mutate(SHL01_SS_ACCOM_TYP = case_when(
#     SE1_SS_EDU_LVL %in% c("hotel_hostel", "workers_hostel", "other") ~ "Master and above",
#     TRUE ~ as.character(SHL01_SS_ACCOM_TYP)
#   ))

column_name <- "SHL01_SS_ACCOM_TYP"

# Exclude "PreferNotAnswer" response
df_hh_filtered <- df_hh[!(df_hh[[column_name]] %in% c("DoNotKnow", "prefer_not_to_answer")), ]

# Use table() to tabulate response options
tabulated_data <- table(df_hh_filtered[[column_name]])

# Calculate the percentage of total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
result_accommodation_type <- data.frame(
  ResponseCategory = names(tabulated_data),
  Count = as.numeric(tabulated_data),
  Percentage = as.numeric(percentage_data)
)

# Print or display the result
print(result_accommodation_type )

df_hh_filtered %>% group_by(SHL01_SS_ACCOM_TYP) %>% filter(!is.na(SHL01_SS_ACCOM_TYP)) %>% 
  summarise(n = sum(Weight)) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# ------------------------------------------------------------------------------
# % HHs with of youth (age 15-24 years) not in education, employment or training
# ------------------------------------------------------------------------------
# 16 to 24 inclusive - that's the data we have for employment


# Need to combine indicators on attendance, employment / unemployent and main activity

# SE8_SS_ACTIVITY	Which of the following best describes what (this person) is mainly doing at present?
# status_unempl	Unemployed/job-seeker
# studying	Studying
# professional_training	Professional training
# engaged_in_HH_resp	Engaged in household or family responsibilities including taking care of children and elderly
# retired	Retired or Pensioner
# long_term_ill_injury	With a long-term illness, injury or disability
# unpaid_volunteering	Doing unpaid volunteering, community or charity work

# Employed: employed != 1
# Not attending school: E1_SS_ATT_EDU != yes
# Not attending training: SE8_SS_ACTIVITY !=professional_training | SE8_SS_ACTIVITY !=professional_training = studying
# Education only in host country?? because now we are removing those who might be attending only online

df_ind <- df_ind %>%  
  mutate(inactive_youth = case_when(     
    (employed != 1 & E1_SS_ATT_EDU != "yes" & !(SE8_SS_ACTIVITY %in% c("professional_training", "studying")) & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE <= 24)) ~ 1,    
    (employed == 1 | E1_SS_ATT_EDU == "yes" | SE8_SS_ACTIVITY %in% c("professional_training", "studying")) & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE <= 24) ~ 0,      
    TRUE ~ NA_real_   ))

df_selected <- df_ind %>%
  select(DR.11_NUM_AGE, employed,E1_SS_ATT_EDU, SE8_SS_ACTIVITY,inactive_youth)

view(df_selected)

round(prop.table(table(df_ind$inactive_youth)), 2)

# write_xlsx(df_ind, "check_inactive_youth.xlsx", col_names = TRUE)

# ------------------------------------------------------------------------------
# % of HH paying rent without financial distress
# ------------------------------------------------------------------------------

# SHL04 - the result of response option paid_on_time

column_name <- "SHL04"

# Exclude "PreferNotAnswer" response
df_hh_filtered <- df_hh[!(df_hh[[column_name]] %in% c("not_applicable", "do_not_know","prefer_not_to_answer")), ]

# Use table() to tabulate response options
tabulated_data <- table(df_hh_filtered[[column_name]])

# Calculate the percentage of total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
result_paying_rent_no_stress <- data.frame(
  ResponseCategory = names(tabulated_data),
  Count = as.numeric(tabulated_data),
  Percentage = as.numeric(percentage_data)
)

# Print or display the result
print(result_paying_rent_no_stress)

# ------------------------------------------------------------------------------
# % of HHs under pressure to leave
# ------------------------------------------------------------------------------

# SHL06_SS_UND_PRESSURE - Are you under pressure to leave your accommodation?
# yes
# no
# PreferNotAnswer

column_name <- "SHL06_SS_UND_PRESSURE"

# Exclude "PreferNotAnswer" response
df_hh_filtered <- df_hh[!(df_hh[[column_name]] %in% c("not_applicable", "do_not_know","prefer_not_to_answer","PreferNotAnswer")), ]

# Use table() to tabulate response options
tabulated_data <- table(df_hh_filtered[[column_name]])

# Calculate the percentage of total for each category
percentage_data <- prop.table(tabulated_data) * 100

# Combine the tabulated data and percentage data into a data frame
result_pressure_to_leave <- data.frame(
  ResponseCategory = names(tabulated_data),
  Count = as.numeric(tabulated_data),
  Percentage = as.numeric(percentage_data)
)

# Print or display the result
print(result_pressure_to_leave)


## -----------------------------------------------------------------------------
# Age category adults only
## -----------------------------------------------------------------------------

#df_hh$DR7.3_NUM_RESP_AGE <- as.numeric(as.character(df_hh$DR7.3_NUM_RESP_AGE))

#df_hh$resp_age_cat <- cut(df_hh$DR7.3_NUM_RESP_AGE,
#                          breaks = c(17, 34, 60, Inf),
#                          labels = c("18-34", "35-60", "60+"))

#table(df_hh$resp_age_cat)


# Age category including children

df_ind$DR.11_NUM_AGE <- as.numeric(as.character(df_ind$DR.11_NUM_AGE)) 

df_ind$age_cat <- cut(df_ind$DR.11_NUM_AGE,
                    breaks = c(-1, 4, 17, 34, 60, Inf),
                    labels = c("0-4", "5-17", "18-34", "35-60", "60+"))


table(df_ind$age_cat)

# Population Pyramid

pop_pyramid <- table(df_ind$age_cat, df_ind$female)

df_ind %>% group_by(age_cat, female) %>%  count() %>%  ungroup() %>% mutate(per = n/sum(n)*100)


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


df_hh_export <- df_hh %>%
  select("_index", "DR7.2_SS_RESP_GEN", "DR7.3_NUM_RESP_AGE" ,'impact3_3_safety_walking',"outcome4_1_GBV","outcome13_1_bank_account","outcome13_2_income","outcome16_2_social_protection","crowding") 

write.xlsx(df_hh_export, "RMS/household_level_indicators_slovakia.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


df_ind_export <- df_ind %>%
  select("_parent_index", "_index", "DR.12_SS_GEN","DR.11_NUM_AGE","age_cat", "DR.13_SS_REL","impact2_3_health","impact3_2a_primary_edu_enrol_rate","impact3_2b_secondary_edu_enrol_rate",
         "outcome1_2_children_registered","outcome1_3_legal_documents","outcome13_3_unemployment", "outcome10_1_polio","outcome10_1_measles")

write.xlsx(df_ind_export, "RMS/individual_level_indicators_slovakia.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


# import the MSNA VAM dataset, select only the final indicators and merge to the above data

df_hh_vam <- read_excel("VAM/hh_indicators_slovakia.xlsx")
df_ind_vam <- read_excel("VAM/ind_indicators_slovakia.xlsx")


df_hh_full <- left_join(df_hh_export, df_hh_vam,
                  by = c("_index" = "_index"))

# write.xlsx(df_hh_full, "Combined/hh_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


df_ind_full <- left_join(df_ind_export, df_ind_vam,
                        by = c("_index" = "_index",
                               "_parent_index" = "_parent_index"))


view(df_ind_full)

# write.xlsx(df_ind_full, "Combined/ind_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


# ------------------------------------------------------------------------------
# Disability on household level
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# Households with children
# ------------------------------------------------------------------------------

# Create a dummy of individual level: 

df_ind_full$DR.11_NUM_AGE <- as.numeric(as.character(df_ind_full$DR.11_NUM_AGE))

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

# print(pivot_table_summary_child)




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


# write.xlsx(df_hh_full, "Combined/hh_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


df_ind_full <- df_ind_full %>%
  rename("_parent_index" = "parent_index")

# write.xlsx(df_ind_full, "Combined/ind_combined_indicators.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


# ------------------------------------------------------------------------------
# FINAL EXPORT
# ------------------------------------------------------------------------------

write.xlsx(df_hh_full, "Combined/household_combined_indicators_poland.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)


write.xlsx(df_ind_full, "Combined/individual_combined_indicators_poland.xlsx", sheetName = "Sheet1", colNames = TRUE, col_labels= TRUE)

