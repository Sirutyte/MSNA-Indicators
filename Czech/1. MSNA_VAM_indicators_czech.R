## Clear environment, if needed
rm(list = ls())
setwd("/Users/irmasirutyte/Desktop/MSNA Composite/MSNA_updated_czech")


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


sheet_names = excel_sheets("Data/2023_Czech_Republic_Multi-Sector_Needs_Assessment.xlsx") # get sheet names
sheet_names # print sheet names

# Read Sheet 1
df_hh <- read_excel("Data/2023_Czech_Republic_Multi-Sector_Needs_Assessment.xlsx", sheet = "2023 Czech Republic Multi-Se...")
View(df_hh)

# Read Sheet 2
df_ind <- read_excel("Data/2023_Czech_Republic_Multi-Sector_Needs_Assessment.xlsx", sheet = "Info")


# ------------------------------------------------------------------------------
# DISABILITY
# ------------------------------------------------------------------------------

# WG.1.1_SS_DIFF_SEE
# WG.1.2_SS_DIFF_HEAR
# WG.1.3_SS_DIFF_WALK
# WG.1.4_SS_DIFF_REM
# WG.1.5_SS_DIFF_DRESS
# WG.1.6_SS_DIFF_COMM

df_ind <-  df_ind %>%
  mutate( # disability identifier variables according to Washington Group standards
    disaux1_34 = WG.1.1_SS_DIFF_SEE %in% c("lot_difficulty","cannot_all"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_34 = WG.1.2_SS_DIFF_HEAR %in% c("lot_difficulty","cannot_all"),
    disaux3_34 = WG.1.3_SS_DIFF_WALK %in% c("lot_difficulty","cannot_all"),
    disaux4_34 = WG.1.4_SS_DIFF_REM %in% c("lot_difficulty","cannot_all"),
    disaux5_34 = WG.1.5_SS_DIFF_DRESS %in% c("lot_difficulty","cannot_all"),
    disaux6_34 = WG.1.6_SS_DIFF_COMM %in% c("lot_difficulty","cannot_all")
  ) %>%
  mutate(
    disSum34 = rowSums(select(., disaux1_34, disaux2_34 , disaux3_34 , disaux4_34 , disaux5_34 , disaux6_34)) # count number of TRUE indicator variables over 6 domains
    
  ) %>%
  mutate(
    DISABILITY3 = case_when( # : the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum34 >= 1 ~ 1,
      disSum34 == 0 & (!(WG.1.1_SS_DIFF_SEE %in% c("refused","DoNotKnow") & WG.1.2_SS_DIFF_HEAR %in% c("refused","DoNotKnow") & WG.1.3_SS_DIFF_WALK %in% c("refused","DoNotKnow") & WG.1.4_SS_DIFF_REM %in% c("refused","DoNotKnow") & WG.1.5_SS_DIFF_DRESS %in% c("refused","DoNotKnow") & WG.1.6_SS_DIFF_COMM %in% c("refused","DoNotKnow"))) ~ 0,
      WG.1.1_SS_DIFF_SEE %in% c("refused","DoNotKnow") & WG.1.2_SS_DIFF_HEAR %in% c("refused","DoNotKnow") & WG.1.3_SS_DIFF_WALK %in% c("refused","DoNotKnow") & WG.1.4_SS_DIFF_REM %in% c("refused","DoNotKnow") & WG.1.5_SS_DIFF_DRESS %in% c("refused","DoNotKnow") & WG.1.6_SS_DIFF_COMM %in% c("refused","DoNotKnow") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY3 = labelled(DISABILITY3,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 3"))

### Calculate having at least one disability identifier among 4 categories 

df_ind <- df_ind %>%
  mutate(disability = case_when(DISABILITY3 == 1 ~ 1, 
                           TRUE ~ 0)
  ) %>%
  mutate(disability = labelled(disability,
                          labels = c(
                            "Without disability" = 0,
                            "With disability" = 1)
  ))
table(df_ind$disability)

round(prop.table(table(df_ind$disability)), 2)


# -----------------------------------------------------------------------------
# FOOD CONSUMPTION SCORE
# -----------------------------------------------------------------------------

# FS.1.1_NUM_FOOD
# FS.1.2_NUM_PULSES
# FS.1.3_NUM_DAIRY
# FS.1.4_NUM_MEAT
# FS.1.5_NUM_VEG
# FS.1.6_NUM_FRUITS
# FS.1.7_NUM_OIL
# FS.1.8_NUM_SUGAR
# FS.1._NUM_SPICES

#assign variable and value labels
#variable labels
var_label(df_hh$FS.1.1_NUM_FOOD) <- "Cereals, grains, roots and tubers, such as:"
var_label(df_hh$FS.1.2_NUM_PULSES) <- "Pulses/ legumes / nuts, such as:"
var_label(df_hh$FS.1.3_NUM_DAIRY) <- "Milk and other dairy products, such as:"
var_label(df_hh$FS.1.4_NUM_MEAT) <- "Meat, fish and eggs, such as:"
var_label(df_hh$FS.1.5_NUM_VEG) <- "Vegetables and leaves, such as:"
var_label(df_hh$FS.1.6_NUM_FRUITS) <- "Fruits, such as:"
var_label(df_hh$FS.1.7_NUM_OIL) <- "Oil/fat/butter, such as:"
var_label(df_hh$FS.1.8_NUM_SUGAR) <- "Sugar, or sweet, such as:"
var_label(df_hh$FS.1._NUM_SPICES) <- "Condiments/Spices, such as:"

df_hh <- df_hh %>%
  mutate(across(c(FS.1.1_NUM_FOOD, FS.1.2_NUM_PULSES, FS.1.3_NUM_DAIRY,FS.1.4_NUM_MEAT, FS.1.5_NUM_VEG, FS.1.6_NUM_FRUITS, FS.1.7_NUM_OIL, FS.1.8_NUM_SUGAR,FS.1._NUM_SPICES), ~ replace(., . %in% c(9999, 8888), NA)))

# calculate FCS
df_hh <- df_hh %>% mutate(FCS = (2 * FS.1.1_NUM_FOOD) +(3 * FS.1.2_NUM_PULSES) +(4*FS.1.4_NUM_MEAT) +(4*FS.1.3_NUM_DAIRY) + FS.1.5_NUM_VEG  + FS.1.6_NUM_FRUITS +(0.5*FS.1.7_NUM_OIL) +(0.5*FS.1.8_NUM_SUGAR))
var_label(df_hh$FCS) <- "Food Consumption Score"

#create FCG groups based on 21/25 or 28/42 thresholds

#Use this when analyzing a country with low consumption of sugar and oil - thresholds 21-35
df_hh <- df_hh %>% mutate(FCSCat21 = case_when(
  FCS <= 21 ~ 1, between(FCS, 21.5, 35) ~ 2, FCS > 35 ~ 3))

val_lab(df_hh$FCSCat21) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(df_hh$FCSCat21) <- "FCS Categories"

# Important note: pay attention to the threshold used by your CO when selecting the syntax (21 cat. vs 28 cat.)
# Use this when analyzing a country with high consumption of sugar and oil – thresholds 28-42

df_hh <- df_hh %>% mutate(FCSCat28 = case_when(
  FCS <= 28 ~ 1, between(FCS, 28.5, 42) ~ 2, FCS > 42 ~ 3))

val_lab(df_hh$FCSCat28) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(df_hh$FCSCat28) <- "FCS Categories"

round(prop.table(table(df_hh$FCSCat28)), 2)


# -----------------------------------------------------------------------------
# LIVELIHOOD COPING STRATEGIES INDEX
# -----------------------------------------------------------------------------

# variable labels 

#stress
var_label(df_hh$L9_SS_MIGR) <- "Entire household migrated/displaced due to a lack of resources to cover basic needs"
var_label(df_hh$L1_SS_BASIC) <- "Spent savings to meet essential needs"
var_label(df_hh$L2_SS_ASSETS) <- "Sell household assets/goods (radio, furniture, television, jewellery etc.) to meet basic needs"
var_label(df_hh$L3_SS_FOOD_CREDIT) <- "Purchase food on credit or borrowed food due to a lack of resources to cover basic needs"

#crisis
var_label(df_hh$L4_SS_SELL_ASSETS) <- "Sell productive assets or means of transport (sewing machine, bicycle, car, etc.) due to a lack of resources to cover basic needs"
var_label(df_hh$L7_SS_OUT_SCH) <- "Withdrew school-aged children from school because of a lack of food or money to buy food"
var_label(df_hh$L5_SS_REDUCED_HLTH_EXP) <- "Reduce essential health expenditures (including drugs) due to a lack of resources to cover basic needs"
var_label(df_hh$L6_SS_REDUCED_EDU_EXP) <- "Reduce essential education expenditures due to a lack of resources to cover basic need"

#emergency
var_label(df_hh$L8_SS_SELL_HSE_LND) <- "Sell house or land (including inside Ukraine) to cover basic needs"
var_label(df_hh$L11_SS_DEGR_INCM) <- "Use degrading sources of income, illegal work, or high-risk jobs to cover basic needs"
var_label(df_hh$L10_SS_CHL_LAB) <- "Involve school-aged children in income generation to meet basic needs"

view(df_hh)

df_hh <- df_hh %>%
  mutate(across(c(L9_SS_MIGR, L1_SS_BASIC, L2_SS_ASSETS, L3_SS_FOOD_CREDIT, L4_SS_SELL_ASSETS, L7_SS_OUT_SCH, L5_SS_REDUCED_HLTH_EXP, L6_SS_REDUCED_EDU_EXP, L8_SS_SELL_HSE_LND, L11_SS_DEGR_INCM, L10_SS_CHL_LAB), ~
                  case_when(
                    . == "no_not_needed" ~ 10,
                    . == "no_already_done" ~ 20,
                    . == "yes" ~ 30,
                    . == "not_applicable" ~ 999,
                    . == "prefer_not_to_answer" ~ 999,
                    . == "dont_know" ~ 999,
                    TRUE ~ as.numeric(.)
                  )
  ))  



# Display columns
selected_columns <- df_hh %>%
  select(L9_SS_MIGR, L1_SS_BASIC, L2_SS_ASSETS, L3_SS_FOOD_CREDIT)

# Print the selected columns
print(selected_columns)

#create a variable to specify if the household used any of the strategies by severity

#stress
df_hh <- df_hh %>% mutate(stress_coping_EN = case_when(
  L9_SS_MIGR== 20 | L9_SS_MIGR== 30 ~ 1,
  L1_SS_BASIC == 20 | L1_SS_BASIC == 30 ~ 1,
  L2_SS_ASSETS == 20 | L2_SS_ASSETS == 30 ~1,
  L3_SS_FOOD_CREDIT == 20 | L3_SS_FOOD_CREDIT == 30 ~ 1,
  TRUE ~ 0))
var_label(df_hh$stress_coping_EN) <- "Did the HH engage in stress coping strategies"

#Crisis
df_hh <- df_hh %>% mutate(crisis_coping_EN = case_when(
  L4_SS_SELL_ASSETS == 20 |  L4_SS_SELL_ASSETS == 30 ~ 1,
  L7_SS_OUT_SCH == 20 | L7_SS_OUT_SCH == 30 ~ 1,
  L5_SS_REDUCED_HLTH_EXP == 20 | L5_SS_REDUCED_HLTH_EXP == 30 ~ 1,
  L6_SS_REDUCED_EDU_EXP == 20 | L6_SS_REDUCED_EDU_EXP == 30 ~ 1,
  TRUE ~ 0))
var_label(df_hh$crisis_coping_EN) <- "Did the HH engage in crisis coping strategies"

#Emergency
df_hh <- df_hh %>% mutate(emergency_coping_EN = case_when(
  L8_SS_SELL_HSE_LND == 20 |  L8_SS_SELL_HSE_LND == 30 ~ 1,
  L11_SS_DEGR_INCM == 20 | L11_SS_DEGR_INCM == 30 ~ 1,
  L10_SS_CHL_LAB == 20 | L10_SS_CHL_LAB == 30 ~ 1,
  TRUE ~ 0))
var_label(df_hh$emergency_coping_EN) <- "Did the HH engage in emergency coping strategies"


#calculate Max_coping_behaviour
df_hh <- df_hh %>% mutate(Max_coping_behaviourEN = case_when(
  emergency_coping_EN == 1 ~ 4,
  crisis_coping_EN == 1 ~ 3,
  stress_coping_EN == 1 ~ 2,
  TRUE ~ 1))
var_label(df_hh$Max_coping_behaviourEN) <- "Summary of asset depletion"
val_lab(df_hh$Max_coping_behaviourEN) = num_lab("
             1 HH not adopting coping strategies
             2 Stress coping strategies
             3 Crisis coping strategies
             4 Emergencies coping strategies
")

round(prop.table(table(df_hh$Max_coping_behaviourEN)), 2)

# -----------------------------------------------------------------------------
# REDUCED COPING STRATEGIES INDEX
# -----------------------------------------------------------------------------

# FS.3.1_NUM_COPE - less preferred food
# FS.3.2_NUM_BURROW - borrow
# FS.3.3_NUM_LIM - limit portion size
# FS.3.4_NUM_LACK - restrict consumption by adults in order for small children to eat
# FS.3.5_NUM_REDUCED - reduce number of meals eaten

# assign variable and value labels
var_label(df_hh$FS.3.1_NUM_COPE) <-  "Rely on less preferred and less expensive food in the past 7 days"
var_label(df_hh$FS.3.2_NUM_BURROW) <- "Borrow food or rely on help from a relative or friend in the past 7 days"
var_label(df_hh$FS.3.5_NUM_REDUCED) <-  "Reduce number of meals eaten in a day in the past 7 days"
var_label(df_hh$FS.3.3_NUM_LIM) <- "Limit portion size of meals at meal times in the past 7 days"
var_label(df_hh$FS.3.4_NUM_LACK) <-  "Restrict consumption by adults in order for small children to eat in the past 7 days"

# Replace multiple specific numeric values (e.g., 999 and 888) with NA in multiple columns
df_hh <- df_hh %>%
  mutate(across(c(FS.3.1_NUM_COPE, FS.3.2_NUM_BURROW, FS.3.5_NUM_REDUCED,FS.3.3_NUM_LIM, FS.3.4_NUM_LACK), ~ replace(., . %in% c(9999, 8888), NA)))


# Display columns 
selected_columns <- df_hh %>%
  select(FS.3.1_NUM_COPE, FS.3.2_NUM_BURROW, FS.3.5_NUM_REDUCED, FS.3.3_NUM_LIM,FS.3.4_NUM_LACK)

# Print the selected columns
print(selected_columns)


# calculate reduced Coping Strategy Index (rCSI)
df_hh <- df_hh %>% mutate(rCSI = FS.3.1_NUM_COPE + (2 * FS.3.2_NUM_BURROW) + FS.3.5_NUM_REDUCED + FS.3.3_NUM_LIM + (3 * FS.3.4_NUM_LACK))
var_label(df_hh$rCSI) <- "Reduced coping strategies index (rCSI)"


# Unweighted - rCSI score
rCSI_table_mean <- df_hh %>% 
  drop_na(rCSI) %>% 
  summarise(meanrCSI = mean(rCSI))

print(rCSI_table_mean)


# ------------------------------------------------------------------------------
# EXPENDITURE
# ------------------------------------------------------------------------------

# SE.2.1_NUM_FOOD - monthly
# SE.2.2_NUM_ACCOM - monthly
# SE.2.3_NUM_HLTH - monthly
# SE.2.4_NUM_HYG - monthly
# SE.2.5_NUM_COMM - monthly
# SE.2.6_NUM_HH_BILL - monthly
# SE.2.7_NUM_OTH - monthly
# SE.2.0_NUM_HH_EXP - total monthly expenditure - REMOVE INCORRECT

# SE.2.8_NUM_HLTH_6_MTH  -  6 months 
# SE.2.9_NUM_DEBT - 6 months
# SE.2.10_NUM_EDU - 12 months 

# Replace 9999 and 99999 with NA in multiple columns
df_hh <- df_hh %>%
  mutate(across(c(SE.2.1_NUM_FOOD, SE.2.2_NUM_ACCOM,SE.2.3_NUM_HLTH, SE.2.4_NUM_HYG, 
                  SE.2.5_NUM_COMM, SE.2.6_NUM_HH_BILL, SE.2.7_NUM_OTH, SE.2.8_NUM_HLTH_6_MTH, 
                  SE.2.9_NUM_DEBT, SE.2.10_NUM_EDU), ~ replace(., . %in% c(9999, 99999), NA)))


# Check the type of the L9_SS_MIGR column
column_type <- typeof(df_hh$SE.2.8_NUM_HLTH_6_MTH)

# Print the result
print(column_type)

# HEALTHCARE EXPENDITURE 6 MONTHS - CONVERT TO 1 MONTH

# df_hh$SE.2.8_NUM_HLTH_6_MTH  <- as.numeric(df_hh$SE.2.0_NUM_HH_EXP)
df_hh$SE.2.8_NUM_HLTH_6_MTH  <- df_hh$SE.2.8_NUM_HLTH_6_MTH  / 6

# DEBT EXPENDITURE 6 MONTHS - CONVERT TO 1 MONTH
df_hh$SE.2.9_NUM_DEBT  <- df_hh$SE.2.9_NUM_DEBT  / 6

# EDUCATION EXPENDITURE 12 MONTHS - CONVERT TO 1 MONTH
df_hh$SE.2.10_NUM_EDU  <- df_hh$SE.2.10_NUM_EDU  / 12

# TOTAL EXPENDITURE - 1 MONTH
# df_hh$SE.2.0_NUM_HH_EXP  <- as.numeric(df_hh$SE.2.0_NUM_HH_EXP)

# 
# # Select the columns you want to sum
# #columns_to_sum <- c(
# #  "SE.2.1_NUM_FOOD", "SE.2.2_NUM_ACCOM", "SE.2.3_NUM_HLTH",
#   "SE.2.4_NUM_HYG", "SE.2.5_NUM_COMM", "SE.2.6_NUM_HH_BILL",
#   "SE.2.7_NUM_OTH", "SE.2.8_NUM_HLTH_6_MTH", "SE.2.9_NUM_DEBT",
#   "SE.2.10_NUM_EDU"
# )
# 
# 
# # Use rowSums to sum the selected columns while ignoring missing values
# df_hh$total_expenditure <- round(rowSums(df_hh[columns_to_sum], na.rm = TRUE), 2)

# Select the columns you want to sum
columns_to_sum <- c(
  "SE.2.1_NUM_FOOD", "SE.2.2_NUM_ACCOM", "SE.2.3_NUM_HLTH",
  "SE.2.4_NUM_HYG", "SE.2.5_NUM_COMM", "SE.2.6_NUM_HH_BILL",
  "SE.2.7_NUM_OTH", "SE.2.8_NUM_HLTH_6_MTH", "SE.2.9_NUM_DEBT",
  "SE.2.10_NUM_EDU"
)

# Create a logical vector indicating if NAs are present in the specified columns
na_in_se_columns <- rowSums(is.na(df_hh[, c("SE.2.1_NUM_FOOD", "SE.2.2_NUM_ACCOM", "SE.2.3_NUM_HLTH")])) > 0

# Use rowSums to sum the selected columns while ignoring missing values
df_hh$total_expenditure <- round(rowSums(df_hh[columns_to_sum], na.rm = TRUE), 2)

# Assign NA to total_expenditure when NAs are present in the specified columns
df_hh$total_expenditure[na_in_se_columns] <- NA


# write_xlsx(df_hh, "VAM/check_expenditure.xlsx")

# df_hh$total_expenditure <-  round(df_hh$SE.2.0_NUM_HH_EXP + df_hh$SE.2.8_NUM_HLTH_6_MTH + df_hh$SE.2.9_NUM_DEBT + df_hh$SE.2.10_NUM_EDU,2)

# 1. SHARE OF EXPENDITURE ON FOOD
df_hh$share_food_expenditure <- round(df_hh$SE.2.1_NUM_FOOD / df_hh$total_expenditure,2)

df_hh %>% summarise(average = mean(share_food_expenditure, na.rm = T))

# 2. SHARE OF EXPENDITURE ON ACCOMMODATION
df_hh$share_accomm_expenditure <- round(df_hh$SE.2.2_NUM_ACCOM / df_hh$total_expenditure,2)

# 3. SHARE OF EXPENDITURE ON HEALTH
df_hh$health_expenditure = df_hh$SE.2.3_NUM_HLTH + df_hh$SE.2.8_NUM_HLTH_6_MTH
df_hh$share_health_expenditure <- round(df_hh$health_expenditure / df_hh$total_expenditure,2)

# 4. SHARE OF EXPENDITURE ON HYGIENE
df_hh$share_hygiene_expenditure <- round(df_hh$SE.2.4_NUM_HYG / df_hh$total_expenditure,2)

# 5. SHARE OF EXPENDITURE ON COMMUNICATION
df_hh$share_communication_expenditure <- round(df_hh$SE.2.5_NUM_COMM / df_hh$total_expenditure,2)

# 6. SHARE OF EXPENDITURE ON HH BILLS
df_hh$share_hh_bills_expenditure <- round(df_hh$SE.2.6_NUM_HH_BILL / df_hh$total_expenditure,2)

# 7. SHARE OF EXPENDITURE ON EDUCATION
df_hh$share_education_expenditure <- round(df_hh$SE.2.10_NUM_EDU / df_hh$total_expenditure,2)

# 8. SHARE OF EXPENDITURE ON DEBT
df_hh$share_debt_expenditure <- round(df_hh$SE.2.9_NUM_DEBT / df_hh$total_expenditure,2)

# 9. SHARE OF EXPENDITURE ON OTHER
df_hh$share_other_expenditure <- round(df_hh$SE.2.7_NUM_OTH / df_hh$total_expenditure,2)
# ------------------------------------------------------------------------------

# EXPORT THE VARIABLES

df_hh_export <- df_hh %>%
  select("_index", "FCS","FCSCat21", "stress_coping_EN", "emergency_coping_EN", "crisis_coping_EN", "Max_coping_behaviourEN", "rCSI", "total_expenditure", "share_food_expenditure","share_accomm_expenditure", "share_health_expenditure","share_hygiene_expenditure","share_communication_expenditure","share_hh_bills_expenditure","share_education_expenditure","share_debt_expenditure","share_other_expenditure" ) 

write.xlsx(df_hh_export, "VAM/hh_indicators_czech.xlsx")


df_ind_export <- df_ind %>%
  select("_index","_parent_index","disability") 

write.xlsx(df_ind_export, "VAM/ind_indicators_czech.xlsx")


