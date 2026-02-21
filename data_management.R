# Load required packages (install as needed)
library(tidyverse)
library(sf)
library(leaflet)


# ---- Create attribute table ----

# Load saved data
metricsFL <- read.csv("data/metrics_florida.csv")

# Perform calculations
metricsFL <- metricsFL %>%
  # calculate percentiles for each variable, ignoring NAs
  mutate(income_pct = if_else(is.na(income), NA_real_, percent_rank(income)*100),
         ssi_pct = if_else(is.na(ssi), NA_real_, percent_rank(ssi)*100),
         unemployed_pct = if_else(is.na(unemployed), NA_real_, percent_rank(unemployed)*100),
         costs_pct = if_else(is.na(costs), NA_real_, percent_rank(costs)*100),
         mobile_pct = if_else(is.na(mobile), NA_real_, percent_rank(mobile)*100),
         education_pct = if_else(is.na(education), NA_real_, percent_rank(education)*100),
         language_pct = if_else(is.na(language), NA_real_, percent_rank(language)*100),
         single_pct = if_else(is.na(single), NA_real_, percent_rank(single)*100),
         age_pct = if_else(is.na(age), NA_real_, percent_rank(age)*100),
         disabled_pct = if_else(is.na(disabled), NA_real_, percent_rank(disabled)*100),
         uninsured_pct = if_else(is.na(uninsured), NA_real_, percent_rank(uninsured)*100),
         lifeexpect_pct = if_else(is.na(lifeexpect), NA_real_, percent_rank(lifeexpect)*100)) %>%
  # flag if variable meets the criteria (>=80th or <=20th percentile, depending on variable)
  mutate(burden_income = if_else(income_pct >= 80, 1, 0, missing = 0),
         burden_ssi = if_else(ssi_pct >= 80, 1, 0, missing = 0),
         burden_unemployed = if_else(unemployed_pct >= 80, 1, 0, missing = 0),
         burden_costs = if_else(costs_pct >= 80, 1, 0, missing = 0),
         burden_mobile = if_else(mobile_pct >= 80, 1, 0, missing = 0),
         burden_education = if_else(education_pct >= 80, 1, 0, missing = 0),
         burden_language = if_else(language_pct >= 80, 1, 0, missing = 0),
         burden_single = if_else(single_pct >= 80, 1, 0, missing = 0),
         burden_age = if_else(age_pct >= 80, 1, 0, missing = 0),
         burden_disabled = if_else(disabled_pct >= 80, 1, 0, missing = 0),
         burden_uninsured = if_else(uninsured_pct >= 80, 1, 0, missing = 0),
         burden_lifeexpect = if_else(lifeexpect_pct <= 20, 1, 0, missing = 0)) %>%
  # calculate the total number of burdens experience in each census tract
  mutate(total_burdens = rowSums(across(burden_income:burden_lifeexpect), na.rm = TRUE)) %>%
  # create field to list each sociodemographic burden threshold met
  mutate(txt_income = ifelse(burden_income == 1, "Low income", NA),
         txt_ssi = ifelse(burden_ssi == 1, "Social security income", NA),
         txt_unemployed = ifelse(burden_unemployed == 1, "Unemployment", NA),
         txt_costs = ifelse(burden_costs == 1, "Housing cost stress", NA),
         txt_mobile = ifelse(burden_mobile == 1, "Mobile homes", NA),
         txt_education = ifelse(burden_education == 1, "Limited education", NA),
         txt_language = ifelse(burden_language == 1, "Linguistically isolated", NA),
         txt_single = ifelse(burden_single == 1, "Single-parent households", NA),
         txt_age = ifelse(burden_age == 1, "Vulnerable age groups", NA),
         txt_disabled = ifelse(burden_disabled == 1, "Disabilities", NA),
         txt_uninsured = ifelse(burden_uninsured == 1, "Lacking health insurance", NA),
         txt_lifeexpect = ifelse(burden_lifeexpect == 1, "Low life expectancy", NA)) %>%
  mutate(burdens = paste(txt_income,txt_ssi,txt_unemployed,txt_costs,txt_mobile,txt_education,
                         txt_language,txt_single,txt_age,txt_disabled,txt_uninsured,txt_lifeexpect, sep = ", ")) %>%
  mutate(burdens = gsub('NA, ', '', burdens)) %>%
  mutate(burdens = gsub(', NA', '', burdens)) %>%
  mutate(burdens = ifelse(burdens == "NA", "None", burdens))



















