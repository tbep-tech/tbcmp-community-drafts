

# Load required packages (install as needed)
library(httr)
library(jsonlite)
library(tidyverse)

# ---- Download and clean tract-level data from 2024 American Community Survey (U.S. Census Bureau) ----

####
# ---- *-- Total Population ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B01003)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
  )

# Download data
response <- GET(url)
stop_for_status(response)

# Parse JSON safely
content_txt <- content(response, as = "text", encoding = "UTF-8")

# Disable simplification
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)

# First row = column names
col_names <- unlist(json_raw[[1]])

# Remaining rows = data
data_rows <- json_raw[-1]

# Bind rows into data frame
df_population <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_population) <- col_names

# ---- *---- Clean ----

df_population <- df_population %>%
  # convert character strings to numeric values
  mutate(across(c(B01003_001E),
                as.numeric)) %>%
  # rename table fields
  rename(population = B01003_001E,
         tract = GEO_ID) %>%
  # remove first 9 characters to get the census tract ID
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  # keep only the necessary data
  select(tract, population)


####
# ---- *-- Limited Education ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B15003)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_education <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_education) <- col_names

# ---- *---- Clean ----

df_education <- df_education %>%
  mutate(across(c(B15003_001E,B15003_002E,B15003_003E,B15003_004E,B15003_005E,B15003_006E,B15003_007E,B15003_008E,
                        B15003_009E,B15003_010E,B15003_011E,B15003_012E,B15003_013E,B15003_014E,B15003_015E,B15003_016E),
                as.numeric)) %>%
  # calculate % of population that did not complete high school
  mutate(education = (B15003_002E+B15003_003E+B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+
           B15003_009E+B15003_010E+B15003_011E+B15003_012E+B15003_013E+B15003_014E+B15003_015E+B15003_016E)/B15003_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, education)


####
# ---- *-- Vulnerable Age Groups ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S0101)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_age <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_age) <- col_names

# ---- *---- Clean ----

df_age <- df_age %>%
  mutate(across(c(S0101_C01_001E,S0101_C01_002E,S0101_C01_015E,S0101_C01_016E,S0101_C01_017E,S0101_C01_018E,S0101_C01_019E),
                as.numeric)) %>%
  # calculate % of population that is under 5 or over 64 years old
  mutate(age = (S0101_C01_002E+S0101_C01_015E+S0101_C01_016E+S0101_C01_017E+S0101_C01_018E+S0101_C01_019E)/S0101_C01_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, age)



####
# ---- *-- Linguistically Isolated Households ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S1602)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_language <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_language) <- col_names

# ---- *---- Clean ----

df_language <- df_language %>%
  mutate(across(c(S1602_C04_001E),
                as.numeric)) %>%
  rename(tract = GEO_ID,
         language = S1602_C04_001E) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, language)


####
# ---- *-- Unemployment ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S2301)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_unemployed <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_unemployed) <- col_names

# ---- *---- Clean ----

df_unemployed <- df_unemployed %>%
  mutate(across(c(S2301_C04_001E),
                as.numeric)) %>%
  rename(tract = GEO_ID,
         unemployed = S2301_C04_001E) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, unemployed)



####
# ---- *-- Low Income ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S1701)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_income <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_income) <- col_names

# ---- *---- Clean ----

df_income <- df_income %>%
  mutate(across(c(S1701_C01_001E,S1701_C01_042E),
                as.numeric)) %>%
  # calculate % of population that is below 200% of the federal poverty line
  mutate(income = (S1701_C01_042E)/S1701_C01_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, income)



####
# ---- *-- Social Security Income Households ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B19055)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_ssi <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_ssi) <- col_names

# ---- *---- Clean ----

df_ssi <- df_ssi %>%
  mutate(across(c(B19055_001E,B19055_002E),
                as.numeric)) %>%
  # calculate % of households receiving social security income
  mutate(ssi = (B19055_002E)/B19055_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, ssi)




####
# ---- *-- Single Parent Households ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/profile?get=group(DP02)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_single <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_single) <- col_names

# ---- *---- Clean ----

df_single <- df_single %>%
  mutate(across(c(DP02_0007PE,DP02_0011PE),
                as.numeric)) %>%
  # calculate % of households that have single men or women with children
  mutate(single = DP02_0007PE+DP02_0011PE) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, single)



# ---- *-- Housing Cost Burden ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B25140)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_costs <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_costs) <- col_names

# ---- *---- Clean ----

df_costs <- df_costs %>%
  mutate(across(c(B25140_001E,B25140_003E,B25140_007E,B25140_011E),
                as.numeric)) %>%
  # calculate % of households where housing costs are over 30% of household income
  mutate(costs = (B25140_003E+B25140_007E+B25140_011E)/B25140_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, costs)



# ---- *-- Disability ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B18101)&ucgid=1400000US12101030101,1400000US12101030102,1400000US12101030202"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_disabled <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_disabled) <- col_names

# ---- *---- Clean ----

df_disabled <- df_disabled %>%
  mutate(across(c(B18101_001E,B18101_004E,B18101_007E,B18101_010E,B18101_013E,B18101_016E,B18101_019E,
                  B18101_023E,B18101_026E,B18101_029E,B18101_032E,B18101_035E,B18101_038E),
                as.numeric)) %>%
  # calculate % of population with a disability
  mutate(disabled = (B18101_004E+B18101_007E+B18101_010E+B18101_013E+B18101_016E+B18101_019E+
                     B18101_023E+B18101_026E+B18101_029E+B18101_032E+B18101_035E+B18101_038E)/B18101_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, disabled)











# Write CSV
write.csv(df, "census_data.csv", row.names = FALSE)

message("CSV saved successfully.")