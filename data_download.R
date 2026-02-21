# Load required packages (install as needed)
library(httr)
library(jsonlite)
library(tidyverse)
library(sf)
library(leaflet)
library(progressr)

# ---- Download and clean tract-level data from 2024 American Community Survey (U.S. Census Bureau) ----

####
# ---- *-- Total Population ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B01003)&ucgid=pseudo(0400000US12$1400000)"
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
# ---- *-- Low Income ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S1701)&ucgid=pseudo(0400000US12$1400000)"
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
  "https://api.census.gov/data/2024/acs/acs5?get=group(B19055)&ucgid=pseudo(0400000US12$1400000)"
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
# ---- *-- Unemployment ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S2301)&ucgid=pseudo(0400000US12$1400000)"
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



# ---- *-- Housing Cost Burden ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B25140)&ucgid=pseudo(0400000US12$1400000)"
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



####
# ---- *-- Mobile Homes ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S1101)&ucgid=pseudo(0400000US12$1400000)"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_mobile <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_mobile) <- col_names

# ---- *---- Clean ----

df_mobile <- df_mobile %>%
  mutate(across(c(S1101_C01_017E),
                as.numeric)) %>%
  rename(tract = GEO_ID,
         mobile = S1101_C01_017E) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, mobile)



####
# ---- *-- Limited Education ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B15003)&ucgid=pseudo(0400000US12$1400000)"
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
# ---- *-- Linguistically Isolated Households ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S1602)&ucgid=pseudo(0400000US12$1400000)"
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
# ---- *-- Single Parent Households ----
####

# ---- *---- Download ----

# NOTE: The census only provides this data at the tract level one county at a time.
# Instead of running code for every county, this code creates a function to do it at once
# It amends the URL to insert a new FL county tract ID (FIPS code) each time to get the right data

# Start by listing all the FL county census tract IDs (FIPS codes)
county_fips <- c(
  "12001","12003","12005","12007","12009","12011","12013","12015","12017","12019",
  "12021","12023","12027","12029","12031","12033","12035","12037","12039","12041",
  "12043","12045","12047","12049","12051","12053","12055","12057","12059","12061",
  "12063","12065","12067","12069","12071","12073","12075","12077","12079","12081",
  "12083","12085","12086","12087","12089","12091","12093","12095","12097","12099",
  "12101","12103","12105","12107","12109","12111","12113","12115","12117","12119",
  "12121","12123","12125","12127","12129","12131","12133"
)

# Define the function
get_county_data <- function(full_fips) {
  # dynamically insert the FIPS code into the API URL
  url <- paste0(
    "https://api.census.gov/data/2024/acs/acs5/profile?",
    "get=group(DP02)&ucgid=pseudo(0500000US",
    full_fips,
    "$1400000)"
  )
  # download and read the data
  response <- GET(url)
  stop_for_status(response)
  content_txt <- content(response, as = "text", encoding = "UTF-8")
  json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
  col_names <- unlist(json_raw[[1]])
  data_rows <- json_raw[-1]
  df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
  colnames(df) <- col_names
  
  # ---- Clean ----
  df <- df %>%
    mutate(across(c(DP02_0007PE, DP02_0011PE), as.numeric)) %>%
    mutate(single = DP02_0007PE + DP02_0011PE) %>%
    rename(tract = GEO_ID) %>%
    mutate(tract = substr(tract, 10, nchar(tract))) %>%
    select(tract, single)
  
  return(df)
}

# Run for all counties and merge them into one dataset (and show a live progress bar)
handlers(global = TRUE)
handlers("txtprogressbar")  # simple console bar

with_progress({
  p <- progressor(along = county_fips)
  
  df_single <- map_dfr(county_fips, function(fips) {
    p()  # update progress bar
    get_county_data(fips)
  })
})



####
# ---- *-- Vulnerable Age Groups ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5/subject?get=group(S0101)&ucgid=pseudo(0400000US12$1400000)"
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




# ---- *-- Disability ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B18101)&ucgid=pseudo(0400000US12$1400000)"
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


####
# ---- *-- No Health Insurance ----
####

# ---- *---- Download ----

# API URL
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs5?get=group(B27001)&ucgid=pseudo(0400000US12$1400000)"
)

# Download data
response <- GET(url)
stop_for_status(response)
content_txt <- content(response, as = "text", encoding = "UTF-8")
json_raw <- fromJSON(content_txt, simplifyVector = FALSE)
col_names <- unlist(json_raw[[1]])
data_rows <- json_raw[-1]
df_uninsured <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
colnames(df_uninsured) <- col_names

# ---- *---- Clean ----

df_uninsured <- df_uninsured %>%
  mutate(across(c(B27001_001E,B27001_005E,B27001_008E,B27001_011E,B27001_014E,B27001_017E,B27001_020E,
                  B27001_023E,B27001_026E,B27001_029E,B27001_033E,B27001_036E,B27001_039E,B27001_042E,
                  B27001_045E,B27001_048E,B27001_051E,B27001_054E,B27001_057E),
                as.numeric)) %>%
  # calculate % of population without health insurance coverage
  mutate(uninsured = (B27001_005E+B27001_008E+B27001_011E+B27001_014E+B27001_017E+B27001_020E+
                        B27001_023E+B27001_026E+B27001_029E+B27001_033E+B27001_036E+B27001_039E+B27001_042E+
                        B27001_045E+B27001_048E+B27001_051E+B27001_054E+B27001_057E)/B27001_001E*100) %>%
  rename(tract = GEO_ID) %>%
  mutate(tract = substr(tract, 10, nchar(tract))) %>% 
  select(tract, uninsured)



# ---- Download and clean tract-level data from the Centers for Disease Control and Prevention (CDC) ----

####
# ---- *-- Life Expectancy (2010-2015) ----
####

# ---- *---- Download ----

# API URL
url <-"https://data.cdc.gov/resource/5h56-n989.csv?$limit=50000"

# Download CSV and convert to data frame
df_lifeexpect <- read.csv(url, stringsAsFactors = FALSE)

# ---- *---- Clean ----

df_lifeexpect <- df_lifeexpect %>%
  # remove states other than Florida
  filter(state_name == "Florida") %>%
  # remove census tracts without life expectancy data
  drop_na() %>%
  # convert the truncated census tract number to a character string without the decimal
  mutate(full_ct_num = as.character(full_ct_num*100)) %>%
  # add preceding zeros to census tract number as needed to reach 6 digits for neighborhood number
  mutate(full_ct_num = case_when(
    nchar(full_ct_num) == 3 ~ paste0("000", full_ct_num),
    nchar(full_ct_num) == 4 ~ paste0("00", full_ct_num),
    nchar(full_ct_num) == 5 ~ paste0("0", full_ct_num),
    nchar(full_ct_num) == 6 ~ full_ct_num,
    TRUE ~ full_ct_num)) %>% # keeps values unchanged if they don't match conditions
  # the census tract number needs to incorporate the preceding state (12) and county codes (3-digit) to be complete
  mutate(tract = case_when(
    county_name == "Alachua County, FL" ~ paste0("12001", full_ct_num),
    county_name == "Baker County, FL" ~ paste0("12003", full_ct_num),
    county_name == "Bay County, FL" ~ paste0("12005", full_ct_num),
    county_name == "Bradford County, FL" ~ paste0("12007", full_ct_num),
    county_name == "Brevard County, FL" ~ paste0("12009", full_ct_num),
    county_name == "Broward County, FL" ~ paste0("12011", full_ct_num),
    county_name == "Calhoun County, FL" ~ paste0("12013", full_ct_num),
    county_name == "Charlotte County, FL" ~ paste0("12015", full_ct_num),
    county_name == "Citrus County, FL" ~ paste0("12017", full_ct_num),
    county_name == "Clay County, FL" ~ paste0("12019", full_ct_num),
    county_name == "Collier County, FL" ~ paste0("12021", full_ct_num),
    county_name == "Columbia County, FL" ~ paste0("12023", full_ct_num),
    county_name == "DeSoto County, FL" ~ paste0("12027", full_ct_num),
    county_name == "Dixie County, FL" ~ paste0("12029", full_ct_num),
    county_name == "Duval County, FL" ~ paste0("12031", full_ct_num),
    county_name == "Escambia County, FL" ~ paste0("12033", full_ct_num),
    county_name == "Flagler County, FL" ~ paste0("12035", full_ct_num),
    county_name == "Franklin County, FL" ~ paste0("12037", full_ct_num),
    county_name == "Gadsden County, FL" ~ paste0("12039", full_ct_num),
    county_name == "Gilchrist County, FL" ~ paste0("12041", full_ct_num),
    county_name == "Glades County, FL" ~ paste0("12043", full_ct_num),
    county_name == "Gulf County, FL" ~ paste0("12045", full_ct_num),
    county_name == "Hamilton County, FL" ~ paste0("12047", full_ct_num),
    county_name == "Hardee County, FL" ~ paste0("12049", full_ct_num),
    county_name == "Hendry County, FL" ~ paste0("12051", full_ct_num),
    county_name == "Hernando County, FL" ~ paste0("12053", full_ct_num),
    county_name == "Highlands County, FL" ~ paste0("12055", full_ct_num),
    county_name == "Hillsborough County, FL" ~ paste0("12057", full_ct_num),
    county_name == "Holmes County, FL" ~ paste0("12059", full_ct_num),
    county_name == "Indian River County, FL" ~ paste0("12061", full_ct_num),
    county_name == "Jackson County, FL" ~ paste0("12063", full_ct_num),
    county_name == "Jefferson County, FL" ~ paste0("12065", full_ct_num),
    county_name == "Lafayette County, FL" ~ paste0("12067", full_ct_num),
    county_name == "Lake County, FL" ~ paste0("12069", full_ct_num),
    county_name == "Lee County, FL" ~ paste0("12071", full_ct_num),
    county_name == "Leon County, FL" ~ paste0("12073", full_ct_num),
    county_name == "Levy County, FL" ~ paste0("12075", full_ct_num),
    county_name == "Liberty County, FL" ~ paste0("12077", full_ct_num),
    county_name == "Madison County, FL" ~ paste0("12079", full_ct_num),
    county_name == "Manatee County, FL" ~ paste0("12081", full_ct_num),
    county_name == "Marion County, FL" ~ paste0("12083", full_ct_num),
    county_name == "Martin County, FL" ~ paste0("12085", full_ct_num),
    county_name == "Miami-Dade County, FL" ~ paste0("12086", full_ct_num),
    county_name == "Monroe County, FL" ~ paste0("12087", full_ct_num),
    county_name == "Nassau County, FL" ~ paste0("12089", full_ct_num),
    county_name == "Okaloosa County, FL" ~ paste0("12091", full_ct_num),
    county_name == "Okeechobee County, FL" ~ paste0("12093", full_ct_num),
    county_name == "Orange County, FL" ~ paste0("12095", full_ct_num),
    county_name == "Osceola County, FL" ~ paste0("12097", full_ct_num),
    county_name == "Palm Beach County, FL" ~ paste0("12099", full_ct_num),
    county_name == "Pasco County, FL" ~ paste0("12101", full_ct_num),
    county_name == "Pinellas County, FL" ~ paste0("12103", full_ct_num),
    county_name == "Polk County, FL" ~ paste0("12105", full_ct_num),
    county_name == "Putnam County, FL" ~ paste0("12107", full_ct_num),
    county_name == "St. Johns County, FL" ~ paste0("12109", full_ct_num),
    county_name == "St. Lucie County, FL" ~ paste0("12111", full_ct_num),
    county_name == "Santa Rosa County, FL" ~ paste0("12113", full_ct_num),
    county_name == "Sarasota County, FL" ~ paste0("12115", full_ct_num),
    county_name == "Seminole County, FL" ~ paste0("12117", full_ct_num),
    county_name == "Sumter County, FL" ~ paste0("12119", full_ct_num),
    county_name == "Suwannee County, FL" ~ paste0("12121", full_ct_num),
    county_name == "Taylor County, FL" ~ paste0("12123", full_ct_num),
    county_name == "Union County, FL" ~ paste0("12125", full_ct_num),
    county_name == "Volusia County, FL" ~ paste0("12127", full_ct_num),
    county_name == "Wakulla County, FL" ~ paste0("12129", full_ct_num),
    county_name == "Walton County, FL" ~ paste0("12131", full_ct_num),
    county_name == "Washington County, FL" ~ paste0("12133", full_ct_num),
    TRUE ~ full_ct_num)) %>% # keeps values unchanged if they don't match conditions
  rename(lifeexpect = le) %>%
  select(tract, lifeexpect)

# ---- Combine the datasets and save for future use ----

# Combine data for each tract, allowing NAs if data unavailable for certain tracts
merged_data <- df_population %>%
  full_join(df_income, by = "tract") %>%
  full_join(df_ssi, by = "tract") %>%
  full_join(df_unemployed, by = "tract") %>%
  full_join(df_costs, by = "tract") %>%
  full_join(df_mobile, by = "tract") %>%
  full_join(df_education, by = "tract") %>%
  full_join(df_language, by = "tract") %>%
  full_join(df_single, by = "tract") %>%
  full_join(df_age, by = "tract") %>%
  full_join(df_disabled, by = "tract") %>%
  full_join(df_uninsured, by = "tract") %>%
  full_join(df_lifeexpect, by = "tract")

# Save for future use (only need to do once)
# dir.create("data", recursive = TRUE, showWarnings = FALSE)
# write.csv(merged_data, "data/metrics_florida.csv", row.names = FALSE)




# ---- Download and clean census tract spatial data from the Cartographic Boundary Files (GENZ2024, U.S. Census TIGER) ----

# ---- *---- Download ----

# API URL
url <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_tract_500k.zip"

# Create a temporary directory
temp_dir <- tempfile()
dir.create(temp_dir)

# Define path to save the zip file
zip_path <- file.path(temp_dir, "cb_2024_us_tract_500k.zip")

# Download the file
download.file(url, destfile = zip_path, mode = "wb")

# Unzip the contents
unzip(zip_path, exdir = temp_dir)

# Find and read the .shp file
shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
tract_sf <- st_read(shp_file)

# Convert to WGS84 for viewing with leaflet
tract_wgs84 <- st_transform(tract_sf, crs = 4326)

# ---- *---- Clean ----

tampabay <- tract_wgs84 %>%
  # keep only census tracts in Florda
  filter(STATE_NAME == "Florida") %>%
  # keep only census tracts in Tampa Bay Coastal Master Plan counties
  filter(NAMELSADCO == "Citrus County" | NAMELSADCO == "Hernando County" | 
           NAMELSADCO == "Hillsborough County" | NAMELSADCO == "Manatee County" | 
           NAMELSADCO == "Pasco County" | NAMELSADCO == "Pinellas County" | 
           NAMELSADCO == "Sarasota County")

# View the census tracts with leaflet
tampabay %>%
  select(GEOID) %>% 
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = "blue",
    fillOpacity = 0.5,
    weight = 0.5,
    opacity = 1,
    color = "black",
    popup = ~GEOID, 
    group = "Census Tract"
  ) %>%
  addLayersControl(
    overlayGroups = "Census Tract",
    options = layersControlOptions(collapsed = FALSE)
  )


# When you're done, clean up by deleting the temporary folders you created for the full tract data
unlink(temp_dir, recursive = TRUE)
