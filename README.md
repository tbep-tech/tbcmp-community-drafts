# tbcmp-community-drafts
Draft products and code for the Tampa Bay Coastal Master Plan underserved community maps. Underserved communities are identified as those facing significant sociodemographic characteristics--including economic, health, and social burdens--that may make them especially vulnerable to the negative impacts of flooding. The following 12 burdens are considered in identifying underserved census tracts within the Tampa Bay region:
* **Income burdens**
  * Low income households
  * Social security income
* **Employment burdens**
  * Unemployed population
* **Housing burdens**
  * Housing cost stress
  * Mobile homes
* **Educational burdens**
  * Limited education
* **Language burdens**
  * Linguistically isolated
* **Family burdens**
  * Singe parent households
* **Health burdens**
  * Vulnerable age groups
  * Disabled population
  * Lack of health insurance
  * Low life expectancy


**Scripts Available**

This repository includes two scripts allowing for all products to be replicated by users in the R environment:
* <ins>data_download.R</ins>: Run this code to download all data from their respective online sources, clean the data, and generate the raw data that will be used to create the community maps.
* <ins>data_management.R</ins>: Run this code to use the raw data created in the previous script to generate and visualize the final community map, including threshold calculations and polygon attributes.

**Data Available**

The 'data' folder includes intermediary and final data products. These products can be created by running the associated code, or directly loaded/downloaded for easier application:
* <ins>metrics_florida.csv</ins>: Data table containing the raw sociodemographic data for all census tracts in Florida.
* <ins>tracts_tampabay.RData</ins>: Spatial data (sf object) containing the raw polygon boundaries of all census tracts in the counties represented in the Tampa Bay Coastal Master Plan (i.e., Citrus, Hernando, Hillsborough, Manatee, Pasco, Pinellas, and Sarasota), available from the U.S. Census Bureau [1].
* <ins>community_map.RData</ins>: Spatial data (sf object) containing the final sociodemographic characteristics for each census tract in the Tampa Bay region. Best for workspaces using R software.
* <ins>community_map.kml</ins>: Spatial data (KML) containing the final sociodemographic characteristics for each census tract in the Tampa Bay region. Best for workspaces using Google software (e.g., Google Earth, Google Maps).
* <ins>community_map.shp (.dbf, .prj, .shx)</ins>: Spatial data (ESRI Shapefile) containing the final sociodemographic characteristics for each census tract in the Tampa Bay region. Best for workspaces using GIS software (e.g., ArcGIS, QGIS).

**Data Fields**

Below is a description of the fields contained in the attribute table of the final community maps:
* Tract characteristics
  * <ins>tract</ins>: The unique ID of the given census tract as classified by the U.S. Census Bureau.
  * <ins>population</ins>: The total population, as estimated in the 2024 ACS [2].
  * <ins>area_mi2</ins>: The area (in square miles) of the census tract, based on the NAD83 / Florida West (ftUS) projected coordinate system. Note this differs from the WGS84 coordinate system used to map the data.
  * <ins>density</ins>: Population density (people per sq. mile) based on *population* and *area_mi2*.
* Sociodemographic characteristics
  * <ins>income</ins>: Percent of the population whose income is below 200% of the federal poverty level, as estimated in the 2024 ACS [3].
  * <ins>ssi</ins>: Percent of households receiving social security income, as estimated in the 2024 ACS [4].
  * <ins>unemployed</ins>: Percent of the population (16 years and older) that is unemployed, as estimated in the 2024 ACS [5].
  * <ins>costs</ins>: Percent of households (owned or rented) where housing costs are more than 30% of household income, as estimated in the 2024 ACS [6].
  * <ins>mobile</ins>: Percent of households that are mobile homes (and other miscellaneous types of units), as estimated in the 2024 ACS [7].
  * <ins>education</ins>: Percent of the population (25 years and older) that did not receive a high school diploma or GED, as estimated in the 2024 ACS [8].
  * <ins>language</ins>: Percent of households considered limited English-speaking, as estimated in the 2024 ACS [9].
  * <ins>single</ins>: Percent of households occupied by a householder with no spouse/partner and children under 18 years old, as estimated in the 2024 ACS [10].
  * <ins>age</ins>: Percent of the population under 5 years old or 65 years and older, as estimated in the 2024 ACS [11].
  * <ins>disabled</ins>: Percent of the population with a disability, as estimated in the 2024 ACS [12].
  * <ins>uninsured</ins>: Percent of the population with no health insurance coverage, as estimated in the 2024 ACS [13].
  * <ins>lifeexpect</ins>: Average life expectancy at time of birth (in years), as estimated for 2010-2015 by the NCHS [14].
* Sociodemographic percentiles
  * <ins>income_pct</ins>: The tract's percentile rank for *income* among all Florida census tracts.
  * <ins>ssi_pct</ins>: The tract's percentile rank for *ssi* among all Florida census tracts.
  * <ins>unemployed_pct</ins>: The tract's percentile rank for *unemployed* among all Florida census tracts.
  * <ins>costs_pct</ins>: The tract's percentile rank for *costs* among all Florida census tracts.
  * <ins>mobile_pct</ins>: The tract's percentile rank for *mobile* among all Florida census tracts.
  * <ins>education_pct</ins>: The tract's percentile rank for *education* among all Florida census tracts.
  * <ins>language_pct</ins>: The tract's percentile rank for *language* among all Florida census tracts.
  * <ins>single_pct</ins>: The tract's percentile rank for *single* among all Florida census tracts.
  * <ins>age_pct</ins>: The tract's percentile rank for *age* among all Florida census tracts.
  * <ins>disabled_pct</ins>: The tract's percentile rank for *disabled* among all Florida census tracts.
  * <ins>uninsured_pct</ins>: The tract's percentile rank for *uninsured* among all Florida census tracts.
  * <ins>lifeexpect_pct</ins>: The tract's percentile rank for *lifeexpect* among all Florida census tracts.
* Burden classifications
  * <ins>burden_income</ins>: Logical, indicates if *income_pct* is in the 80th percentile or higher.
  * <ins>burden_ssi</ins>: Logical, indicates if *ssi_pct* is in the 80th percentile or higher.
  * <ins>burden_unemployed</ins>: Logical, indicates if *unemployed_pct* is in the 80th percentile or higher.
  * <ins>burden_costs</ins>: Logical, indicates if *costs_pct* is in the 80th percentile or higher.
  * <ins>burden_mobile</ins>: Logical, indicates if *mobile_pct* is in the 80th percentile or higher.
  * <ins>burden_education</ins>: Logical, indicates if *education_pct* is in the 80th percentile or higher.
  * <ins>burden_language</ins>: Logical, indicates if *language_pct* is in the 80th percentile or higher.
  * <ins>burden_single</ins>: Logical, indicates if *single_pct* is in the 80th percentile or higher.
  * <ins>burden_age</ins>: Logical, indicates if *age_pct* is in the 80th percentile or higher.
  * <ins>burden_disabled</ins>: Logical, indicates if *disabled_pct* is in the 80th percentile or higher.
  * <ins>burden_uninsured</ins>: Logical, indicates if *uninsured_pct* is in the 80th percentile or higher.
  * <ins>burden_lifeexpect</ins>: Logical, indicates if *lifeexpect_pct* is in the 80th percentile or higher.
* Burden summaries
  * <ins>total_burdens</ins>: Total count of burden classifications across the 12 sociodemographic characteristics.
  * <ins>burdens</ins>: Verbose list of all the sociodemographic burdens identified in a given tract.

**Data Sources**
  * [1] U.S. Census Bureau. (2024). 2024 Cartographic Boundary Files (Shapefiles). *Scale 1:500,000 (national)*. Retrieved February 19, 2026, from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html.
  * [2] U.S. Census Bureau. (2024). Total Population. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table B01003*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [3] U.S. Census Bureau. (2024). Poverty Status in the Past 12 Months. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table S1701*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [4] U.S. Census Bureau. (2024). Social Security Income in the Past 12 Months for Households. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table B19055*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [5] U.S. Census Bureau. (2024). Employment Status. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table S2301*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [6] U.S. Census Bureau. (2024). Housing Costs as a Percentage of Household Income in the Past 12 Months. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table B25140*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [7] U.S. Census Bureau. (2024). Households and Families. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table S1101*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [8] U.S. Census Bureau. (2024). Educational Attainment for the Population 25 Years and Older. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table B15003*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [9] U.S. Census Bureau. (2024). Limited English Speaking Households. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table S1602*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [10] U.S. Census Bureau. (2024). Selected Social Characteristics in the United States. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table DP02*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [11] U.S. Census Bureau. (2024). Age and Sex. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table S0101*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [12] U.S. Census Bureau. (2024). Sex by Age by Disability Status. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table B18101*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [13] U.S. Census Bureau. (2024). Health Insurance Coverage Status by Sex by Age. *American Community Survey 2024, ACS 1-Year Estimates Detailed Tables, Table B27001*. Retrieved February 19, 2026, from https://data.census.gov/table.
  * [14] National Center for Health Statistics. (2019). U.S. Life Expectancy at Birth by State and Census Tract - 2010-2015. U.S. Centers for Disease Control and Prevention. Retrieved February 19, 2026, from https://data.cdc.gov/d/5h56-n989.
