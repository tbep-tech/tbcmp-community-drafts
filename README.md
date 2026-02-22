# tbcmp-community-drafts
Draft products and code for the Tampa Bay Coastal Master Plan underserved community maps.

**Scripts Available**
* <ins>data_download.R</ins>: Run this code to download all data from their respective online sources, clean the data, and generate the raw data that will be used to create the community maps.
* <ins>data_management.R</ins>: Run this code to use the raw data created in the previous script to generate and visualize the final community map, including threshold calculations and polygon attributes.

**Data Available**
* <ins>metrics_florida.csv</ins>: Data table containing the raw sociodemographic data for all census tracts in Florida.
* <ins>tracts_tampabay.RData</ins>: Spatial data (sf object) containing the raw polygon boundaries of all census tracts in the counties represented in the Tampa Bay Coastal Master Plan (i.e., Citrus, Hernando, Hillsborough, Manatee, Pasco, Pinellas, and Sarasota).
* <ins>community_map.RData</ins>: Spatial data (sf object) containing the final sociodemographic characteristics for each census tract in the Tampa Bay region. Best for workspaces using R software.
* <ins>community_map.kml</ins>: Spatial data (KML) containing the final sociodemographic characteristics for each census tract in the Tampa Bay region. Best for workspaces using Google software (e.g., Google Earth, Google Maps).
* <ins>community_map.shp (.dbf, .prj, .shx)</ins>: Spatial data (ESRI Shapefile) containing the final sociodemographic characteristics for each census tract in the Tampa Bay region. Best for workspaces using GIS software (e.g., ArcGIS, QGIS).

**Data Fields**

Below is a description of the fields contained in the attribute table of the final community maps:
* <ins>tract</ins>: The unique ID of the given census tract as classified by the U.S. Census Bureau.
* <ins>population</ins>: The total population, as estimated in the 2024 ACS [1].
* <ins>area_mi2</ins>: The area (in square miles) of the census tract, based on the NAD83 / Florida West (ftUS) projected coordinate system. Note this differs from the WGS84 coordinate system used to map the data.
* <ins>density</ins>: Population density (people per sq. mile) based on *population* and *area_mi2*.
* <ins>income</ins>: Percent of the population below 200% of the federal poverty line, as estimated in the 2024 ACS [2].
* <ins>ssi</ins>: Percent of the population receiving social security income, as estimated in the 2024 ACS [3].
* <ins>unemployed</ins>: Percent of the population that is unemployed, as estimated in the 2024 ACS [4].
* <ins>costs</ins>: Percent of the population whose housing costs constitute more than 30% of their income, as estimated in the 2024 ACS [5].
* <ins>mobile</ins>: Percent of households that are mobile (and other) homes, as estimated in the 2024 ACS [6].
* <ins>education</ins>: Percent of the population age 25 and older that did not complete high school, as estimated in the 2024 ACS [7].
* <ins>language</ins>: Percent of households considered linguistically isolated, as estimated in the 2024 ACS [8].
* <ins>single</ins>: Percent of households occupied by a single parent of one or more children (under 18 years old), as estimated in the 2024 ACS [9].
* <ins>age</ins>: Percent of the population under 5 years old or 65 years and older, as estimated in the 2024 ACS [10].
* <ins>disabled</ins>: Percent of the population with a disability, as estimated in the 2024 ACS [11].
* <ins>uninsured</ins>: Percent of the population without health insurance, as estimated in the 2024 ACS [12].
* <ins>lifeexpect</ins>: Average life expectancy at time of birth (in years), as estimated by the CDC [13].
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


