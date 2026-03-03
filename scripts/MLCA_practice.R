## ---------------------------
##' [libraries]
## ---------------------------
library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tidycensus)
library(tigris)       #maybe we don't need this package
options(tigris_use_cache = TRUE)
library(sf)
library(multilevLCA)

#setwd("~/GitHub/out_migration/")


## ---------------------------
##' [IPEDS/Carnegie Classification Data]
## ---------------------------
# load IPEDS data and merge with the Carnegie Classification
ipeds24 <- read_csv("data/sources/hd2024.csv")
carnegie <- read_excel("data/sources/2025-Public-Data-File.xlsx", sheet = 2)
data <- left_join(ipeds24, carnegie, by = c("UNITID" = "unitid"))

# select variables and create a dataset for analysis
data <- data |> select(LONGITUD, LATITUDE, 
                        UNITID, INSTNM, 
                        CITY, STABBR, ZIP, ICLEVEL, DEGGRANT, LOCALE,
                        HBCU, TRIBAL, 
                        CBSA, CBSATYPE, CSA, COUNTYCD, COUNTYNM, 
                        CONTROL, INSTCAT,
                        CARNEGIEIC, CARNEGIESAEC, hsi)

# rename variables
data <- data |> rename(instnm = INSTNM, 
                       longitude = LONGITUD, 
                       latitude = LATITUDE,
                       unitid = UNITID, city = CITY, stabbr = STABBR, zip = ZIP,
                       iclevel = ICLEVEL, deggrant = DEGGRANT, locale = LOCALE,
                       hbcu = HBCU, tribal = TRIBAL,
                       cbsa = CBSA, cbsatype = CBSATYPE, csa = CSA, countycd = COUNTYCD,
                       countynm = COUNTYNM, control = CONTROL, instcat = INSTCAT,
                       carnegieic = CARNEGIEIC, carnegiesaec = CARNEGIESAEC) |> 
  select(unitid, instnm, city, countycd, countynm, cbsa, cbsatype, csa, 
         locale, zip, stabbr, longitude, latitude, 
         control, iclevel, instcat, deggrant, 
         carnegieic, carnegiesaec, ic2025, saec2025,
         hbcu, hsi, tribal)



## ---------------------------
##' [Data Cleaning]
## ---------------------------
data <- data |> 
  filter(iclevel != -3 & iclevel != 3) |> 
  mutate(instcat = ifelse(instcat %n% c(-2, -1, 1), NA, instcat),
         control = ifelse(control %n% c(-1, 3), NA, control),
         csa = ifelse(csa == -2, NA, csa), 
         cbsatype = ifelse(cbsatype == -2, NA, cbsatype))
#should be updated



## ---------------------------
##' [Census/ACS Data]
## ---------------------------

#urban <- urban_areas(cb = FALSE, year = 2024) |>
#  mutate(states = str_extract(NAME10, ",(.*)")) |>
#  filter(!str_detect(states, "AS|VI|MP|GU"))
# -> Q. Is this process necessary?

census <- get_acs(geography = "cbsa",
                  year = 2024,
                  variables = c("B01003_001E", # Total population
                                "B19001_001E", # Total Household Income in the Past 12 Months
                                "B19013_001E", # Median Household Income in the Past 12 Months
                                "B19025_001E", # Aggregated Household Income in the Past 12 Months
                                "DP03_0051E", # INCOME AND BENEFITS_Total households (estimate)
                                "DP03_0051PE", # INCOME AND BENEFITS_Total households (percent)
                                "DP03_0119PE", # % below poverty
                                "DP02_0065E", # Population 25 years and over!!Bachelor's degree (estimate)
                                "DP02_0065PE", # Population 25 years and over!!Bachelor's degree (percent)
                                "DP02_0068PE", # % bach or higher
                                "DP02PR_0068PE", # % bach or higher for PR
                                "B03002_003E", # total white (alone, not hispanic)
                                "B03002_004E", # total black (alone, not hispanic)
                                "B03002_012E", # total hispanic
                                "B03002_001E" #total by ethnicity
                  ),
                  output = "wide") 
# Q. which variables should be included?


#urban <- left_join(urban, census, by = c("GEOID10" = "GEOID"))

urban <- urban |>
  mutate(perc_white = B03002_003E/B03002_001E*100,
         perc_black = B03002_004E/B03002_001E*100,
         perc_hisp = B03002_012E/B03002_001E*100,
         perc_bach = coalesce(DP02_0068PE, DP02PR_0068PE), #Q. which variable should be used for % bach or higher?
         tot_pop = B03002_001E*100/100000,
         pop_sqmile = B01003_001E/(ALAND10/2.59e+6)) |> # sq meters to sq miles #Q. is this the way to calculate population density?
  select(GEOID10, NAME10, UATYP10, states,
         perc_white, perc_black, perc_hisp,
         perc_commute_transit = DP03_0021PE, 
         perc_novehicle = DP04_0058PE, 
         perc_poverty = DP03_0119PE, 
         perc_bach, tot_pop, pop_sqmile)

# TIGER boundary for Population Density
counties_sf <- counties(year = 2024, cb = TRUE)   # ALAND unit = m²
pop <- get_acs(geography = "county",
               variables = "B01003_001", # Total population
               year = 2024,
               survey = "acs5") # American Community Survey 5-year estimates (mean of 2020-2024)
tiger <- pop |> 
  left_join(counties_sf |> st_drop_geometry() |> select(GEOID, ALAND),
            by = "GEOID")  |> 
  mutate(
    land_area_km2 = ALAND / 1e6,
    density = estimate / land_area_km2
    )


## ---------------------------
##' [MLCA]
## ---------------------------

# Example structure, replace with our actual data and variable names
# data: Our data frame
# Y: Vector of column names for observed indicator variables
# id_high: Column name for the higher-level group ID
# iT: Number of lower-level (individual) classes
# iM: Number of higher-level (group) classes

model_fit <- multiLCA(
  data = data_frame,
  Y = c("item1", "item2", "item3", "item4"), # Your indicator items
  id_high = "group_id_column",               # The column identifying groups
  iT = 3,                                    # e.g., 3 lower-level classes
  iM = 2                                     # e.g., 2 higher-level classes
)

# View a summary of the model results
summary(model_fit)

