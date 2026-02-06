library(readr)
library(dplyr)
library(readxl)
library(stringr)

#setwd("~/GitHub/out_migration/")

# load IPEDS data and merge with the Carnegie Classification
hd2024 <- read_csv("data/sources/hd2024.csv")
ace <- read_csv("data/sources/ace-institutional-classifications.csv")
data <- left_join(hd2024, ace, by = c("UNITID" = "unitid"))

# select variables and create a dataset for analysis
data <- data %>% select(LONGITUD, LATITUDE, 
                          UNITID, INSTNM, 
                          CITY, STABBR, ZIP, ICLEVEL, DEGGRANT, HBCU, TRIBAL, 
                          CBSA, CBSATYPE, CSA, COUNTYCD, COUNTYNM, 
                          CONTROL, INSTCAT,
                          `Institutional Classification`, `Student Access and Earnings Classification`)


# create HSI status variable
X2025_HSILists <- read_excel("data/sources/2025_HSILists.xlsx")

data <- X2025_HSILists %>% 
  select(INSTNM, HSI) %>% 
  right_join(data, by = c("INSTNM")) %>% 
  mutate(HSI = if_else(is.na(HSI), 0, HSI))

# rename variables
data <- data |> rename(instnm = INSTNM, hsi = HSI, longitude = LONGITUD, latitude = LATITUDE,
                       unitid = UNITID, city = CITY, stabbr = STABBR, zip = ZIP,
                       iclevel = ICLEVEL, deggrant = DEGGRANT, hbcu = HBCU, tribal = TRIBAL,
                       cbsa = CBSA, cbsatype = CBSATYPE, csa = CSA, countycd = COUNTYCD,
                       countynm = COUNTYNM, control = CONTROL, instcat = INSTCAT,
                       c_level = `Institutional Classification`, 
                       c_seaclass = `Student Access and Earnings Classification`) |> 
                select(unitid, instnm, city, stabbr, countycd, countynm, cbsa, cbsatype, csa, 
                       zip,longitude, latitude, 
                       control, iclevel, instcat, deggrant, c_level, c_seaclass,
                       hbcu, hsi, tribal)


# clean and recode Carnegie classification variables
data <- data |> 
  mutate(c_level = case_when(
      c_level == "Mixed Associate Small" ~ 1,
      c_level == "Mixed Associate Medium" ~ 2,
      c_level == "Mixed Associate Large" ~ 3,
      c_level == "Mixed Associate/Baccalaureate" ~ 4,
      c_level == "Mixed Baccalaureate" ~ 5,
      c_level == "Mixed Undergraduate/Graduate-Master's Small" ~ 6,
      c_level == "Mixed Undergraduate/Graduate-Master's Large/Medium" ~ 7,
      c_level == "Mixed Undergraduate/Graduate-Doctorate Small" ~ 8,
      c_level == "Mixed Undergraduate/Graduate-Doctorate Medium" ~ 9,
      c_level == "Mixed Undergraduate/Graduate-Doctorate Large" ~ 10,
      is.na(c_level) ~ -1,
      str_detect(c_level, "^Special") ~ -9,
      str_detect(c_level, "^Professions") ~ -8)) %>% 
  mutate(c_seaclass = case_when(
    c_seaclass == "Higher Access, Lower Earnings" ~ 1,
    c_seaclass == "Higher Access, Medium Earnings" ~ 2,
    c_seaclass == "Opportunity Colleges and Universities-Higher Access, Higher Earnings" ~ 3,
    c_seaclass == "Lower Access, Lower Earnings" ~ 4,
    c_seaclass == "Lower Access, Medium Earnings" ~ 5,
    c_seaclass == "Lower Access, Higher Earnings" ~ 6,
    is.na(c_seaclass) ~ -1,
    c_seaclass == "Not Classified" ~ -2)) |> 
  #data |> count(c_level) |> count(c_seaclass)
  filter(iclevel != -3 & iclevel != 3 )



#Turning non desired variables into NA
outmigration_data <- outmigration_data |>
    mutate(instcat = ifelse(instcat == 1, NA, instcat), 
           instcat = ifelse(instcat == -1, NA, instcat), 
           instcat = ifelse(instcat == -2, NA, instcat), 
           control = ifelse(control == 3, NA, control), 
           csa = ifelse(csa == -2, NA, csa), 
           cbsatype = ifelse(cbsatype == -2, NA, cbsatype))