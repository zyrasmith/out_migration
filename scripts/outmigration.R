library(readr)
library(dplyr)
library(readxl)

#setwd("~/GitHub/out_migration/")

# load IPEDS data and merge with the Carnegie Classification
hd2024 <- read_csv("~/GitHub/out_migration/data/sources/hd2024.csv")
ace <- read_csv("~/GitHub/out_migration/data/sources/ace-institutional-classifications.csv")
merged <- left_join(hd2024, ace, by = c("UNITID" = "unitid"))

# select variables and create a dataset for analysis
data <- merged %>% select(LONGITUD, LATITUDE, 
                          UNITID, INSTNM, 
                          CITY, STABBR, ZIP, ICLEVEL, DEGGRANT, HBCU, TRIBAL, 
                          CBSA, CBSATYPE, CSA, COUNTYCD, COUNTYNM, 
                          CONTROL, INSTCAT,
                          `Institutional Classification`, `Student Access and Earnings Classification`)


# create HSI status variable
#data$HSI <- 0

X2025_HSILists <- read_excel("~/GitHub/out_migration/data/sources/2025_HSILists.xlsx")

hsi_data <- X2025_HSILists %>% 
  select(INSTNM, HSI) %>% 
  right_join(data, by = c("INSTNM")) %>% 
  mutate(HSI = if_else(is.na(HSI), 0, HSI))


# # final dataset for analysis
# write_csv(merged2, "~/Documents/GitHub/out_migration/data/data.csv")
# data <- read_csv("~/Documents/GitHub/out_migration/data/data.csv")

# clean and recode Carnegie classification variables
data <- hsi_data %>% 
  rename(level = `Institutional Classification`, 
         sea_class = `Student Access and Earnings Classification`)%>% 
  filter(!grepl("^Special", level), !grepl("^Profession", level)) %>% 
  #unique(data$level)
  mutate(level = case_when(
      level == "Mixed Associate Small" ~ 1,
      level == "Mixed Associate Medium" ~ 2,
      level == "Mixed Associate Large" ~ 3,
      level == "Mixed Associate/Baccalaureate" ~ 4,
      level == "Mixed Baccalaureate" ~ 5,
      level == "Mixed Undergraduate/Graduate-Master's Small" ~ 6,
      level == "Mixed Undergraduate/Graduate-Master's Large/Medium" ~ 7,
      level == "Mixed Undergraduate/Graduate-Doctorate Small" ~ 8,
      level == "Mixed Undergraduate/Graduate-Doctorate Medium" ~ 9,
      level == "Mixed Undergraduate/Graduate-Doctorate Large" ~ 10,
      is.na(level) ~ -1 )) %>% 
  mutate(sea_class = case_when(
    sea_class == "Higher Access, Lower Earnings" ~ 1,
    sea_class == "Higher Access, Medium Earnings" ~ 2,
    sea_class == "Opportunity Colleges and Universities-Higher Access, Higher Earnings" ~ 3,
    sea_class == "Lower Access, Lower Earnings" ~ 4,
    sea_class == "Lower Access, Medium Earnings" ~ 5,
    sea_class == "Lower Access, Higher Earnings" ~ 6,
    is.na(sea_class) ~ -1,
    sea_class == "Not Classified" ~ -2)) %>% 
  filter(ICLEVEL != -3 & ICLEVEL != 3 )


#  select(-level)
level_lookup <- tibble(level = c(-1, 1:10),
  level_label = c("NA", "Mixed Associate Small", "Mixed Associate Medium", "Mixed Associate Large",
                    "Mixed Associate/Baccalaureate", "Mixed Baccalaureate", 
                    "Mixed UG/Grad Master's Small", "Mixed UG/Grad Master's Large/Medium",
                    "Mixed UG/Grad Doctorate Small", "Mixed UG/Grad Doctorate Medium", "Mixed UG/Grad Doctorate Large"))

unique(data$sea_class)
data <- data %>%  %>% select(-sea_class)
sea_class_lookup <- tibble(sea_class = c(-2, -1, 1:6), sea_class_label = c(
    "Not Classified", "NA",
    "Higher Access, Lower Earnings", "Higher Access, Medium Earnings", "Opportunity Colleges and Universities (High Access, High Earnings)",
    "Lower Access, Lower Earnings", "Lower Access, Medium Earnings", "Lower Access, Higher Earnings"))

# preliminary cleaned data
write_csv(data, "~/Documents/GitHub/out_migration/data/outmigration_data.csv")


# load preliminary cleaned data
library(readr)
outmigration_data <- read_csv("~/GitHub/out_migration/data/outmigration_data.csv")

#Turning non desired variables into NA
library(dplyr)

outmigration_data <- outmigration_data |>
    mutate(INSTCAT = ifelse(INSTCAT == 1, NA, INSTCAT), 
           INSTCAT = ifelse(INSTCAT == -1, NA, INSTCAT), 
           INSTCAT = ifelse(INSTCAT == -2, NA, INSTCAT), 
           CONTROL = ifelse(CONTROL == 3, NA, CONTROL), 
           CSA= ifelse(CSA == -2, NA, CSA), 
           CBSATYPE= ifelse(CBSATYPE == -2, NA, CBSATYPE))






