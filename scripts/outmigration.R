library(readr)
library(dplyr)

# load IPEDS data and merge with the Carnegie Classification
hd2024 <- read_csv("../data/sources/hd2024.csv")
ace <- read_csv("../data/sources/ace-institutional-classifications.csv")
merged <- left_join(hd2024, ace, by = c("UNITID" = "unitid"))

# select variables and create a dataset for analysis
data <- merged %>% select(UNITID, INSTNM, CITY, ZIP, ICLEVEL, DEGGRANT, HBCU, TRIBAL, CBSA, CBSATYPE, CSA, COUNTYCD, COUNTYNM, CONTROL, INSTCAT, `Institutional Classification`, `Student Access and Earnings Classification`)

# create HSI status variable
data$HSI <- 0
library(readxl)
X2025_HSILists <- read_excel("../data/sources/2025_HSILists.xlsx")
hsi_data <- X2025_HSILists %>% select(INSTNM, HSI)
merged1 <- left_join(data, hsi_data, by = c("INSTNM"))
merged2 <- merged1 %>% mutate(HSI = coalesce(HSI.y, HSI.x)) %>% select(-HSI.x, -HSI.y)

# final dataset for analysis
write_csv(merged2, "~/Documents/GitHub/out_migration/data/data.csv")
library(readr)
data <- read_csv("~/Documents/GitHub/out_migration/data/data.csv")

# clean and recode Carnegie classification variables
data <- data %>% rename(purpose = `Institutional Classification`, selectivity = `Student Access and Earnings Classification`)

data <- data %>% filter(!grepl("^Special", purpose), !grepl("^Profession", purpose))

unique(data$purpose)
data <- data %>% mutate(PURPOSE = case_when(
      purpose == "Mixed Associate Small" ~ 1,
      purpose == "Mixed Associate Medium" ~ 2,
      purpose == "Mixed Associate Large" ~ 3,
      purpose == "Mixed Associate/Baccalaureate" ~ 4,
      purpose == "Mixed Baccalaureate" ~ 5,
      purpose == "Mixed Undergraduate/Graduate-Master's Small" ~ 6,
      purpose == "Mixed Undergraduate/Graduate-Master's Large/Medium" ~ 7,
      purpose == "Mixed Undergraduate/Graduate-Doctorate Small" ~ 8,
      purpose == "Mixed Undergraduate/Graduate-Doctorate Medium" ~ 9,
      purpose == "Mixed Undergraduate/Graduate-Doctorate Large" ~ 10,
      is.na(purpose) ~ -1 )) %>% select(-purpose)
purpose_lookup <- tibble(PURPOSE = c(-1, 1:10),
  purpose_label = c("NA", "Mixed Associate Small", "Mixed Associate Medium", "Mixed Associate Large",
                    "Mixed Associate/Baccalaureate", "Mixed Baccalaureate", 
                    "Mixed UG/Grad Master's Small", "Mixed UG/Grad Master's Large/Medium",
                    "Mixed UG/Grad Doctorate Small", "Mixed UG/Grad Doctorate Medium", "Mixed UG/Grad Doctorate Large"))

unique(data$selectivity)
data <- data %>% mutate(SELECTIVITY = case_when(
      selectivity == "Higher Access, Lower Earnings" ~ 1,
      selectivity == "Higher Access, Medium Earnings" ~ 2,
      selectivity == "Opportunity Colleges and Universities-Higher Access, Higher Earnings" ~ 3,
      selectivity == "Lower Access, Lower Earnings" ~ 4,
      selectivity == "Lower Access, Medium Earnings" ~ 5,
      selectivity == "Lower Access, Higher Earnings" ~ 6,
      is.na(selectivity) ~ -1,
      selectivity == "Not Classified" ~ -2)) %>% select(-selectivity)
selectivity_lookup <- tibble(SELECTIVITY = c(-2, -1, 1:6), selectivity_label = c(
    "Not Classified", "NA",
    "Higher Access, Lower Earnings", "Higher Access, Medium Earnings", "Opportunity Colleges and Universities (High Access, High Earnings)",
    "Lower Access, Lower Earnings", "Lower Access, Medium Earnings", "Lower Access, Higher Earnings"))

# preliminary cleaned data
write_csv(data, "~/Documents/GitHub/out_migration/data/outmigration_data.csv")



# load preliminary cleaned data
library(readr)
outmigration_data <- read_csv("~/Documents/GitHub/out_migration/data/outmigration_data.csv")