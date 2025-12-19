library(readr)
library(dplyr)

hd2024 <- read_csv("Documents/GitHub/out_migration/data/hd2024.csv")
ace <- read_csv("Documents/GitHub/out_migration/data/ace-institutional-classifications.csv")
merged <- left_join(hd2024, ace, by = c("UNITID" = "unitid"))


data <- merged %>% select(UNITID, INSTNM, CITY, ZIP, ICLEVEL, DEGGRANT, HBCU, TRIBAL, CBSA, CBSATYPE, CSA, COUNTYCD, COUNTYNM, CONTROL, INSTCAT, `Institutional Classification`)
data$HSI <- 0

X2025_HSILists <- read_excel("Downloads/2025_HSILists.xlsx")
data1 <- X2025_HSILists %>% select(INSTNM, HSI)

merged1 <- left_join(data, data1, by = c("INSTNM"))
merged2 <- merged1 %>% mutate(HSI = coalesce(HSI.y, HSI.x)) %>% select(-HSI.x, -HSI.y)

write_csv(merged2, "~/Documents/GitHub/out_migration/data/data.csv")