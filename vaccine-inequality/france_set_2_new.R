library(readxl)
library(data.table)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(xlsx)
library(writexl)

# france <- read_delim("Desktop/research/France Raw Dataset/vacsi-v-dep-2022-03-31-19h01.csv", 
#                      ";", escape_double = FALSE, col_types = cols(jour = col_date(format = "%Y-%m-%d")), 
#                      trim_ws = TRUE)
# 
# 
# table(france$dep)
# 
# 
# france_ses <- read_delim("Desktop/research/France Raw Dataset/cc_filosofi_2018_DEP-geo2021.CSV", 
#                          ";", escape_double = FALSE, trim_ws = TRUE)%>%
#   select(c(1,4,6))%>%
#   add_column(country = "France", .before = "CODGEO")
# 
# write_xlsx(france_ses ,"/Users/mac/Desktop/research/readagain.xlsx")

france_ses <- read_excel("Desktop/research/readagain.xlsx")%>%
  select(c(1:4))%>%
  rename(income6 = `TP6018`)%>%
  rename(income8 = `MED18`)%>%
  pivot_longer(cols = c(income6, income8), names_to = "measure", values_to = "value")

write_csv(france_ses ,"Desktop/research/France Raw Dataset/FR_Set_2.csv")

