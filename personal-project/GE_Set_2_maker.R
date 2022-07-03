library(readr)
library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)

household_income <- read_csv("Desktop/research/germany/household income.csv")
gross_earnings <- read_csv("Desktop/research/germany/gross earnings.csv")%>%
  inner_join(household_income)%>%
  mutate("Kennziffer" = NULL)%>%
  rename(income5= 3, income7=2, location = 1)%>%
  mutate(country = "Germany")%>%# 5 = household/ 7 = gross earning
  pivot_longer(cols = c(income7, income5), names_to = "measure", values_to = "value")%>%
  relocate(country, .before = location)
  


write_csv(gross_earnings,"/Users/mac/Desktop/research/germany/GE_Set_2.csv")




View(household_income)