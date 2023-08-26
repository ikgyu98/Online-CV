library(readxl)
library(data.table)
library(writexl)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(convertr)


ke_ses <- read_csv("Desktop/research/kenya/poverty_kenya_table_2015.csv")%>%
  slice(-c(1,2,3,4))%>%
  rename(income7=`Food poverty (percentage)`)%>%
  rename(income8=`Food poverty Gap (percentage)`)%>%
  rename(income6=`Overall Poverty (percentage)`)%>%
  rename(income10=`Overall Poverty Gap (percentage)`)%>%
  rename(income5=`Hardcore poverty (percentage)`)%>%
  rename(income9=`Poverty Gap (percentage)`)%>%
  pivot_longer(cols = c(income7, income8, income6, income10, income5, income9), names_to = "measure", values_to = "value")%>%
  mutate(`Population per 1000` = NULL)%>%
  rename(location=`Residence/ County`)%>%
  mutate(country = "Kenya")%>%
  relocate(country, .before = location)


write_csv(ke_ses ,"Desktop/research/kenya/KE_Set_2.csv")
