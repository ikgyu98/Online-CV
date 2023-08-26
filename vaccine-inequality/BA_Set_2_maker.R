library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(convertr)


temp<- read_csv("/Users/mac/Desktop/research/bangladesh/dataset/income_by_union.csv")%>%
  drop_na()%>%
  select(District, income)%>%
  group_by(District)%>%
  summarise(value = mean(income))%>%
  rename(location = District)%>%
  add_column(country = "Bangladesh", .before="location")%>%
  add_column(measure = "income7", .after="location")
  
  
write_csv(temp,"/Users/mac/Desktop/research/bangladesh/dataset/BA_Set_2.csv")

