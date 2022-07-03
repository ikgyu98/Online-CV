library(readr)
library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)

sample <- read_csv("Desktop/research/germany/2022-04-14_Deutschland_Landkreise_COVID-19-Impfungen.csv")


germanypop <- read_delim("Downloads/covid-19-germany-landkreise.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select(`Regional code`, `Population`)%>%
  rename(location = `Regional code`)%>%
  rename(area = `Population`)


pop <- read_csv("Desktop/research/germany/household income.csv")%>%
  mutate(Haushaltseinkommen = NULL)%>%
  rename(location = 1, area = 2)%>%
  left_join(germanypop, by = "location")%>%
  rename(pop = `area.y`)

population <- pop%>%
  select(-`location`)%>%
  rename(location = `area.x`)


germ1 <- read_csv("Desktop/research/germany/2022-04-14_Deutschland_Landkreise_COVID-19-Impfungen.csv", 
                                                                  col_types = cols(Impfdatum = col_date(format = "%Y-%m-%d"), 
                                                                                   Impfschutz = col_character()))%>%
  rename(date = 1, location = 2, age = 3, dose = 4, vnum = 5)%>%
  filter(dose == 1)%>%
  group_by(date, location)%>%
  summarise(vnum = sum(vnum))%>%
  inner_join(pop)%>%
  mutate(dose = "partial")



germ2 <- read_csv("Desktop/research/germany/2022-04-14_Deutschland_Landkreise_COVID-19-Impfungen.csv", 
                  col_types = cols(Impfdatum = col_date(format = "%Y-%m-%d"), 
                                   Impfschutz = col_character()))%>%
  rename(date = 1, location = 2, age = 3, dose = 4, vnum = 5)%>%
  filter(dose == 2)%>%
  group_by(date, location)%>%
  summarise(vnum = sum(vnum))%>%
  inner_join(pop)%>%
  mutate(dose = "full")

germ3 <- read_csv("Desktop/research/germany/2022-04-14_Deutschland_Landkreise_COVID-19-Impfungen.csv", 
                  col_types = cols(Impfdatum = col_date(format = "%Y-%m-%d"), 
                                   Impfschutz = col_character()))%>%
  rename(date = 1, location = 2, age = 3, dose = 4, vnum = 5)%>%
  filter(dose == 3)%>%
  group_by(date, location)%>%
  summarise(vnum = sum(vnum))%>%
  inner_join(pop)%>%
  mutate(dose = "3 or booster")
  

total <- rbind(germ1, germ2, germ3)%>%
  mutate(location=NULL)%>%
  rename(location = area)%>%
  mutate(country = "Germany")%>%
  mutate(resolution = 2)%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before=date)%>%
  relocate(vnum, .before = vcum)%>%
  relocate(resolution, .after = country)%>%
  relocate(date, .before = dose)%>%
  inner_join(population)%>%
  relocate(pop, .before = `date`)



write_csv(total,"/Users/mac/Desktop/research/germany/GE_Set_1.csv")

  
  
View(germ)