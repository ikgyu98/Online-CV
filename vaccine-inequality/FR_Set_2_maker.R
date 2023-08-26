library(tidyverse)
library(readxl)
library(readr)
library("writexl")



France_SES <- read_csv("Desktop/research/France Raw Dataset/region SES.csv")[c(1,2,3,4,15)] %>%
  rename(location = "REG_ID") %>%
  add_column(country = "France", .before = "location") %>%
  rename(value = "Value")%>%
  mutate(measure = case_when(
    endsWith(Indicator, "Unemployment rate") ~ "labor2",
    endsWith(Indicator, "Standardised mortality rate") ~ "wellbeing2",
    endsWith(Indicator, "Share of labour force with at least secondary education") ~ "edu1",
    endsWith(Indicator, "Employment rate") ~ "labor1",
    endsWith(Indicator, "Life expectancy at birth") ~ "wellbeing1",
    endsWith(Indicator, "Air pollution, level of PM2.5") ~ "environ1",
    endsWith(Indicator, "Number of rooms per person") ~ "housing1",
    endsWith(Indicator, "Share of households with internet broadband access") ~ "housing2",
    endsWith(Indicator, "Disposable income per capita") ~ "income2",
    endsWith(Indicator, "Perceived social network support") ~ "etc1",
    endsWith(Indicator, "Homicide rate") ~ "etc2",
    endsWith(Indicator, "Self-evaluation of life satisfaction") ~ "etc3",
    endsWith(Indicator, "Perception of corrution") ~ "etc4",
    endsWith(Indicator, "Voter turnout in general election") ~ "etc5",
    TRUE ~ as.character(Indicator))
  )%>%
  mutate(IND = NULL) %>%
  mutate(Indicator = NULL)%>%
  mutate(location = NULL)%>%
  rename(location = "Regions")%>%
  relocate(measure, .before = value)

a<-France_SES %>%
  mutate(temp= ifelse(grepl("Auvergne", location), "Auvergne-Rhône-Alpes",
                      ifelse(grepl("Rhône-Alpes", location), "Auvergne-Rhône-Alpes",
                             ifelse(grepl("Burgundy", location), "Bourgogne-Franche-Comté",
                                    ifelse(grepl("Franche-Comté", location), "Bourgogne-Franche-Comté",
                                           ifelse(grepl("Brittany", location), "Bretagne",
                                                  ifelse(grepl("Centre", location), "Centre-Val de Loire",
                                                         ifelse(grepl("Corsica", location), "Corse",
                                                                ifelse(grepl("Alsace", location), "Grand Est",
                                                                       ifelse(grepl("Champagne-Ardenne", location), "Grand Est",
                                                                              ifelse(grepl("Lorraine", location), "Grand Est",
                                                                                     ifelse(grepl("Picardy", location), "Hauts-de-France",
                                                                                            ifelse(grepl("Nord-Pas-de-Calais", location), "Hauts-de-France",
                                                                                                   ifelse(grepl("Ile de France", location), "Ile-de-France",
                                                                                                          ifelse(grepl("Lower Normandy", location), "Normandie",
                                                                                                                 ifelse(grepl("Upper Normandy", location), "Normandie",
                                                                                                                        ifelse(grepl("Aquitaine", location), "Nouvelle-Aquitaine",
                                                                                                                               ifelse(grepl("Limousin", location), "Nouvelle-Aquitaine",
                                                                                                                                      ifelse(grepl("Poitou-Charentes", location), "Nouvelle-Aquitaine",
                                                                                                                                             ifelse(grepl("Languedoc-Roussillon", location), "Occitanie",
                                                                                                                                                    ifelse(grepl("Midi-Pyrénées", location), "Occitanie",
                                                                                                                                                           ifelse(grepl("Provence-Alpes-Côte d'Azur", location), "Provence-Alpes-Côte d'Azur",
                                                                                                                                                                  ifelse(grepl("Pays de la Loire", location), "Pays de la Loire",
                                                                                                                                                                         NA)))))))))))))))))))))))%>%
  na.omit()%>%
  group_by(temp, measure)%>%
  summarise(n = mean(value))%>%
  rename(value = "n", location = "temp")%>%
  add_column(country = "France", .before = "location") 







#write_xlsx(a,"/Users/mac/Desktop/research/FR_Set_2.xlsx")



