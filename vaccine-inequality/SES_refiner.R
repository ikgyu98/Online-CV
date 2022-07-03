library(tidyverse)
library(readxl)


df <- read_excel("Desktop/research/county SES.xlsx")[c(1,2,3,4,15)] %>%
  rename(location = "REG_ID") %>%
  add_column(country = "Norway", .before = "location") %>%
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
    TRUE ~ as.character(Indicator))
  )%>%
  mutate(IND = NULL) %>%
  mutate(Indicator = NULL)

a <- df%>%
  mutate(tempo = case_when(
    endsWith(Regions, "Agder and Rogaland") ~ "Agder,Rogaland",
    endsWith(Regions, "Hedmark and Oppland") ~ "Hedmark,Oppland",
    endsWith(Regions, "Northern Norway") ~ "Nordland,Troms og Finnmark",
    endsWith(Regions, "Oslo and Akershus") ~ "Oslo (f),Akershus",
    endsWith(Regions, "South-Eastern Norway") ~ "Viken,Oslo (f),Innlandet,Vestfold og Telemark",
    endsWith(Regions, "Western Norway") ~ "Rogaland,Vestland,Møre og Romsdal"
    ,TRUE ~ as.character(Regions)
  ))
  

SES <- separate_rows(a, tempo, sep = ",")%>%
  mutate(location = NULL)%>%
  mutate(Regions = NULL)%>%
  rename(location = "tempo")%>%
  relocate(location, .before = value)%>%
  relocate(measure, .before = value)

#View(SES)

write_xlsx(SES,"Desktop/Norway SES formatted.xlsx")


table(SES["location"])
