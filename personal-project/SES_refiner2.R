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

#View(df)

 
#colnames()
#View(SES)

write_xlsx(df,"Desktop/NO_Set_2.xlsx")


#table(SES["location"])

