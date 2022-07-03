library(readr)
library(dplyr)
library(tidyr)
library(data.table)


# denmark <- read_csv("https://uofi.box.com/shared/static/6ln48qdhr6ssaaxsxshb2s4n6vmo1v4f.csv", 
#                     col_types = cols(date = col_date(format = "%Y-%m-%d")))
# 
# table(denmark$region)


russia <- read_csv("https://uofi.box.com/shared/static/px12gegk336ionmw6va8letutiv1uw61.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

table(russia$region)

sweden <- read_csv("https://uofi.box.com/shared/static/lfokn40o4tumc35hdp1sua6w777v9swp.csv", 
            col_types = cols(date = col_date(format = "%Y-%m-%d")))


table(spain$region_iso)


spain <- read_csv("https://uofi.box.com/shared/static/5gsa9ayzbtvns9owzyhqfzy3a48jr2oq.csv", 
            col_types = cols(date = col_date(format = "%Y-%m-%d")))


table(sweden$region_iso)

population <- fread("https://raw.githubusercontent.com/sociepy/covid19-vaccination-subnational/main/data/population.csv")%>%
  select(c(1,3))

russiases <- read_csv("Desktop/vaccine_inequality/russiases.csv")%>%
  pivot_longer(2, names_to = "measure", values_to = "value")%>%
  rename(`region` = 1)

pop <- population%>%
  distinct(region_iso, .keep_all = TRUE)

rus <- left_join(russia, pop, by = "region_iso")%>%
  mutate(resolution = "1")%>%
  mutate(total_vaccinations = NULL)%>%
  rename(country = location)%>%
  relocate(resolution, .after = country)%>%
  relocate(date, .after = region_iso)%>%
  rename(`pop` = `population`)%>%
  relocate(pop, .after = date)%>%
  rename(partial = people_vaccinated, full = people_fully_vaccinated)%>%
  pivot_longer(8:9, values_to = "vcum", names_to = "dose")

test <- left_join(rus, russiases, by = "region")

write_csv(test, "Desktop/vaccine_inequality/RU_Set_12.csv")

table(rus$region)
table(rus$region_iso)


spa <- left_join(spain, pop, by = "region_iso")%>%
  mutate(resolution = "1")%>%
  mutate(total_vaccinations = NULL)%>%
  rename(country = location)%>%
  relocate(resolution, .after = country)%>%
  relocate(date, .after = region_iso)%>%
  rename(`pop` = `population`)%>%
  relocate(pop, .after = date)%>%
  rename(partial = people_vaccinated, full = people_fully_vaccinated)%>%
  pivot_longer(8:9, values_to = "vcum", names_to = "dose")



swe <- left_join(sweden, pop, by = "region_iso")%>%
  mutate(resolution = "1")%>%
  mutate(total_vaccinations = NULL)%>%
  rename(country = location)%>%
  relocate(resolution, .after = country)%>%
  relocate(date, .after = region_iso)%>%
  rename(`pop` = `population`)%>%
  relocate(pop, .after = date)%>%
  rename(partial = people_vaccinated, full = people_fully_vaccinated)%>%
  pivot_longer(8:9, values_to = "vcum", names_to = "dose")


write_csv(rus, "Desktop/vaccine_inequality/RU_Set_1.csv")

write_csv(spa, "Desktop/vaccine_inequality/SP_Set_1.csv")

write_csv(swe, "Desktop/vaccine_inequality/SW_Set_1.csv")


spainses <- read_csv("Desktop/vaccine_inequality/spain_ses.csv")%>%
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
  select(c(2, 6, 7))%>%
  rename(location = 1, value = 2)%>%
  relocate(measure, .before = value)%>%
  mutate(country = "Spain")%>%
  relocate(country, .before = location)

write_csv(spainses, "Desktop/vaccine_inequality/SP_Set_2.csv")

swedses <- read_csv("Desktop/vaccine_inequality/sweden_ses.csv")%>%
  rename(location = 1, income11 = 2, income12 = 3, income6 = 4)%>%
  pivot_longer(2:4, names_to = "measure", values_to = "value")%>%
  mutate(country = "Sweden")%>%
  relocate(country , .before = location)

write_csv(swedses, "Desktop/vaccine_inequality/SW_Set_2.csv")
table(spa$region)
table(spainses$Regions)

table(sweden$region)
table(swedses$...1)

