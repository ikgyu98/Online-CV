library(readr)
library(tibble)
library(dplyr)
library(tidyr)


# italysimple <- read_csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv", 
#                   col_types = cols(data = col_date(format = "%Y-%m-%d")))%>%
#   select(1, 2, 7, 8, 10, 16)%>%
#   rename(date = data, vaccine = forn, location = reg, `3 or booster` = db1, partial = d1, full = d2)%>%
#   mutate(country = "Italy")%>%
#   pivot_longer(3:5, names_to = "dose", values_to = "vnum")%>%
#   mutate(vaccine=replace(vaccine, vaccine=="Pfizer Pediatrico", "Pfizer"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="Pfizer/BioNTech", "Pfizer"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="Vaxzevria (AstraZeneca)", "AstraZeneca"))%>%
#   relocate(country, .before = date)%>%
#   relocate(location, .before = date)%>%
#   relocate(vaccine, .after = dose)
#   
# table(italysimple$vaccine)
# write_csv(italysimple,"/Users/mac/Desktop/research/italy/IT_Set_1_simple.csv")

italycomplex <- read_csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv", 
                        col_types = cols(data = col_date(format = "%Y-%m-%d")))%>%
  select(1, 2, 7, 8, 9, 10, 16)%>%
  mutate(sample = d1 + dpi)%>%
  select(-c(`d1`, `dpi`))%>%
  rename(date = data, vaccine = forn, location = reg, `3 or booster` = db1, partial = sample, full = d2)%>%
  mutate(country = "Italy")%>%
  relocate(country, .before = date)%>%
  relocate(location, .before = date)%>%
  pivot_longer(5:7, names_to = "dose", values_to = "vnum")%>%
  mutate(vaccine=replace(vaccine, vaccine=="Pfizer Pediatrico", "Pfizer"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="Pfizer/BioNTech", "Pfizer"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="Vaxzevria (AstraZeneca)", "AstraZeneca"))%>%
  relocate(vaccine, .after = dose)

italyses <- read_csv("Desktop/research/italy/italyses.csv")%>%
  rename(location = 1)

italy <- merge(x = italycomplex, y = italyses[ , c("location", "pop")], by = "location", all.x=TRUE)



italy_final <- italy%>%
  mutate(resolution = "1")%>%
  relocate(location, .before = date)%>%
  relocate(pop, .before = date)%>%
  relocate(resolution, .after = country)%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)

write_csv(italy_final,"/Users/mac/Desktop/research/italy/IT_Set_1.csv")






