library(readxl)
library(data.table)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(xlsx)

france <- read_delim("/Users/mac/Desktop/research/France Raw Dataset/vacsi-v-dep-2022-03-31-19h01.csv", 
                     ";", escape_double = FALSE, col_types = cols(jour = col_date(format = "%Y-%m-%d")), 
                     trim_ws = TRUE)%>%
  select(c(1:6))



reference <- read_excel("/Users/mac/Desktop/research/readagain.xlsx")%>%
  select(c(2,5))%>%
  rename(dep = number)

reference[1, 2] <- "01"
reference[2, 2] <- "02"
reference[3, 2] <- "03"
reference[4, 2] <- "04"
reference[5, 2] <- "05"
reference[6, 2] <- "06"
reference[7, 2] <- "07"
reference[8, 2] <- "08"
reference[9, 2] <- "09"

france_mani <- left_join(france, reference, by ='dep')

infant <- filter(france_mani, vaccin == "5")
adult <- filter(france_mani, vaccin == "1")

fuck <- left_join(adult, infant, by = c('dep', 'jour'))%>%
  rowwise()%>%
  mutate(n_dose1 = sum(c(n_dose1.x, n_dose1.y)))%>%
  mutate(n_dose2 = sum(c(n_dose2.x, n_dose2.y)))%>%
  mutate(n_dose3 = sum(c(n_dose3.x, n_dose3.y)))%>%
  select(c(1:3, 13:15, 7))%>%
  rename(vaccin = vaccin.x)%>%
  rename(location = location.x)%>%
  mutate(vaccin = "7")

france_ <- rbind(france_mani, fuck)

france__<-france_[!france_$vaccin=='5',]
france___<-france__[!france__$vaccin=='1',]

table(france___$vaccin)

france____ <- france___%>%
  mutate(vaccine = case_when(vaccin == "0" ~ "total",
                             vaccin == "2" ~ "Moderna",
                             vaccin == "3" ~ "AstraZeneka",
                             vaccin == "4" ~ "Janssen",
                             vaccin == "7" ~ "Pfizer",
                             vaccin == "6" ~ "Novavax"))%>%
  add_column(resolution = 2, .before="dep") %>%
  add_column(country = "France", .before="resolution")%>%
  mutate(dep = NULL)%>%
  rename(date = "jour")%>%
  mutate(vaccin = NULL)%>%
  relocate(location, .after = resolution)%>%
  rename("partial" = 5, "full" = 6, "3 or booster" = 7)%>%
  pivot_longer(5:7, names_to = "dose", values_to = "vnum")%>%
  add_column(vcum = NA, .after = "vnum")%>%
  add_column(vperc = NA, .after = "vcum")%>%
  relocate(vaccine, .after = dose)
    

population <- read_csv("/Users/mac/Desktop/research/France\ Raw\ Dataset/frenchpop.csv")%>%
  rename(location = `Department name`)

france_final <- left_join(france____, population, by ='location')%>%
  mutate(`Department code` = NULL)%>%
  rename(pop = `Municipal population`)%>%
  relocate(pop, .after = location)



write_csv(france_final,"/Users/mac/Desktop/research/France Raw Dataset/FR_Set_1.csv")
