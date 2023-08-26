library(readxl)
library(data.table)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(xlsx)


france <- read_delim("Desktop/research/France Raw Dataset/vacsi-a-reg-2022-02-15-19h09.csv",
                     ";", escape_double = FALSE, trim_ws = TRUE)

xxx<-france %>%
  mutate(location = case_when(
    reg == "01" ~ "Guadeloupe",
    reg == "02" ~ "Martinique",
    reg == "03" ~ "Guyane",
    reg == "04" ~ "La Réunion",
    reg == "11" ~ "Ile-de-France",
    reg == "24" ~ "Centre-Val de Loire",
    reg == "27" ~ "Bourgogne-Franche-Comté",
    reg == "28" ~ "Normandie",
    reg == "32" ~ "Hauts-de-France",
    reg == "44" ~ "Grand Est",
    reg == "52" ~ "Pays de la Loire",
    reg == "53" ~ "Bretagne",
    reg == "75" ~ "Nouvelle-Aquitaine",
    reg == "76" ~ "Occitanie",
    reg == "84" ~ "Auvergne-Rhône-Alpes",
    reg == "93" ~ "Provence-Alpes-Côte d’Azur",
    reg == "94" ~ "Corse",
    reg == "05" ~ "Saint-Pierre-et-Miquelon",
    reg == "06" ~ "Mayotte",
    reg == "07" ~ "Saint-Barthélemy",
    reg == "08" ~ "Saint-Martin", TRUE ~ as.character(reg)))%>%
  relocate(location, .before = reg)%>%
  mutate(reg = NULL) %>%
  rename(date = "jour")%>%
  add_column(resolution = 1, .before="location") %>%
  add_column(country = "France", .before="resolution") 




kkk <- xxx %>%
  filter(clage_vacsi == "0")%>%
  mutate(n_cum_dose1= NULL)%>%
  mutate(n_cum_complet= NULL)%>%
  mutate(n_cum_rappel= NULL)%>%
  mutate(couv_dose1= NULL)%>%
  mutate(couv_complet= NULL)%>%
  mutate(couv_rappel= NULL)%>%
  rename("partial" = 6, "full" = 7, "3 or booster" = 8)%>%
  pivot_longer(6:8, names_to = "dose", values_to = "vnum")%>%
  add_column(vcum = NA, .after = "vnum")%>%
  add_column(vperc = NA, .after = "vcum")%>%
  mutate(clage_vacsi=NULL)



kkk <- kkk %>%
  mutate(pop = case_when(
    location == "Auvergne-Rhône-Alpes" ~ "8092598",
    location == "Bourgogne-Franche-Comté" ~ "2786205",
    location == "Bretagne" ~ "3371297",
    location == "Centre-Val de Loire" ~ "2562431",
    location == "Corse" ~ "349273",
    location == "Grand Est" ~ "5524817",
    location == "Hauts-de-France" ~ "5977462",
    location == "Ile-de-France" ~ "12326429",
    location == "Normandie" ~ "3306092",
    location == "Nouvelle-Aquitaine" ~ "6039767",
    location == "Occitanie" ~ "5985751",
    location == "Pays de la Loire" ~ "3838060",
    location == "Provence-Alpes-Côte d’Azur" ~ "5089661"))%>%
  filter(!is.na(pop))%>%
  relocate(pop, .after = location)


#write_xlsx(kkk,"/Users/mac/Desktop/research/FR_Set_1.xlsx")


