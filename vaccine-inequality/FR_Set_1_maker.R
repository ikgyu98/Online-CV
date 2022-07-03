library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)
library(readr)


france <- read_delim("Desktop/research/France Raw Dataset/vacsi-a-reg-2022-02-15-19h09.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

  
a<-france %>%
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
  #mutate(clage_vacsi = NULL) %>%
  rename(date = "jour")%>%
  add_column(resolution = 1, .before="location") %>%
  add_column(country = "France", .before="resolution") 


b <- a %>%
  filter(clage_vacsi == "04")%>%
  mutate(n_cum_dose1= NULL)%>%
  mutate(n_cum_complet= NULL)%>%
  mutate(n_cum_rappel= NULL)%>%
  mutate(couv_dose1= NULL)%>%
  mutate(couv_complet= NULL)%>%
  mutate(couv_rappel= NULL)%>%
  rename("partial" = 6, "full" = 7, "3 or booster" = 8)%>%
  pivot_longer(6:8, names_to = "dose", values_to = "vnum")
  # pivot_longer(n_dose1:n_rappel, names_to = c("partial", "full", "3 or booster"), values_to = "vnum")%>%
  # pivot_longer(n_dose1:n_rappel, names_to = c("partial", "full", "3 or booster"), values_to = "vnum")



c <- a %>%
  filter(clage_vacsi == "09")%>%
  mutate(n_cum_dose1= NULL)%>%
  mutate(n_cum_complet= NULL)%>%
  mutate(n_cum_rappel= NULL)%>%
  mutate(couv_dose1= NULL)%>%
  mutate(couv_complet= NULL)%>%
  mutate(couv_rappel= NULL)%>%
  rename("partial" = 6, "full" = 7, "3 or booster" = 8)%>%
  pivot_longer(6:8, names_to = "dose", values_to = "vnum")

table(c$location)
View(c)

d <- a %>%
  filter(clage_vacsi == "11")
e <- a %>%
  filter(clage_vacsi == "17")
f <- a %>%
  filter(clage_vacsi == "24")
g <- a %>%
  filter(clage_vacsi == "29")
h <- a %>%
  filter(clage_vacsi == "39")
i <- a %>%
  filter(clage_vacsi == "49")
j <- a %>%
  filter(clage_vacsi == "59")
k <- a %>%
  filter(clage_vacsi == "64")
l <- a %>%
  filter(clage_vacsi == "69")
m <- a %>%
  filter(clage_vacsi == "74")
n <- a %>%
  filter(clage_vacsi == "79")
o <- a %>%
  filter(clage_vacsi == "80")


kkk <- a %>%
  filter(clage_vacsi == "0")%>%
  mutate(n_cum_dose1= NULL)%>%
  mutate(n_cum_complet= NULL)%>%
  mutate(n_cum_rappel= NULL)%>%
  mutate(couv_dose1= NULL)%>%
  mutate(couv_complet= NULL)%>%
  mutate(couv_rappel= NULL)%>%
  rename("partial" = 6, "full" = 7, "3 or booster" = 8)%>%
  pivot_longer(6:8, names_to = "dose", values_to = "vnum")

View(kkk)



zzz <- a %>%
  group_by(clage_vacsi) %>%
  mutate(n_cum_dose1= NULL)%>%
  mutate(n_cum_complet= NULL)%>%
  mutate(n_cum_rappel= NULL)%>%
  mutate(couv_dose1= NULL)%>%
  mutate(couv_complet= NULL)%>%
  mutate(couv_rappel= NULL)%>%
  rename("partial" = 6, "full" = 7, "3 or booster" = 8)%>%
  pivot_longer(6:8, names_to = "dose", values_to = "vnum")


table(zzz$clage_vacsi)

# 0 : Tous âges
# 04 : 0-4
# 09 : 5-9
# 11 : 10-11
# 17 : 12-17
# 24 : 18-24
# 29 : 25-29
# 39 : 30-39
# 49 : 40-49
# 59 : 50-59
# 64 : 60-64
# 69 : 65-69
# 74 : 70-74
# 79 : 75-79
# 80 : 80 et 

#View(a)
View(b)



# View(france)
# colnames(france)
# 
# table(france$clage_vacsi)




