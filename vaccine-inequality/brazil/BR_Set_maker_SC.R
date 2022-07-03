library(readr)
library(tibble)
library(dplyr)
library(tidyr)

# 
# https://uofi.box.com/shared/static/huozo7ujilo68iza52rxi8whz2nbsrbe.csv
# https://uofi.box.com/shared/static/vob9c0xjc3k5gvsikrxp3yt3i590maka.csv
# https://uofi.box.com/shared/static/dpycj3tyekjc78etrigdi6dk63kixqcs.csv


SC_1 <- read_delim("https://uofi.box.com/shared/static/huozo7ujilo68iza52rxi8whz2nbsrbe.csv", 
                        ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                     paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SC_2 <- read_delim("https://uofi.box.com/shared/static/vob9c0xjc3k5gvsikrxp3yt3i590maka.csv", 
                        ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                     paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SC_3 <- read_delim("https://uofi.box.com/shared/static/dpycj3tyekjc78etrigdi6dk63kixqcs.csv", 
                        ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                     paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))



Roraima <- rbind(SC_1,SC_2,SC_3)

a <- Roraima %>% filter(vacina_descricao_dose == "Única")

SC_full <- Roraima %>%
  rename(vaccine = 1, date = 2, dose =3)%>%
  mutate(vaccine=replace(vaccine, vaccine=="ASTRAZENECA", "AstraZeneca"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="ASTRAZENECA/FIOCRUZ", "AstraZeneca (Fiocruz)"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="JANSSEN", "Janssen"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="Pendente Identifica??o", "unknown"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="Pendente Identificação", "unknown"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="PFIZER", "Pfizer"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="SINOVAC/BUTANTAN", "Sinovac"))%>%
  mutate(dose=replace(dose, dose=="1ª Dose", "partial"))%>%
  mutate(dose=replace(dose, dose=="Dose", "partial"))%>%
  mutate(dose=replace(dose, dose=="Única", "partial"))%>%
  group_by(date, vaccine, dose)%>%
  summarise(vnum = n())%>%
  ungroup()

SC <- SC_full%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Santa Catarina")%>%
  mutate(vcum = 0)%>%
  mutate(vperce = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)
