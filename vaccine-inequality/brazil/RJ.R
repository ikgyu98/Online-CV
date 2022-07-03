library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Rio de Janeiro


library(dplyr)
library(readr)

RJ_1<- fread("https://uofi.box.com/shared/static/bj6pm9kcjc4o9u2o14qdehtl4bdn7wua.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

RJ_2<- fread("https://uofi.box.com/shared/static/c8g8snxpwmzo390rqz7tw583ajolgu2k.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

RJ_3<- fread("https://uofi.box.com/shared/static/ddsf0s68l0xhndfry33xzdhkfotgqi4k.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")



RJ_1 <- read_delim("https://uofi.box.com/shared/static/bj6pm9kcjc4o9u2o14qdehtl4bdn7wua.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))%>%
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

RJ_2 <- read_delim("https://uofi.box.com/shared/static/c8g8snxpwmzo390rqz7tw583ajolgu2k.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))%>%
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

RJ1 <- rbind(RJ_1,RJ_2, RJ_3)

rm(RJ_1, RJ_2)
RJ_1 <- read_csv("/Users/mac/Desktop/research/brazil/BR_Set_1_RJ1.csv")

RJ_3 <- read_delim("https://uofi.box.com/shared/static/ddsf0s68l0xhndfry33xzdhkfotgqi4k.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))%>%
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



RJ1 <- rbind(RJ_1,RJ_3)
write_csv(RJ1,"/Users/mac/Desktop/research/brazil/BR_Set_1_RJ2.csv")

RJ2 <- RJ1 %>%
  rename(code = 1, vaccine = 2, date = 3, dose =4)%>%
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
  group_by(code, date, vaccine, dose)%>%
  summarise(vnum = n())%>%
  ungroup()

RJ <- RJ2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Rio De Janeiro")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "RJ")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

# RJ2 <- RJ1 %>%
#   rename(vaccine = 1, date = 2, dose =3)%>%
#   mutate(vaccine=replace(vaccine, vaccine=="ASTRAZENECA", "AstraZeneca"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="ASTRAZENECA/FIOCRUZ", "AstraZeneca (Fiocruz)"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="JANSSEN", "Janssen"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="Pendente Identifica??o", "unknown"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="Pendente Identificação", "unknown"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="PFIZER", "Pfizer"))%>%
#   mutate(vaccine=replace(vaccine, vaccine=="SINOVAC/BUTANTAN", "Sinovac"))%>%
#   mutate(dose=replace(dose, dose=="1ª Dose", "partial"))%>%
#   mutate(dose=replace(dose, dose=="Dose", "partial"))%>%
#   mutate(dose=replace(dose, dose=="Única", "partial"))%>%
#   group_by(date, vaccine, dose)%>%
#   summarise(vnum = n())%>%
#   ungroup()

RJ <- RJ1%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Rio de Janeiro")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "RJ")%>%
  relocate(state , .before = date)

write_csv(RJ,"/Users/mac/Desktop/research/brazil/BR_Set_1_RJ.csv")

rm(list = ls())

.rs.restartR()

#########################################################################################################
#########################################################################################################
#########################################################################################################
