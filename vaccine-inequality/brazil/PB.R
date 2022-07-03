
library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Paraiba (PB)

PB_1 <- read_delim("https://uofi.box.com/shared/static/zudnmesy746hlm7h0qa1vnoldzkru3ov.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PB_2 <- read_delim("https://uofi.box.com/shared/static/aracra5kd99hofbgbmgvi47ntsrv4mm0.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PB_3 <- read_delim("https://uofi.box.com/shared/static/kp8jp1gvgouamxsrzlftp4hcbfdlhexc.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

PB_1<- fread("https://uofi.box.com/shared/static/zudnmesy746hlm7h0qa1vnoldzkru3ov.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PB_2<- fread("https://uofi.box.com/shared/static/aracra5kd99hofbgbmgvi47ntsrv4mm0.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PB_3<- fread("https://uofi.box.com/shared/static/kp8jp1gvgouamxsrzlftp4hcbfdlhexc.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PB1 <- rbind(PB_1,PB_2,PB_3)


PB2 <- PB1 %>%
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

PB <- PB2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Paraiba")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "PB")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(PB,"/Users/mac/Desktop/research/brazil/BR_Set_1_PB.csv")

rm(list = ls())

.rs.restartR()

#########################################################################################################
#########################################################################################################
#########################################################################################################