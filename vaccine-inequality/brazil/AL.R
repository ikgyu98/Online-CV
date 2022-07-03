library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Alagoas (AL)


AL_1 <- read_delim("https://uofi.box.com/shared/static/yflybm6wri5t4xjcpfk7gw35apn5amv8.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

AL_2 <- read_delim("https://uofi.box.com/shared/static/hdppf1xh7xozmc2yrhkg4yrh3oui8ql1.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

AL_3 <- read_delim("https://uofi.box.com/shared/static/dsf4vhwqiajq5f31ghu5fozzjmuuy1tn.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

AL_1 <- fread("https://uofi.box.com/shared/static/yflybm6wri5t4xjcpfk7gw35apn5amv8.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

AL_2 <- fread("https://uofi.box.com/shared/static/hdppf1xh7xozmc2yrhkg4yrh3oui8ql1.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

AL_3 <- fread("https://uofi.box.com/shared/static/dsf4vhwqiajq5f31ghu5fozzjmuuy1tn.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")



AL1 <- rbind(AL_1,AL_2,AL_3)


AL2 <- AL1 %>%
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

AL <- AL2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Alagoas")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "AL")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(AL,"/Users/mac/Desktop/research/brazil/BR_Set_1_AL.csv")

rm(list = ls())

.rs.restartR()
#########################################################################################################
#########################################################################################################
#########################################################################################################