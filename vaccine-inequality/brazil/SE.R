library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#SE


SE_1 <- read_delim("https://uofi.box.com/shared/static/5vjyq3c6pnpiplyjyfhrf50gvouvxgvn.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SE_2 <- read_delim("https://uofi.box.com/shared/static/9eqhbm2fnzoyhnouaz1q773wk6tdwsn0.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SE_3 <- read_delim("https://uofi.box.com/shared/static/tgm9dibt4n2z5rqqyaukbxivk21oi8ic.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

SE_1<- fread("https://uofi.box.com/shared/static/5vjyq3c6pnpiplyjyfhrf50gvouvxgvn.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

SE_2<- fread("https://uofi.box.com/shared/static/9eqhbm2fnzoyhnouaz1q773wk6tdwsn0.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

SE_3<- fread("https://uofi.box.com/shared/static/tgm9dibt4n2z5rqqyaukbxivk21oi8ic.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")


SE1 <- rbind(SE_1,SE_2,SE_3)


SE2 <- SE1 %>%
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

SE <- SE2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "SE")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "SE")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(SE,"/Users/mac/Desktop/research/brazil/BR_Set_1_SE.csv")

rm(list = ls())

.rs.restartR()


#########################################################################################################
#########################################################################################################
#########################################################################################################