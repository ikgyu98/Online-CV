library(readr)
library(tibble)
library(dplyr)
library(tidyr)



#Pernambuco (PE)

PE_1 <- read_delim("https://uofi.box.com/shared/static/gtxidt9ql7tbvrwyuobsxuemp5u6318q.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PE_2 <- read_delim("https://uofi.box.com/shared/static/z0x9bakn8aejqsiznek2efu8byk43n05.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PE_3 <- read_delim("https://uofi.box.com/shared/static/3dbs41mc5m9mjz9rgcxalkfnb7g67s9y.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

PE_1<- fread("https://uofi.box.com/shared/static/gtxidt9ql7tbvrwyuobsxuemp5u6318q.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PE_2<- fread("https://uofi.box.com/shared/static/z0x9bakn8aejqsiznek2efu8byk43n05.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PE_3<- fread("https://uofi.box.com/shared/static/3dbs41mc5m9mjz9rgcxalkfnb7g67s9y.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")


PE1 <- rbind(PE_1,PE_2,PE_3)


PE2 <- PE1 %>%
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

PE <- PE2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Pernambuco")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "PE")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(PE,"/Users/mac/Desktop/research/brazil/BR_Set_1_PE.csv")

rm(list = ls())

.rs.restartR()

#########################################################################################################
#########################################################################################################
#########################################################################################################