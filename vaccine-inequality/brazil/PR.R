library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Parana(PR)


PR_1 <- read_delim("https://uofi.box.com/shared/static/tyw9sbq8pcigz6cd1hqx5krk5585i4rx.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PR_2 <- read_delim("https://uofi.box.com/shared/static/3htmvxkry9rd4zdt3agsfxoyv13gaxqi.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PR_3 <- read_delim("https://uofi.box.com/shared/static/qu6qzzkw5n0u35tfo6c8rq40rz7uio52.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

PR_1<- fread("https://uofi.box.com/shared/static/tyw9sbq8pcigz6cd1hqx5krk5585i4rx.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PR_2<- fread("https://uofi.box.com/shared/static/3htmvxkry9rd4zdt3agsfxoyv13gaxqi.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PR_3<- fread("https://uofi.box.com/shared/static/qu6qzzkw5n0u35tfo6c8rq40rz7uio52.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PR1 <- rbind(PR_1,PR_2,PR_3)


PR2 <- PR1 %>%
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

PR <- PR2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Paraná")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "PR")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(PR,"/Users/mac/Desktop/research/brazil/BR_Set_1_PR.csv")

rm(list = ls())

.rs.restartR()


#########################################################################################################
#########################################################################################################
#########################################################################################################