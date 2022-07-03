library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Santa Catarina
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


library(dplyr)
library(readr)

SC_1<- fread("https://uofi.box.com/shared/static/huozo7ujilo68iza52rxi8whz2nbsrbe.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

SC_2<- fread("https://uofi.box.com/shared/static/vob9c0xjc3k5gvsikrxp3yt3i590maka.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

SC_3<- fread("https://uofi.box.com/shared/static/dpycj3tyekjc78etrigdi6dk63kixqcs.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

SC1 <- rbind(SC_1,SC_2,SC_3)


SC2 <- SC1 %>%
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


SC <- SC2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Santa Catarina")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "SC")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(SC,"/Users/mac/Desktop/research/brazil/BR_Set_1_SC.csv")

rm(list = ls())

.rs.restartR()
