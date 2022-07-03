library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Mato Grosso do Sul (MS)

MS_1 <- read_delim("https://uofi.box.com/shared/static/qmv8zncds44b95tswq26yrno92vt13rl.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

MS_2 <- read_delim("https://uofi.box.com/shared/static/yk94f8ir7ikxtzua1geaikdfrh25f4rr.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

MS_3 <- read_delim("https://uofi.box.com/shared/static/p8rzurzyqd2frcnjy7vi0muewon7udea.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))


library(dplyr)
library(readr)

MS_1<- fread("https://uofi.box.com/shared/static/qmv8zncds44b95tswq26yrno92vt13rl.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

MS_2<- fread("https://uofi.box.com/shared/static/yk94f8ir7ikxtzua1geaikdfrh25f4rr.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

MS_3<- fread("https://uofi.box.com/shared/static/p8rzurzyqd2frcnjy7vi0muewon7udea.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")



MS1 <- rbind(MS_1,MS_2,MS_3)


MS2 <- MS1 %>%
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

MS <- MS2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Mato Grosso Do Sul")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "MS")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(MS,"/Users/mac/Desktop/research/brazil/BR_Set_1_MS.csv")

rm(list = ls())

.rs.restartR()


#########################################################################################################
#########################################################################################################
#########################################################################################################