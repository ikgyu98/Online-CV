library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Ceara (CE)


CE_1 <- read_delim("https://uofi.box.com/shared/static/x4osby3fmgfyvqnef0mlqs4jr6l3gris.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

CE_2 <- read_delim("https://uofi.box.com/shared/static/w0e3ewqvz785l6wwjjty0tofpmc6qr1u.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

CE_3 <- read_delim("https://uofi.box.com/shared/static/zorx58jacb302u7tvmp1b54yu3vjilig.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

CE_1 <- fread("https://uofi.box.com/shared/static/x4osby3fmgfyvqnef0mlqs4jr6l3gris.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

CE_2 <- fread("https://uofi.box.com/shared/static/w0e3ewqvz785l6wwjjty0tofpmc6qr1u.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

CE_3 <- fread("https://uofi.box.com/shared/static/zorx58jacb302u7tvmp1b54yu3vjilig.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")


CE1 <- rbind(CE_1,CE_2,CE_3)


CE2 <- CE1 %>%
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

CE <- CE2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Ceara")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "CE")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(CE,"/Users/mac/Desktop/research/brazil/BR_Set_1_CE.csv")

rm(list = ls())

.rs.restartR()
#########################################################################################################
#########################################################################################################
#########################################################################################################