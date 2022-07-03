library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Acre (AC)

AC_1 <- read_delim("https://uofi.box.com/shared/static/9h2j58jzgqirxdp75azajhqyytlczcim.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

AC_2 <- read_delim("https://uofi.box.com/shared/static/y3dy2xlv911p23danwn3oqtah1cxp1pq.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

AC_3 <- read_delim("https://uofi.box.com/shared/static/hdgisfajmntkda6vc31s39rvbx2hodon.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))



AC_1 <- fread("https://uofi.box.com/shared/static/9h2j58jzgqirxdp75azajhqyytlczcim.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

AC_2 <- fread("https://uofi.box.com/shared/static/y3dy2xlv911p23danwn3oqtah1cxp1pq.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

AC_3 <- fread("https://uofi.box.com/shared/static/hdgisfajmntkda6vc31s39rvbx2hodon.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

library(dplyr)
library(readr)

AC1 <- rbind(AC_1,AC_2,AC_3)


AC2 <- AC1 %>%
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

AC <- AC2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Acre")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "AC")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)




write_csv(AC,"/Users/mac/Desktop/research/brazil/BR_Set_1_AC.csv")

rm(list = ls())

.rs.restartR()
#########################################################################################################
#########################################################################################################
#########################################################################################################

