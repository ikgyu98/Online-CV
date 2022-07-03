library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Goias (GO)

GO_1 <- read_delim("https://uofi.box.com/shared/static/2h5qt6uxh61z3wqqx85lcfcpw1hqpvd6.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

GO_2 <- read_delim("https://uofi.box.com/shared/static/fyhjst5u6t02chxit775lnz289s7lluk.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

GO_3 <- read_delim("https://uofi.box.com/shared/static/fbh284ad1am2wfr2m2f11cb2pq8zh6im.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))


library(dplyr)
library(readr)

GO_1 <- fread("https://uofi.box.com/shared/static/2h5qt6uxh61z3wqqx85lcfcpw1hqpvd6.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

GO_2 <- fread("https://uofi.box.com/shared/static/fyhjst5u6t02chxit775lnz289s7lluk.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

GO_3 <- fread("https://uofi.box.com/shared/static/fbh284ad1am2wfr2m2f11cb2pq8zh6im.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

GO1 <- rbind(GO_1,GO_2,GO_3)


GO2 <- GO1 %>%
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

GO <- GO2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Goias")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "GO")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)


write_csv(GO,"/Users/mac/Desktop/research/brazil/BR_Set_1_GO.csv")

rm(list = ls())

.rs.restartR()

#########################################################################################################
#########################################################################################################
#########################################################################################################