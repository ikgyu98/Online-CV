library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Maranhao (MA)


MA_1 <- read_delim("https://uofi.box.com/shared/static/l0nokxz7yxphmlc1y2al0h58bncvcpz5.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

MA_2 <- read_delim("https://uofi.box.com/shared/static/xdl7dm93a4fbz3rnt2f6w5jw1hg1ok4u.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

MA_3 <- read_delim("https://uofi.box.com/shared/static/ozn5xaxxxjohbysizz4jjy0lscm10n7n.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))



library(dplyr)
library(readr)

MA_1 <- fread("https://uofi.box.com/shared/static/l0nokxz7yxphmlc1y2al0h58bncvcpz5.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

MA_2 <- fread("https://uofi.box.com/shared/static/xdl7dm93a4fbz3rnt2f6w5jw1hg1ok4u.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

MA_3 <- fread("https://uofi.box.com/shared/static/ozn5xaxxxjohbysizz4jjy0lscm10n7n.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")


MA1 <- rbind(MA_1,MA_2,MA_3)


MA2 <- MA1 %>%
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

MA <- MA2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Maranhao")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "MA")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(MA,"/Users/mac/Desktop/research/brazil/BR_Set_1_MA.csv")

rm(list = ls())

.rs.restartR()


#########################################################################################################
#########################################################################################################
#########################################################################################################