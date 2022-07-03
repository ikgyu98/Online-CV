library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Para (PA)


PA_1 <- read_delim("https://uofi.box.com/shared/static/07ldeahbw747m3od9qeawkwefjl59yh1.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PA_2 <- read_delim("https://uofi.box.com/shared/static/5k2nybfkzwb3ss8820cpg51mw5wlcjmf.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

PA_3 <- read_delim("https://uofi.box.com/shared/static/ybyj9r7bt89ezk11von29fot7lfm68tt.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

PA_1<- fread("https://uofi.box.com/shared/static/07ldeahbw747m3od9qeawkwefjl59yh1.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PA_2<- fread("https://uofi.box.com/shared/static/5k2nybfkzwb3ss8820cpg51mw5wlcjmf.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

PA_3<- fread("https://uofi.box.com/shared/static/ybyj9r7bt89ezk11von29fot7lfm68tt.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")


PA1 <- rbind(PA_1,PA_2,PA_3)


PA2 <- PA1 %>%
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

PA <- PA2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Para")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "PA")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(PA,"/Users/mac/Desktop/research/brazil/BR_Set_1_PA.csv")

rm(list = ls())

.rs.restartR()


#########################################################################################################
#########################################################################################################
#########################################################################################################
