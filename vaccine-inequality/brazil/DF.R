library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Distrito Federal (DF)


DF_1 <- read_delim("https://uofi.box.com/shared/static/zq0ow283j7w3e8buheap2ua96u0s8ipn.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

DF_2 <- read_delim("https://uofi.box.com/shared/static/yjxk9yzyr1akekn7djpk047s6pmpwil5.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

DF_3 <- read_delim("https://uofi.box.com/shared/static/cgo020q8h5nf2iy9cdi2xwc9jjjnd57o.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

DF_1 <- fread("https://uofi.box.com/shared/static/zq0ow283j7w3e8buheap2ua96u0s8ipn.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

DF_2 <- fread("https://uofi.box.com/shared/static/yjxk9yzyr1akekn7djpk047s6pmpwil5.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

DF_3 <- fread("https://uofi.box.com/shared/static/cgo020q8h5nf2iy9cdi2xwc9jjjnd57o.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

DF1 <- rbind(DF_1,DF_2,DF_3)


DF2 <- DF1 %>%
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

DF <- DF2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Distrito Federal")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "DF")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(DF,"/Users/mac/Desktop/research/brazil/BR_Set_1_DF.csv")

rm(list = ls())

.rs.restartR()
#########################################################################################################
#########################################################################################################
#########################################################################################################