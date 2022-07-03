library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#Rio Grande Do Sul (RS)


RS_1 <- read_delim("https://uofi.box.com/shared/static/t51cxq81x1um3ghj8e4ubxigemn60qre.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

RS_2 <- read_delim("https://uofi.box.com/shared/static/dqtlmru97aarzixztnydtp0knoyy35xt.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

RS_3 <- read_delim("https://uofi.box.com/shared/static/zk9ov90hbwh5v76jgeajss51zonxaahs.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))



library(dplyr)
library(readr)

RS_1<- fread("https://uofi.box.com/shared/static/t51cxq81x1um3ghj8e4ubxigemn60qre.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

RS_2<- fread("https://uofi.box.com/shared/static/dqtlmru97aarzixztnydtp0knoyy35xt.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

RS_3<- fread("https://uofi.box.com/shared/static/zk9ov90hbwh5v76jgeajss51zonxaahs.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")


RS1 <- rbind(RS_1,RS_2,RS_3)


RS2 <- RS1 %>%
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

RS <- RS2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Rio Grande Do Sul")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "RS")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(RS,"/Users/mac/Desktop/research/brazil/BR_Set_1_RS.csv")

rm(list = ls())

.rs.restartR()

