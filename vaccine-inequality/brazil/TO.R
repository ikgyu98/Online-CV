
library(readr)
library(tibble)
library(dplyr)
library(tidyr)



#TO

TO_1 <- read_delim("https://uofi.box.com/shared/static/89fmdrh9b34xg47u5y75rrakvv2jdza5.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

TO_2 <- read_delim("https://uofi.box.com/shared/static/s67faqquzaqrhpa31b6un6tvylxntryl.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

TO_3 <- read_delim("https://uofi.box.com/shared/static/jrdswnvnk5zsedi3zck5gsznwypeodqk.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

library(dplyr)
library(readr)

TO_1<- fread("https://uofi.box.com/shared/static/89fmdrh9b34xg47u5y75rrakvv2jdza5.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

TO_2<- fread("https://uofi.box.com/shared/static/s67faqquzaqrhpa31b6un6tvylxntryl.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

TO_3<- fread("https://uofi.box.com/shared/static/jrdswnvnk5zsedi3zck5gsznwypeodqk.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

TO1 <- rbind(TO_1,TO_2,TO_3)


TO2 <- TO1 %>%
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

TO <- TO2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "TO")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "TO")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(TO,"/Users/mac/Desktop/research/brazil/BR_Set_1_TO.csv")

rm(list = ls())

.rs.restartR()

#########################################################################################################
#########################################################################################################
#########################################################################################################