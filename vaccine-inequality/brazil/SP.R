library(readr)
library(tibble)
library(dplyr)
library(tidyr)


#SP
SP_1 <- read_delim("https://uofi.box.com/shared/static/kp4uyv90zh6rke40dlto1quorxlc8qfu.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip(),
                                                                paciente_idade = col_skip(),
                                                                paciente_enumSexoBiologico = col_skip(),
                                                                paciente_racaCor_codigo = col_skip(),
                                                                paciente_racaCor_codigo= col_skip(),
                                                                paciente_racaCor_valor= col_skip(),
                                                                paciente_endereco_coIbgeMunicipio= col_skip(),
                                                                paciente_endereco_coPais= col_skip(),
                                                                paciente_endereco_nmMunicipio= col_skip(),
                                                                paciente_endereco_nmPais = col_skip(),
                                                                paciente_endereco_uf= col_skip(),
                                                                paciente_endereco_cep= col_skip(),
                                                                paciente_nacionalidade_enumNacionalidade= col_skip(),
                                                                estabelecimento_valor= col_skip(),
                                                                estabelecimento_razaoSocial= col_skip(),
                                                                estalecimento_noFantasia= col_skip()), trim_ws = TRUE)%>%
  select(c(2,10, 12, 13))

write_csv(SP_1 ,"/Users/mac/Desktop/research/brazil/BR_Set_1_SP_1.csv")

SP_1 <- read_delim("https://uofi.box.com/shared/static/kp4uyv90zh6rke40dlto1quorxlc8qfu.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SP_2 <- read_delim("https://uofi.box.com/shared/static/ukagtk8tpnaep3p8zzoxmy1pod78pz8k.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SP_3 <- read_delim("https://uofi.box.com/shared/static/ly50y9ddj0gam9dds9xbkl0hocf4o3yj.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

SP_1 <- read_csv("Desktop/research/brazil/BR_Set_1_SP_1.csv")
SP_2 <- read_csv("Desktop/research/brazil/BR_Set_1_SP_2.csv")
SP_3 <- read_csv("Desktop/research/brazil/BR_Set_1_SP_3.csv")

SP1 <- rbind(SP_1,SP_2,SP_3)


SP2 <- SP1 %>%
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

SP <- SP2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "SP")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "SP")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

library(dplyr)
library(readr)

SP <- read_csv("Desktop/research/brazil/BR_Set_1_SP.csv")
SP <- SP %>%
  mutate(location = "SP")


write_csv(SP_1,"/Users/mac/Desktop/research/brazil/BR_Set_1_SP_1.csv")
write_csv(SP_2,"/Users/mac/Desktop/research/brazil/BR_Set_1_SP_2.csv")
library(data.table)



library(dplyr)
library(readr)

 <- fread("", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

 <- fread("", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

 <- fread("", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")




SP2 <- SP1 %>%
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

SP <- SP2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "SP")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "SP")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)



write_csv(SP,"/Users/mac/Desktop/research/brazil/BR_Set_1_SP.csv")

rm(list = ls())
.rs.restartR()
#########################################################################################################
#########################################################################################################
#########################################################################################################