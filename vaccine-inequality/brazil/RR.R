library(readr)
library(tibble)
library(dplyr)
library(tidyr)




Roraima_1 <- read_delim("https://uofi.box.com/shared/static/x1dzw9i2lllku2a7b5jdhuzlcwyx5htc.csv", 
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
  select(c(24, 26, 27))

Roraima_2 <- read_delim("https://uofi.box.com/shared/static/42f3qg3ewxpiy44vtstnobsxa3o3qwpf.csv", 
                        ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                     paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

Roraima_3 <- read_delim("https://uofi.box.com/shared/static/0jolbv9u62vh8uavnfdpo5p7dth1brq0.csv", 
                        ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                     paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

Roraima <- rbind(Roraima_1,Roraima_2,Roraima_3)

RR_1 <- read.table("https://uofi.box.com/shared/static/x1dzw9i2lllku2a7b5jdhuzlcwyx5htc.csv", sep = ";", header = T)[, c(18,26,28,29)]

colnames(RR_1)
RR_2 <- read.table("https://uofi.box.com/shared/static/42f3qg3ewxpiy44vtstnobsxa3o3qwpf.csv", sep = ";", header = T)[, c(18,26,28,29)]


RR_3 <- read.table("https://uofi.box.com/shared/static/0jolbv9u62vh8uavnfdpo5p7dth1brq0.csv", sep = ";", header = T)[, c(18,26,28,29)]



RR1 <- rbind(RR_1,RR_2,RR_3)


RR2 <- RR1 %>%
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

RR<- RR2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Roraima")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "RR")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)


write_csv(RR,"/Users/mac/Desktop/research/brazil/BR_Set_1_RR.csv")

rm(list = ls())

.rs.restartR()
