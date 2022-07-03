library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)
??fread

#Minas Gerais(MG)
test <- fread("https://uofi.box.com/shared/static/pww5uogdw8hx38egrbyi5hfk78davkcc.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

test <- fread("https://uofi.box.com/shared/static/pww5uogdw8hx38egrbyi5hfk78davkcc.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

test <- fread("https://uofi.box.com/shared/static/pww5uogdw8hx38egrbyi5hfk78davkcc.csv", select = c(
  "estabelecimento_municipio_codigo", "vacina_fabricante_nome", "vacina_dataAplicacao", "vacina_descricao_dose"
), sep = ";")

MG_1 <- read_delim("https://uofi.box.com/shared/static/pww5uogdw8hx38egrbyi5hfk78davkcc.csv", 
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
colnames(MG_1)

MG_1 <- MG_1 %>%
  select(c(2,10, 12, 13))
write_csv(MG_1 ,"/Users/mac/Desktop/research/brazil/BR_Set_1_MG_1.csv")


MG_2 <- read_delim("https://uofi.box.com/shared/static/z5llp1bvx31grnwejhesyag5iygdu275.csv", 
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

write_csv(MG_2 ,"/Users/mac/Desktop/research/brazil/BR_Set_1_MG_2.csv")

MG_2 <- read_delim("https://uofi.box.com/shared/static/z5llp1bvx31grnwejhesyag5iygdu275.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

MG_3 <- read_delim("https://uofi.box.com/shared/static/2id3qhtuaqyy2l1vr9yen3gsya76r2jp.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(24, 26, 27))

MG_3 <- read_delim("https://uofi.box.com/shared/static/2id3qhtuaqyy2l1vr9yen3gsya76r2jp.csv", 
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

write_csv(MG_3 ,"/Users/mac/Desktop/research/brazil/BR_Set_1_MG_3.csv")



MG_1<- read.table("https://uofi.box.com/shared/static/pww5uogdw8hx38egrbyi5hfk78davkcc.csv", sep = ";", fill = TRUE, header = T)[, c(18,26,28,29)]

MG_2<- read.table("https://uofi.box.com/shared/static/z5llp1bvx31grnwejhesyag5iygdu275.csv", sep = ";", header = T)[, c(18,26,28,29)]


MG_3<- read.table("https://uofi.box.com/shared/static/2id3qhtuaqyy2l1vr9yen3gsya76r2jp.csv", sep = ";", header = T)[, c(18,26,28,29)]



library(readr)

MG_1 <- read_csv("Desktop/research/brazil/BR_Set_1_MG_1.csv")
MG_2 <- read_csv("Desktop/research/brazil/BR_Set_1_MG_2.csv")
MG_3 <- read_csv("Desktop/research/brazil/BR_Set_1_MG_3.csv")

MG1 <- rbind(MG_1,MG_2,MG_3)
MG2 <- MG1 %>%
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

MG <- MG2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Minas Gerais")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "MG")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(MG,"/Users/mac/Desktop/research/brazil/BR_Set_1_MG.csv")

rm(list = ls())

.rs.restartR()
#########################################################################################################
#########################################################################################################
#########################################################################################################