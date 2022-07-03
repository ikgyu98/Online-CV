library(readr)
library(tibble)
library(dplyr)
library(tidyr)




test <- read_delim("https://uofi.box.com/shared/static/x1dzw9i2lllku2a7b5jdhuzlcwyx5htc.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select(c(2, 26, 28, 29))


test1 <- test%>%
  filter(`vacina_descricao_dose` == "2º Reforço")

ast <- Roraima_1%>%
  filter(`vacina_fabricante_nome` == "ASTRAZENECA")

table(ast$vacina_descricao_dose)

astfio <- Roraima_1%>%
  filter(`vacina_fabricante_nome` == "ASTRAZENECA/FIOCRUZ")

table(astfio$vacina_descricao_dose)

jan <- Roraima_1%>%
  filter(`vacina_fabricante_nome` == "JANSSEN")

table(jan$vacina_descricao_dose)

pfizer <- Roraima_1%>%
  filter(`vacina_fabricante_nome` == "PFIZER")

table(pfizer$vacina_descricao_dose)

sino <- Roraima_1%>%
  filter(`vacina_fabricante_nome` == "SINOVAC/BUTANTAN")

table(sino$vacina_descricao_dose)






Roraima_1 <- read_delim("https://uofi.box.com/shared/static/x1dzw9i2lllku2a7b5jdhuzlcwyx5htc.csv", 
                        ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                     paciente_id = col_skip()), trim_ws = TRUE)%>%
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

Roraima_full <- Roraima %>%
  rename(vaccine = 1, date = 2, dose =3)%>%
  mutate(vaccine=replace(vaccine, vaccine=="ASTRAZENECA", "AstraZeneca"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="ASTRAZENECA/FIOCRUZ", "AstraZeneca (Fiocruz)"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="JANSSEN", "Janssen"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="Pendente Identifica??o", "unknown"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="Pendente Identificação", "unknown"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="PFIZER", "Pfizer"))%>%
  mutate(vaccine=replace(vaccine, vaccine=="SINOVAC/BUTANTAN", "Sinovac"))%>%
  mutate(dose=replace(dose, dose=="1ª Dose", "partial"))%>%
  mutate(dose=replace(dose, dose=="Dose", "partial"))%>%
  group_by(date, vaccine, dose)%>%
  summarise(vnum = n())%>%
  ungroup()

Roraima <- Roraima_full%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Roraima")%>%
  mutate(vcum = 0)%>%
  mutate(vperce = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)

table(Roraima_full$vaccine)
table(Roraima_full$dose)


