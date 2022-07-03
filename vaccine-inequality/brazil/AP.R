library(readr)
library(tibble)
library(dplyr)
library(tidyr)

test1 <- read.table("https://uofi.box.com/shared/static/gvqghkhsx4eb7duq5fnjvr3cx28xsucx.csv", sep = ";", header = T)[, c(18,26,28,29)]

#Amapa (AP)

AP_1 <- read_delim("https://uofi.box.com/shared/static/gvqghkhsx4eb7duq5fnjvr3cx28xsucx.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(16, 24, 26, 27))

AP_2 <- read_delim("https://uofi.box.com/shared/static/4tjxbn0teq76diwkps5pfasmb1bmvwfl.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(16, 24, 26, 27))

AP_3 <- read_delim("https://uofi.box.com/shared/static/i061n6stnf7gb1yyq2e3556zi0ospp3q.csv", 
                   ";", escape_double = FALSE, col_types = cols(document_id = col_skip(), 
                                                                paciente_id = col_skip()), trim_ws = TRUE)%>%
  select(c(16, 24, 26, 27))



AP1 <- rbind(AP_1,AP_2,AP_3)


AP2 <- AP1 %>%
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
  

AP <- AP2%>%
  mutate(country = "Brazil")%>%
  mutate(resolution = 1)%>%
  mutate(location = "Amapa")%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before = date)%>%
  relocate(resolution, .before = date)%>%
  relocate(location, .before = date)%>%
  relocate(vaccine, .before = dose)%>%
  mutate(state = "AP")%>%
  relocate(state , .before = date)%>%
  relocate(code, .after = state)

write_csv(AP,"/Users/mac/Desktop/research/brazil/BR_Set_1_AP_codetest.csv")

rm(list = ls())

.rs.restartR()

#########################################################################################################
#########################################################################################################
#########################################################################################################