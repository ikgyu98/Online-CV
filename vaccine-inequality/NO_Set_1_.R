library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyverse)

# county_vaccination_all is the exact dataframe extracted from the file name county vaccination
# county_vaccination is a dataframe with date column extracted
# date is a dataframe which has been extracted from the county_vaccination_all dataframe
# df is a dataframe which will eventually be outputted as an excel file with requested/required format.


county_vaccination_all <- read_excel("Desktop/county vaccination.xlsx")
county_vaccination <- county_vaccination_all[-1]
date <- county_vaccination_all[1]
colnames(county_vaccination)
df <- county_vaccination





for (i in seq_along(df)){
  df[208+i] <- ifelse(grepl("Dose 1,", colnames(df)[i]), "partial",
                      ifelse(grepl("Dose 2,", colnames(df)[i]), "full", "3 or booster"))
  i = i+1
}

i=1
df1 <- county_vaccination

for (i in seq_along(df1)){
  df[416+i] <- ifelse(grepl("Pfizer)", colnames(df1)[i]), "Pfizer",
                      ifelse(grepl("CoronaVac", colnames(df1)[i]), "CoronaVac",
                             ifelse(grepl("Covishield", colnames(df1)[i]), "Covishield",
                                    ifelse(grepl("Janssen", colnames(df1)[i]), "Janssen",
                                           ifelse(grepl("Moderna", colnames(df1)[i]), "Moderna",
                                                  ifelse(grepl("CureVac", colnames(df1)[i]), "CureVac",
                                                         "AstraZeneca"))))))
  i = i+1
}


i=1
df2 <- county_vaccination

for (i in seq_along(df2)){
  df[624+i] <- ifelse(grepl("Agder", colnames(df2)[i]), "Agder",
                      ifelse(grepl("oppgitt", colnames(df2)[i]), "Ikke oppgitt",
                             ifelse(grepl("Innlandet", colnames(df2)[i]), "Innlandet",
                                    ifelse(grepl("Romsdal", colnames(df2)[i]), "Møre og Romsdal",
                                           ifelse(grepl("Nordland", colnames(df2)[i]), "Nordland",
                                                  ifelse(grepl("Oslo", colnames(df2)[i]), "Oslo (f)",
                                                         ifelse(grepl("Rogaland", colnames(df2)[i]), "Rogaland",
                                                                ifelse(grepl("Svalbard", colnames(df2)[i]), "Svalbard",
                                                                       ifelse(grepl("Finnmark", colnames(df2)[i]), "Troms og Finnmark",
                                                                              ifelse(grepl("Trøndelag", colnames(df2)[i]), "Trøndelag",
                                                                                     ifelse(grepl("Telemark", colnames(df2)[i]), "Vestfold og Telemark",
                                                                                            ifelse(grepl("Vestland", colnames(df2)[i]), "Vestland",
                                                                                                   "Viken"))))))))))))
  i = i+1
}



# test is a dataframe with population
# testdose is a dataframe with dose number
# testdosetype is a dataframe with type of the vaccine
# testcity is a dataframe with cities




test <- df[1:208]
testdose <- df[209:416]
testdosetype <- df[417:624]
testcity <- df[625:832]




test1 <- data.frame('Covid-19, Dose 1, Agder, Comirnaty (BioNTech og Pfizer)'=unlist(test, use.names = FALSE))
testdose1 <- data.frame('209'=unlist(testdose, use.names = FALSE))
testdosetype1 <- data.frame('417'=unlist(testdosetype, use.names = FALSE))
testcity1 <- data.frame('625'=unlist(testcity, use.names = FALSE))


test_date <- matrix(rep(date,each=832),nrow=832)
test_date <- data.frame(date,i=rep(1:832,ea=NROW(date)))
test_date <- test_date[-2]



test_col<- cbind(testcity1, test_date, testdose1, testdosetype1, test1)



colnames(test_col) <- c('location', 'date', 'dose', 'vaccine', 'vnum')

#View(test_col)

test_col <- test_col %>%
  add_column(resolution = 1, .before="location") %>%
  add_column(country = "Norway", .before="resolution") %>%
  add_column(vcum = NA, .after="vnum") %>%
  add_column(vperc = NA, .after="vcum")


test_col <- test_col %>%
  mutate(pop = case_when(
    endsWith(location, "Viken") ~ "1241165",
    endsWith(location, "(f)") ~ "693494",
    endsWith(location, "Vestland") ~ "636531",
    endsWith(location, "Rogaland") ~ "479892",
    endsWith(location, "Trøndelag") ~ "468702",
    endsWith(location, "Telemark") ~ "419396",
    endsWith(location, "Innlandet") ~ "371385",
    endsWith(location, "Agder") ~ "307231",
    endsWith(location, "Romsdal") ~ "265238",
    endsWith(location, "Finnmark") ~ "243311",
    endsWith(location, "Nordland") ~ "241235"
  ))
test_col <- test_col %>% relocate(pop, .before = date)


Agder <- subset(test_col, location == "Agder")
dfAgder <- data.frame(unclass(table(Agder$vaccine, Agder$dose)))%>%
  add_row( .before= 1)

Ikkeoppgitt<- subset(test_col, location == "Ikke oppgitt")
dfIkkeoppgitt <- data.frame(unclass(table(Ikkeoppgitt$vaccine, Ikkeoppgitt$dose)))%>%
  add_row( .before= 1)

Innlandet<- subset(test_col, location == "Innlandet")
dfInnlandet <- data.frame(unclass(table(Innlandet$vaccine, Innlandet$dose)))%>%
  add_row( .before= 1)

MøreogRomsdal<- subset(test_col, location == "Møre og Romsdal")
dfMøreogRomsdal <- data.frame(unclass(table(MøreogRomsdal$vaccine, MøreogRomsdal$dose)))%>%
  add_row( .before= 1)

Nordland<- subset(test_col, location == "Nordland")
dfNordland <- data.frame(unclass(table(Nordland$vaccine, Nordland$dose)))%>%
  add_row( .before= 1)

Oslof<- subset(test_col, location == "Oslo (f)")
dfOslof <- data.frame(unclass(table(Oslof$vaccine, Oslof$dose)))%>%
  add_row( .before= 1)

Rogaland<- subset(test_col, location == "Rogaland")
dfRogaland <- data.frame(unclass(table(Rogaland$vaccine, Rogaland$dose)))%>%
  add_row( .before= 1)
  

Svalbard<- subset(test_col, location == "Svalbard")
dfSvalbard <- data.frame(unclass(table(Svalbard$vaccine, Svalbard$dose)))%>%
  add_row( .before= 1)

TromsogFinnmark<- subset(test_col, location == "Troms og Finnmark")
dfTromsogFinnmark <- data.frame(unclass(table(TromsogFinnmark$vaccine, TromsogFinnmark$dose)))%>%
  add_row( .before= 1)

Trøndelag<- subset(test_col, location == "Trøndelag")
dfTrøndelag <- data.frame(unclass(table(Trøndelag$vaccine, Trøndelag$dose)))%>%
  add_row( .before= 1)

VestfoldogTelemark<- subset(test_col, location == "Vestfold og Telemark")
dfVestfoldogTelemark <- data.frame(unclass(table(VestfoldogTelemark$vaccine, VestfoldogTelemark$dose)))%>%
  add_row( .before= 1)

Vestland<- subset(test_col, location == "Vestland")
dfVestland <- data.frame(unclass(table(Vestland$vaccine, Vestland$dose)))%>%
  add_row( .before= 1)

Viken<- subset(test_col, location == "Viken")
dfViken <- data.frame(unclass(table(Viken$vaccine, Viken$dose)))%>%
  add_row( .before= 1)

eh <- rbind(dfAgder, dfIkkeoppgitt, dfInnlandet, dfMøreogRomsdal, dfNordland, dfOslof, dfRogaland, dfSvalbard, dfTromsogFinnmark, 
            dfTrøndelag, dfVestfoldogTelemark, dfVestland, dfViken)

View(eh)
# write_xlsx(Agder,"Desktop/Agder.xlsx")
# write_xlsx(Ikkeoppgitt,"Desktop/Ikkeoppgitt.xlsx")
# write_xlsx(Innlandet,"Desktop/Innlandet.xlsx")
# write_xlsx(MøreogRomsdal,"Desktop/MøreogRomsdal.xlsx")
# write_xlsx(Nordland,"Desktop/Nordland.xlsx")
# write_xlsx(Oslof,"Desktop/Oslof.xlsx")
# write_xlsx(Rogaland,"Desktop/Rogaland.xlsx")
# write_xlsx(Svalbard,"Desktop/Svalbard.xlsx")
# write_xlsx(TromsogFinnmark,"Desktop/TromsogFinnmark.xlsx")
# write_xlsx(Trøndelag,"Desktop/Trøndelag.xlsx")
# write_xlsx(VestfoldogTelemark,"Desktop/VestfoldogTelemark.xlsx")
# write_xlsx(Vestland,"Desktop/Vestland.xlsx")
# write_xlsx(Viken,"Desktop/Viken.xlsx")
# 


nrow(Agder[Agder$dose == "partial" & Agder$vaccine == "Pfizer", ])  
nrow(Agder[Agder$dose == "partial" & Agder$vaccine == "AstraZeneca", ])  
nrow(Agder[Agder$dose == "partial" & Agder$vaccine == "CoronaVac", ])  
nrow(Agder[Agder$dose == "partial" & Agder$vaccine == "Covishield", ])  
nrow(Agder[Agder$dose == "partial" & Agder$vaccine == "Janssen", ])  
nrow(Agder[Agder$dose == "partial" & Agder$vaccine == "Moderna", ])  

nrow(Agder[Agder$dose == "full" & Agder$vaccine == "Pfizer", ])  
nrow(Agder[Agder$dose == "full" & Agder$vaccine == "AstraZeneca", ])  
nrow(Agder[Agder$dose == "full" & Agder$vaccine == "CoronaVac", ])  
nrow(Agder[Agder$dose == "full" & Agder$vaccine == "Covishield", ])  
nrow(Agder[Agder$dose == "full" & Agder$vaccine == "Janssen", ])  
nrow(Agder[Agder$dose == "full" & Agder$vaccine == "Moderna", ])  

nrow(Agder[Agder$dose == "3 or booster" & Agder$vaccine == "Pfizer", ])  
nrow(Agder[Agder$dose == "3 or booster" & Agder$vaccine == "AstraZeneca", ])  
nrow(Agder[Agder$dose == "3 or booster" & Agder$vaccine == "CoronaVac", ])  
nrow(Agder[Agder$dose == "3 or booster" & Agder$vaccine == "Covishield", ])  
nrow(Agder[Agder$dose == "3 or booster" & Agder$vaccine == "Janssen", ])  
nrow(Agder[Agder$dose == "3 or booster" & Agder$vaccine == "Moderna", ])  





#write_xlsx(test_col,"Desktop/for county vaccination filling.xlsx")

#table(test_col["location"])






# "Northern Norway" ~ "Nordland,Troms og Finnmark",
# "South-Eastern Norway" ~ "Viken,Oslo (f),Innlandet,Vestfold og Telemark",
# "Western Norway" ~ "Rogaland,Vestland,Møre og Romsdal"
