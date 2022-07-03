library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(convertr)




a0930_ <- read_csv("Desktop/research/kenya/30TH-SEPT-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a0930 <- read_csv("Desktop/research/kenya/30TH-SEPT-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a0930_)%>%
  mutate(date = as.Date("2021-09-30"))
  


a1007_ <- read_csv("Desktop/research/kenya/7TH-OCTOBER-2021-1.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1007 <- read_csv("Desktop/research/kenya/7TH-OCTOBER-2021-1.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1007_)%>%
  mutate(date = as.Date("2021-10-07"))



a1014_ <- read_csv("Desktop/research/kenya/14TH-OCTOBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1014 <- read_csv("Desktop/research/kenya/14TH-OCTOBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1014_)%>%
  mutate(date = as.Date("2021-10-14"))



a1021_ <- read_csv("Desktop/research/kenya/21ST-OCTOBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1021 <- read_csv("Desktop/research/kenya/21ST-OCTOBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1021_)%>%
  mutate(date = as.Date("2021-10-21"))



a1028_ <- read_csv("Desktop/research/kenya/28TH-OCTOBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1028 <- read_csv("Desktop/research/kenya/28TH-OCTOBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1028_)%>%
  mutate(date = as.Date("2021-10-28"))



a1104_ <- read_csv("Desktop/research/kenya/4TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1104 <- read_csv("Desktop/research/kenya/4TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1104_)%>%
  mutate(date = as.Date("2021-11-04"))



a1111_ <- read_csv("Desktop/research/kenya/11TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1111 <- read_csv("Desktop/research/kenya/11TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1111_)%>%
  mutate(date = as.Date("2021-11-11"))



a1118_ <- read_csv("Desktop/research/kenya/18TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1118 <- read_csv("Desktop/research/kenya/18TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1118_)%>%
  mutate(date = as.Date("2021-11-18"))



a1125_ <- read_csv("Desktop/research/kenya/25TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4))%>%
  rename(County = County_1)
a1125 <- read_csv("Desktop/research/kenya/25TH-NOVEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1125_)%>%
  rename(Dose1 = Dose2)%>%
  rename(Dose2 = FullyVac)%>%
  mutate(date = as.Date("2021-11-25"))


part1 <- rbind(a0930, a1007, a1014, a1021, a1028, a1104, a1111, a1118)%>%
  rename(Dose2 = FullyVac)
  #mutate(Dose2 = 0)

part1 <-  rbind(part1, a1125)

# part1 <- part1%>%
#   mutate(FullyVac = 0)%>%
#   mutate(Dose1of2=0)

a1202_ <- read_csv("Desktop/research/kenya/2ND-DECEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4,5))%>%
  rename(County = County_1)
a1202__ <- read_csv("Desktop/research/kenya/2ND-DECEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(6,7))%>%
  rename(County = County_2)
a1202 <- read_csv("Desktop/research/kenya/2ND-DECEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1202_)%>%
  inner_join(a1202__)%>%
  mutate(date = as.Date("2021-12-02"))



a1209_ <- read_csv("Desktop/research/kenya/9TH-DECEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(3,4,5))%>%
  rename(County = County_1)
a1209__ <- read_csv("Desktop/research/kenya/9TH-DECEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(6,7))%>%
  rename(County = County_2)
a1209 <- read_csv("Desktop/research/kenya/9TH-DECEMBER-2021.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1209_)%>%
  inner_join(a1209__)%>%
  mutate(date = as.Date("2021-12-09"))



a1216_ <- read_csv("Desktop/research/kenya/16TH-DECEMBER-2021.csv")%>%
  select(-c(8:69))%>%
  slice(-c(48))%>%
  select(c(3,4,5))%>%
  rename(County = County_1)
a1216__ <- read_csv("Desktop/research/kenya/16TH-DECEMBER-2021.csv")%>%
  select(-c(8:69))%>%
  slice(-c(48))%>%
  select(c(6,7))%>%
  rename(County = County_2)
a1216 <- read_csv("Desktop/research/kenya/16TH-DECEMBER-2021.csv")%>%
  select(-c(8:69))%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1216_)%>%
  inner_join(a1216__)%>%
  mutate(date = as.Date("2021-12-16"))



a1223_ <- read_csv("Desktop/research/kenya/23RD-DECEMBER-2021.csv")%>%
  select(-c(8:113))%>%
  slice(-c(41:121))%>%
  select(c(3,4,5))%>%
  rename(County = County_1)
a1223__ <- read_csv("Desktop/research/kenya/23RD-DECEMBER-2021.csv")%>%
  select(-c(8:113))%>%
  slice(-c(41:121))%>%
  select(c(6,7))%>%
  rename(County = County_2)
a1223 <- read_csv("Desktop/research/kenya/23RD-DECEMBER-2021.csv")%>%
  select(-c(8:113))%>%
  slice(-c(41:121))%>%
  select(c(1,2))%>%
  inner_join(a1223_)%>%
  inner_join(a1223__)%>%
  mutate(date = as.Date("2021-12-23"))

  

a1230_ <- read_csv("Desktop/research/kenya/30TH-DECEMBER-2021-1.csv")%>%
  slice(-c(48))%>%
  select(c(3,4,5))%>%
  rename(County = County_1)
a1230__ <- read_csv("Desktop/research/kenya/30TH-DECEMBER-2021-1.csv")%>%
  slice(-c(48))%>%
  select(c(6,7))%>%
  rename(County = County_2)
a1230 <- read_csv("Desktop/research/kenya/30TH-DECEMBER-2021-1.csv")%>%
  slice(-c(48))%>%
  select(c(1,2))%>%
  inner_join(a1230_)%>%
  inner_join(a1230__)%>%
  mutate(date = as.Date("2021-12-30"))


part2 <- rbind(a1202, a1209, a1216, a1223, a1230)%>%
  mutate(Dose1 = NULL)%>%
  mutate(FullyVac = NULL)%>%
  rename(Dose1 = Dose1of2)

part1 <- rbind(part1, part2)



abefore <- read_csv("Desktop/research/kenya/Kenya_Vaccine_Timeseries.csv", 
                    col_types = cols(Date = col_date(format = "%Y.%m.%d")))%>%
  select(-c(5))%>%
  rename(date= Date)%>%
  rename(Dose2 = FullyVac)%>%
  slice(-c(777:866))



total <- rbind(abefore, part1)%>%
  rename(location = County)%>%
  relocate(location, .before = date)%>%
  add_column("resolution" = 1, .before = "location")%>%
  add_column(country = "Kenya", .before="resolution")%>%
  rename("partial" = 5, "full" = 6)%>%
  pivot_longer(5:6, names_to = "dose", values_to = "vcum")%>%
  add_column(vnum = 0, .before="vcum")%>%
  add_column(vperc = 0, .after="vcum")



popul <- read_csv("Desktop/research/kenya/poverty_kenya_table_2015.csv")%>%
  select(c(1,8))%>%
  slice(-c(1:4))%>%
  mutate(population = `Population per 1000`*1000)%>%
  mutate(`Population per 1000` = NULL)%>%
  rename(location = `Residence/ County`)


Kenya_final <- left_join(total, popul, by ='location')%>%
  rename(pop = population)%>%
  relocate(pop, .after = location)

Kenya_final[is.na(Kenya_final)] <- 0

write_csv(Kenya_final,"/Users/mac/Desktop/research/kenya/KE_Set_1.csv")


# temp<-total%>%
#   filter(location == "Nairobi")





