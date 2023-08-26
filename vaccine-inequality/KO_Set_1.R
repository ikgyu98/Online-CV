library(readr)
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)


korea_vaccine <- read_csv("Desktop/research/korea/korea_vaccine.csv", 
                          col_types = cols(secondCnt = col_double(), 
                                           totalSecondCnt = col_double(), accumulatedSecondCnt = col_double(), 
                                           thirdCnt = col_double(), totalThirdCnt = col_double(), 
                                           accumulatedThirdCnt = col_double()))[,-c(2,3, 6,7,8,9,10,12,13)]%>%
  separate(seq, c("date", "location"), sep = "_")

korea_vaccine$date <- ymd(korea_vaccine$date)
korea_vaccine$location[grepl("충청북도", korea_vaccine$location)] <- "Chungcheongbuk-do"
korea_vaccine$location[grepl("충청남도", korea_vaccine$location)] <- "Chungcheongnam-do"
korea_vaccine$location[grepl("강원도", korea_vaccine$location)] <- "Gangwon-do"
korea_vaccine$location[grepl("경기도", korea_vaccine$location)] <- "Gyeonggi-do"
korea_vaccine$location[grepl("경상북도", korea_vaccine$location)] <- "Gyeongsangbuk-do"
korea_vaccine$location[grepl("경상남도", korea_vaccine$location)] <- "Gyeongsangnam-do"
korea_vaccine$location[grepl("전라북도", korea_vaccine$location)] <- "Jeollabuk-do"
korea_vaccine$location[grepl("전라남도", korea_vaccine$location)] <- "Jeollanam-do"
korea_vaccine$location[grepl("대구광역시", korea_vaccine$location)] <- "Daegu"
korea_vaccine$location[grepl("대전광역시", korea_vaccine$location)] <- "Daejeon"
korea_vaccine$location[grepl("부산광역시", korea_vaccine$location)] <- "Busan"
korea_vaccine$location[grepl("서울특별시", korea_vaccine$location)] <- "Seoul"
korea_vaccine$location[grepl("세종특별자치시", korea_vaccine$location)] <- "Sejong"
korea_vaccine$location[grepl("울산광역시", korea_vaccine$location)] <- "Ulsan"
korea_vaccine$location[grepl("인천광역시", korea_vaccine$location)] <- "Incheon"
korea_vaccine$location[grepl("제주특별자치도", korea_vaccine$location)] <- "Jeju-do"
korea_vaccine$location[grepl("광주광역시", korea_vaccine$location)] <- "Gwangju"

pop <- read_csv("Desktop/research/korea/kor_pop.csv")

korea <- korea_vaccine%>%
  filter(!(grepl("전국", location)))%>%
  filter(!(grepl("기타", location)))%>%
  rename("partial" = 3, "full" = 4, "3 or booster" = 5)%>%
  pivot_longer(3:5, names_to = "dose", values_to = "vnum")%>%
  mutate(country = "Republic of Korea")%>%
  mutate(resolution = 1)%>%
  mutate(vcum = 0)%>%
  mutate(vperc = 0)%>%
  relocate(country, .before=date)%>%
  relocate(vnum, .before = vcum)%>%
  relocate(resolution, .after = country)%>%
  relocate(date, .before = dose)%>%
  inner_join(pop)%>%
  relocate(pop, .after = location)%>%
  replace(is.na(.), 0)


write_csv(korea,"/Users/mac/Desktop/research/korea/KO_Set_1.csv")
