library(readr)
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)


korses <- read_csv("Desktop/research/korea/KOR SES.csv", 
                                 col_types = cols(`Gross regional income per capita` = col_integer(), 
                                                  `personal income` = col_integer(), 
                                                  employment = col_double(), unemployment = col_double(), 
                                                  `recipient of basic living` = col_integer(), 
                                                  `household income` = col_integer()))%>%
  slice(-18)%>%
  rename(income2 = 2, income1 = 3, labor1 = 4, labor2 = 5, income6 = 6, income5 = 7)%>%
  pivot_longer(cols = c(income2, income1, labor1, labor2, income6, income5), names_to = "measure", values_to = "value")%>%
  mutate(country = "Republic of Korea")%>%
  relocate(country, .before = location)


write_csv(korses,"/Users/mac/Desktop/research/korea/KO_Set_2.csv")
