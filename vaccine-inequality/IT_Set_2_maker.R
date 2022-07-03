library(readr)
library(tibble)
library(dplyr)
library(tidyr)


italyses <- read_csv("Desktop/research/italy/italyses.csv")%>%
  rename(location = 1)%>%
  mutate(pop = NULL)%>%
  mutate(country = "Italy")%>%
  relocate(country, .before = location)%>%
  pivot_longer(3:8, names_to = "measure", values_to = "value")
  

write_csv(italyses,"/Users/mac/Desktop/research/italy/IT_Set_2.csv")
