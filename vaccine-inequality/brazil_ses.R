library(readr)
library(dplyr)
library(tidyr)


code <- read_csv("https://uofi.box.com/shared/static/7ecqrytkky5x650lg61qc8j93ts7bo8j.csv")%>%
  select(c(1,2,4))%>%
  rename(stateac = 2, code = 3)%>%
  mutate(code = substring(code,1, nchar(code)-1))

code$key <- paste(code$name, code$stateac)

ses <- read_csv("Desktop/vaccine_inequality/ses.csv")


ses$key <- paste(ses$name, ses$stateac)


# write_csv(testtests, "Desktop/vaccine_inequality/missing2.csv")


final <- ses %>% inner_join(code, by = "key")%>%
  select(c(1,2,3,4,5,6,10))%>%
  mutate(population = as.integer(population))%>%
  rename(location = 1, stateac = 6)%>%
  relocate(state, .after = location)%>%
  relocate(stateac, .after = state)%>%
  relocate(code, .after = stateac)%>%
  pivot_longer(5:7, names_to = "measure", values_to = "value")

write_csv(final, "Desktop/vaccine_inequality/BR_Set_2.csv")
  
  
