library(readr)
library(dplyr)
library(sprintf)
brazilses <- read_csv("Desktop/research/brazilses.csv")%>%
  rename(name = `municipio`)%>%
  rename(state = `region_id`)

brazilcovid <- read_delim("Desktop/research/brazilcovid.csv", 
                          ";", escape_double = FALSE, col_types = cols(DOSE = col_character(), 
                                                                       CD_MUNICIPIO = col_character(), CD_REGIAO_COVID = col_character(), 
                                                                       DATA = col_date(format = "%Y-%m-%d")), 
                          trim_ws = TRUE)%>%
  rename(areacode = `CD_MUN_RESIDENCIA`)

rescode <- read_csv("https://raw.githubusercontent.com/datasets-br/city-codes/master/data/br-city-codes.csv")%>%
  select(1,2,4)%>%
  rename(code = 3)%>%
  mutate(code = code%/%10)%>%
  mutate(areacode = as.character(code))%>%
  mutate(code = NULL)


temp<-brazilcovid%>%
  filter(!`areacode`=="None")

temp1<-left_join(temp, rescode, by = "areacode")
temp2<-left_join(temp1, brazilses, by = c("name", "state"))%>%
  select(-c(1,2,4,5,6,7,8))%>%
  select(c(1,2, 7,8,9,10,11,12,13,14,15))%>%
  rename(date = DATA, areaname = name, state_abbrev = region, poverty = pooverty, poverty_percentage  = pooverty_percentage )%>%
  relocate(areaname, .before=COUNT)%>%
  relocate(state, .before=COUNT)%>%
  relocate(state_abbrev, .before=COUNT)%>%
  relocate(areaname, .before=COUNT)%>%
  relocate(DOSE, .before = COUNT)



write_csv(temp2,"/Users/mac/Desktop/research/brazil/BR_TEMP_Set.csv")




# 
# str(temp2, temp.len=3)
# rs <- data.frame(table(rescode$name))
# test[rs$Freq >1, ]
# 
# Map(function(x,y) assign(x,y, envir = .GlobalEnv), x = sprintf("df%s",seq(1:27)), y = split(temp1,temp1$state))
# 
# Map(function(x,y) assign(x,y, envir = .GlobalEnv), x = sprintf("dfdf%s",seq(1:27)), y = split(temp1,temp1$state))
# xxxx<-table(brazilses$name)
# xxx<-left_join(df27, brazilses, by = "name")
# xxxx<-left_join(df27, brazilses, by = c("name", "state"))
# xx<-brazilses%>%
#   filter(region_id == "TO")
# 
# 
# table(brazilses$name=="Alvorada")
# table(TO$name)
# table(xxx$name)
# table(xx$name)
# 
# statenames <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI", 
#                 "PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
# statenames <- c("df1","df2","df3","df4","df5","df6","df7","df8","df9","df10","df11","df12","df13","df14","df15","df16","df17", 
#                 "df18","df19","df20","df21","df22","df23","df24","df25","df26","df27")
# 
# for(i in seq_along(1:27)){
#   as.data.frame(paste0("dfdf", i))  # saves to environment
# }
# n <- 27
# lst <- replicate(n,data.frame(y=character(), x=numeric(),
#                               stringsAsFactors=FALSE), simplify=FALSE)
# names(lst) <- paste0('dfdf', 1:n)
# 
# df.list <- list(`AC`,`AL`,`AM`,`AP`,`BA`,`CE`,`DF`,`ES`,`GO`,`MA`,`MG`,`MS`,`MT`,`PA`,`PB`,`PE`,`PI`, 
#                 `PR`,`RJ`,`RN`,`RO`,`RR`,`RS`,`SC`,`SE`,`SP`,`TO`)
# 
# result = list()
# nmy <- paste0('y', 1:n)
# nmx <- paste0('x', 1:n)
# mm<-data.frame(matrix(0, ncol = 22, nrow = 1))
# for (i in seq_along(1:27)){
# 
#   result[i] <-left_join(get(paste0("df",i)), brazilses, by = c("name","state"))
# 
# }
# 
# split(temp1,temp1$state)
# warnings()
# 
# temp2 <- temp1 %>% 
#   group_by(state) %>%
#   group_split() %>% 
#   map_df(~ {
#     .x %>% 
#       smooth_new_cases() %>% 
#       compute_likelihood() %>% 
#       compute_posterior() %>% 
#       estimate_rt()
#   }) %>%
#   ungroup()
# 
