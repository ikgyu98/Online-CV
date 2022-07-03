library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(convertr)
data<- as.data.table(read_xlsx("bagerhat.xls", sheet = "Ark1"))



file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)
x <- lapply(df.list, data.frame)

y<- c("Bagerhat", "Bandarban", "Barguna", "Barisal" ,"Bhola" ,"Bogra" ,"Brahmanbaria" ,"Chandpur" 
      ,"Chapai Nababganj","Chittagong" ,"Chuadanga" ,"Comilla" ,"Coxs Bazar" 
      ,"Dhaka" ,"Dinajpur" ,"Faridpur" ,"Feni" ,"Gaibandha" ,"Gazipur" ,"Gopalganj" 
      ,"Habiganj" ,"Jamalpur" ,"Jessore" ,"Jhalokati" ,"Jhenaidaha" ,"Joypurhat" 
      ,"Khagrachhari" ,"Khulna" ,"Kishoreganj" ,"Kurigram" ,"Kustia" ,"Laksmipur" 
      ,"Lalmonirhat" ,"Madaripur" ,"Magura" ,"Manikganj" ,"Maulavi Bazar" ,"Meherpur" 
      ,"Munshiganj" ,"Mymensingh" ,"Naogaon" ,"Narail" ,"Narayanganj" ,"Narshingdi" 
      ,"Natore" ,"Netrokona" ,"Nilphamari" ,"Noakhali" ,"Pabna" ,"Panchagarh" 
      ,"Patuakhali" ,"Pirojpur" ,"Rajbari" ,"Rajshahi" ,"Rangamati" ,"Rangpur" 
      ,"Satkhira" ,"Shariatpur" ,"Sherpur" ,"Sirajganj" ,"Sunamganj" ,"Sylhet" 
      ,"Tangail" ,"Thakurgaon" 
)

for( i in seq_along(x)){
  
  x[[i]]$location <- rep(y[i],nrow(x[[i]]))
  
}

a<-do.call(rbind, x)

b<-a %>%
  mutate(Category = as.Date(Category, format = "%Y.%m.%d"))



pop<- read_csv("/Users/mac/Desktop/research/bangladesh/dataset/bgd_admpop_adm2_bd.csv")%>%
  select(admin2Name_en, P)%>%
  rename(location = admin2Name_en)


     
Bangladesh <- b %>%
  relocate(location, .before = Category)%>%
  mutate(All.Vaccine =NULL)%>%
  add_column(resolution = 2, .before="location") %>%
  add_column(country = "Bangladesh", .before="resolution")%>%
  rename(date = Category)%>%
  rename("partial" = X1st.doses)%>%
  rename("full" = X2nd.doses)%>%
  rename("3 or booster" = X3rd.doses)%>%
  pivot_longer(5:7, names_to = "dose", values_to = "vnum")%>%
  add_column(vcum = NA, .after = "vnum")%>%
  add_column(vperc = NA, .after = "vcum")%>%
  add_column(vaccine = NA, .after = "dose")%>%
  left_join(pop, by ='location')%>%
  rename(pop = P)%>%
  relocate(pop, .after = 'location')


#write_xlsx(Bangladesh,"/Users/mac/Desktop/research/bangladesh/BA_Set_1.xlsx")

write_csv(Bangladesh,"/Users/mac/Desktop/research/bangladesh/dataset/BA_Set_1.csv")



