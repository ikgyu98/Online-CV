
library(readxl)
county_vaccination <- read_excel("Desktop/research/county vaccination.xlsx")

colnames(county_vaccination)

View(county_vaccination)

# county_vaccination$[grepl("Dose 1", ipo$business)] <- "Realtor"

dose1 <- county_vaccination[ , grepl( "Dose 1" , names( county_vaccination ) ) ]
dose2 <- county_vaccination[ , grepl( "Dose 2" , names( county_vaccination ) ) ]
dose3 <- county_vaccination[ , grepl( "Dose 3" , names( county_vaccination ) ) ]

dplyr::bind_rows(dose1)


View(dose1)
