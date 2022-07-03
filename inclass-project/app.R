library(shiny)
library(tidyverse)
library(bslib)
library(readr)
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(formattable)
library(DT)
library(lubridate)
library(stringr)
library(EpiEstim)
library(incidence)
library(remotes)
library(sars2pack)
library(random)
library(purrr)
library(hdi)
library(zoo)
library(smoother)
library(data.table)

################################################################################################################################################################

###############################################Date Cleaning Section#############################################################################################

###############################################Date Cleaning Section############################################################################################

################################################################################################################################################################




globe_positive<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                          col_types = cols(`Province/State` = col_skip(), 
                                           Lat = col_skip(), Long = col_skip()))%>%
    summarise_at(2:825, sum, na.rm = TRUE)%>%
    mutate(`location` = "world", .before = "1/22/20")%>%
    pivot_longer(!`location`, names_to = "date", values_to = "cum")%>%
    mutate(confirmed = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`location` = NULL, `cum` = NULL)

globe_death<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                       col_types = cols(`Province/State` = col_skip(), 
                                        Lat = col_skip(), Long = col_skip()))%>%
    summarise_at(2:825, sum, na.rm = TRUE)%>%
    mutate(`location` = "world", .before = "1/22/20")%>%
    pivot_longer(!`location`, names_to = "date", values_to = "cum")%>%
    mutate(death = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`location` = NULL, `cum` = NULL)

globe_recovered<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                           col_types = cols(`Province/State` = col_skip(), 
                                            Lat = col_skip(), Long = col_skip()))%>%
    summarise_at(2:825, sum, na.rm = TRUE)%>%
    mutate(`location` = "world", .before = "1/22/20")%>%
    pivot_longer(!`location`, names_to = "date", values_to = "cum")%>%
    mutate(recovered = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`location` = NULL, `cum` = NULL)

world_pdr_ <- left_join(globe_positive, globe_death, by = "date")
world_pdr <- left_join(world_pdr_, globe_recovered, by = "date")%>%
    arrange(desc("date"))

################################################################################################################################################################

kor_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
                      col_types = cols(`Province/State` = col_skip(), 
                                       Lat = col_skip(), Long = col_skip()))%>%
    filter(`Country/Region` == "Korea, South")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(death = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)



kor_positive <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                         col_types = cols(`Province/State` = col_skip(), 
                                          Lat = col_skip(), Long = col_skip()))%>%
    filter(`Country/Region` == "Korea, South")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(confirmed = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)

for_kor_rt <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                       col_types = cols(`Province/State` = col_skip(), 
                                        Lat = col_skip(), Long = col_skip()))%>%
    filter(`Country/Region` == "Korea, South")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(incidence = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)


kor_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", 
                          col_types = cols(`Province/State` = col_skip(), 
                                           Lat = col_skip(), Long = col_skip()))%>%
    filter(`Country/Region` == "Korea, South")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(recovered = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)

kor_recovered[47,2] <-56

#56 in 3/8/20 temporarily

kor_pdr_ <- left_join(kor_positive, kor_death, by = "date")
kor_pdr <- left_join(kor_pdr_, kor_recovered, by = "date")%>%
    arrange(desc("date"))

################################################################################################################################################################

us_positive <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                        col_types = cols(`Province/State` = col_skip(), 
                                         Lat = col_skip(), Long = col_skip()))%>%      
    filter(`Country/Region` == "US")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(confirmed = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)

us_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
                     col_types = cols(`Province/State` = col_skip(), 
                                      Lat = col_skip(), Long = col_skip()))%>%      
    filter(`Country/Region` == "US")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(death = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)

us_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", 
                         col_types = cols(`Province/State` = col_skip(), 
                                          Lat = col_skip(), Long = col_skip()))%>%      
    filter(`Country/Region` == "US")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(recovered = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)

us_recovered[112,2] <-5348
us_recovered[306,2]<- 51950
#238081

#5348 in 5/12/20 temporarily

#51950 in 11/22/20 temp

for_rt_us_ <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")%>%
    mutate(`UID` = NULL, `iso2`= NULL, `iso3` = NULL, `code3` = NULL, `FIPS` = NULL, `Admin2` = NULL, `Country_Region` = NULL, `Lat` = NULL, `Long_` = NULL, `Combined_Key`= NULL)%>%
    rename(`state`= `Province_State`)

v <- colnames(for_rt_us_)

for_rt_us <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")%>%
    mutate(`UID` = NULL, `iso2`= NULL, `iso3` = NULL, `code3` = NULL, `FIPS` = NULL, `Admin2` = NULL, `Country_Region` = NULL, `Lat` = NULL, `Long_` = NULL, `Combined_Key`= NULL)%>%
    dplyr::rename(`state`= `Province_State`)%>%
    filter(!`state`%in% c("American Samoa", "Diamond Princess", "Grand Princess", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "Guam"))%>%
    group_by(`state`)%>%
    summarise_all(list(sum = sum))%>%
    setNames(gsub("_sum", "", names(.)))%>%
    pivot_longer(!`state`, names_to = "date", values_to = "cum")%>%
    mutate(incidence = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`cum` = NULL)%>%
    ungroup()


us_pdr_ <- left_join(us_positive, us_death, by = "date")
us_pdr <- left_join(us_pdr_, us_recovered, by = "date")

kor_pdr_graph_test<- kor_pdr
us_pdr_graph_test <- us_pdr

kor_pdr_graph <- kor_pdr
kor_pdr_table <- kor_pdr
kor_pdr_graph$recovered[562:824]<-NA

us_pdr$date <- format(us_pdr$date,'%Y-%m-%d')
kor_pdr$date <- format(kor_pdr$date,'%Y-%m-%d')

#kor_pdr_graph_test$date <- format(kor_pdr$date,'%Y-%m-%d')#
kor_pdr_graph_test$recovered[562:length(kor_pdr_graph_test)]<-NA
#us_pdr_graph_test$date <- format(us_pdr$date,'%Y-%m-%d')#
us_pdr_graph_test$recovered[328:length(us_pdr_graph_test)]<-NA

kor_pdr_graph_test$confirmed <- as.integer(kor_pdr_graph_test$confirmed)
kor_pdr_graph_test$death <- as.integer(kor_pdr_graph_test$death)
kor_pdr_graph_test$recovered <- as.integer(kor_pdr_graph_test$recovered) 

us_pdr_graph_test$confirmed <- as.integer(us_pdr_graph_test$confirmed)
us_pdr_graph_test$death <- as.integer(us_pdr_graph_test$death)
us_pdr_graph_test$recovered <- as.integer(us_pdr_graph_test$recovered)    

us_pdr$confirmed <- as.integer(us_pdr$confirmed)
us_pdr$death <- as.integer(us_pdr$death)
us_pdr$recovered <- as.integer(us_pdr$recovered)

kor_pdr$confirmed <- as.integer(kor_pdr$confirmed)
kor_pdr$death <- as.integer(kor_pdr$death)
kor_pdr$recovered <- as.integer(kor_pdr$recovered)



################################################################################################################################################################

################################################################################################################################################################

################################################################################################################################################################

################################################################################################################################################################

for_kor_rt <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                       col_types = cols(`Province/State` = col_skip(), 
                                        Lat = col_skip(), Long = col_skip()))%>%
    filter(`Country/Region` == "Korea, South")%>%
    pivot_longer(!`Country/Region`, names_to = "date", values_to = "cum")%>%
    mutate(incidence = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`Country/Region` = NULL, `cum` = NULL)

for_rt_us <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")%>%
    mutate(`UID` = NULL, `iso2`= NULL, `iso3` = NULL, `code3` = NULL, `FIPS` = NULL, `Admin2` = NULL, `Country_Region` = NULL, `Lat` = NULL, `Long_` = NULL, `Combined_Key`= NULL)%>%
    dplyr::rename(`state`= `Province_State`)%>%
    filter(!`state`%in% c("American Samoa", "Diamond Princess", "Grand Princess", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico", "Guam"))%>%
    group_by(`state`)%>%
    summarise_all(list(sum = sum))%>%
    setNames(gsub("_sum", "", names(.)))%>%
    pivot_longer(!`state`, names_to = "date", values_to = "cum")%>%
    mutate(incidence = c(cum[1],diff(cum)))%>%
    mutate(date = format(as.Date(date, "%m/%d/%y")))%>%
    mutate(date = ymd(date))%>%
    mutate(`cum` = NULL)%>%
    ungroup()


# computation

smooth_new_cases <- function(cases){
    cases %>% 
        arrange(date) %>%
        mutate(smoothened = round(
            smoother::smth(incidence, window = 7, tails = TRUE)
        )) %>%
        dplyr::select(`date`, `incidence`, `smoothened`)%>%
        mutate(`incidence` = NULL)%>%
        rename(`incidence` = `smoothened`)
}

kor_rt<-for_kor_rt %>% 
    smooth_new_cases() %>%
    slice(-c(1:30))


SK_RT = estimate_Rt(
    kor_rt,
    cases_column = "incidence",
    date_column = "date",
    estimation_family='epiestim',
    cumulative=FALSE,
    method = 'parametric_si',
    config = list(mean_si=5.195, std_si=0.83))[c("date_start", "Mean(R)", "Median(R)", "Std(R)", "Quantile.0.05(R)", "Quantile.0.95(R)")]%>%
    dplyr::slice(which.max(as.Date(date_start, '%m/%d/%Y')))%>%
    mutate(LOCATION = "Republic of Korea")%>%
    dplyr::rename(DATE = "date_start", MEAN = `Mean(R)`, MEDIAN = `Median(R)`, SD = `Std(R)`, `QUANTILE 5%` = `Quantile.0.05(R)`, `QUANTILE 95%` = `Quantile.0.95(R)`)%>%
    relocate(`LOCATION`, .before = DATE)%>%
    mutate(across(where(is.numeric), round, 3))%>%
    mutate(DATE = NULL)



smooth_new_cases_us <- function(cases){
    cases %>% 
        arrange(date) %>%
        mutate(smoothened = round(
            smoother::smth(incidence, window = 7, tails = TRUE)
        )) %>%
        dplyr::select(`state`, `date`, `incidence`, `smoothened`)%>%
        mutate(`incidence` = NULL)%>%
        dplyr::rename(`incidence` = `smoothened`)
}

us_rt <- for_rt_us%>%
    filter(date >= "2020-04-01") %>% 
    group_by(`state`)%>%
    group_split() %>% 
    map_df(~ {
        .x %>%
            smooth_new_cases_us()
    })%>%
    ungroup()


est_by = function(df) {
    estimate_Rt(
        df,
        cases_column = "incidence",
        date_column = "date",
        estimation_family='epiestim',
        cumulative=TRUE,
        method = 'parametric_si',
        config = list(mean_si=5.195, std_si=0.83))
}
z = us_rt %>% 
    tidyr::nest(-state) %>%
    dplyr::mutate(rt_df = purrr::map(data, est_by)) %>% 
    tidyr::unnest(cols=rt_df)

US_RT<-z%>%
    dplyr::select(`state`, `date_start`, `Mean(R)`, `Median(R)`, `Std(R)`, `Quantile.0.05(R)`, `Quantile.0.95(R)`)%>%
    dplyr::group_by(`state`)%>%
    dplyr::slice(which.max(as.Date(date_start, '%m/%d/%Y')))%>%
    dplyr::filter(`Mean(R)` <= 5)%>%
    dplyr::rename(LOCATION = `state`, DATE = "date_start", MEAN = `Mean(R)`, MEDIAN = `Median(R)`, SD = `Std(R)`, `QUANTILE 5%` = `Quantile.0.05(R)`, `QUANTILE 95%` = `Quantile.0.95(R)`)%>%
    relocate(`LOCATION`, .before = DATE)%>%
    ungroup()%>%
    mutate(across(where(is.numeric), round, 3))%>%
    arrange(desc(MEAN))%>%
    dplyr::slice(1:5)%>%
    mutate(DATE = NULL)



################################################################################################################################################################

################################################################################################################################################################

################################################################################################################################################################

################################################Shiny App Section################################################################################################

################################################Shiny App Section###############################################################################################

################################################Shiny App Section###############################################################################################

################################################Shiny App Section################################################################################################

################################################Shiny App Section################################################################################################

################################################Shiny App Section################################################################################################

################################################################################################################################################################

################################################################################################################################################################

################################################################################################################################################################


ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "COVID-19 Analyzer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Today's Overview", tabName = "overview", icon = icon("dashboard")), 
            menuItem("Analyzer", icon = icon("dashboard"), startExpanded = TRUE,
                     menuSubItem("The United States", tabName = "us_analysis"),
                     menuSubItem("Republic of Korea", tabName = "korea_analysis")),
            menuItem("Trend Summary Table",tabName = "summary", icon = icon("th"))
            
            
        )
    ),

#################################################################################
    dashboardBody(
        skin = "green",
        
        



        tabItems(
            tabItem(tabName = "overview", 

                    tags$div(class="header", checked=NA,
                             tags$h1(strong("World"))),
                    fluidRow(
            
                        valueBoxOutput("worldposbox"),
                        valueBoxOutput("worldrecbox"),
                        valueBoxOutput("worlddeathbox")
                        
                        ),
                    
                    tags$div(class="header", checked=NA,
                             tags$h1(strong("The United States"))),
                    
                    fluidRow(
                        

                        valueBoxOutput("usposbox"),
                        valueBoxOutput("usrecbox"),
                        valueBoxOutput("usdeathbox")
                        

                        ),
                    
                    tags$div(class="header", checked=NA,
                             tags$h1(strong("Republic of Korea"))),
                    
                    fluidRow(
                        
                        
                        valueBoxOutput("korposbox"),
                        valueBoxOutput("korrecbox"),
                        valueBoxOutput("kordeathbox")
                        
                        
                        ),
                    
                    hr(),
                    p(strong("Recovered "), "individuals' data are depreciated from the original source: issue has been filed."),
                   
                    a("COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", href="https://github.com/CSSEGISandData/COVID-19")
                    
                    ),
            
#################################################################################

            tabItem(tabName = "us_analysis", 
                    
                    fluidRow(
                        
                        
                        box(title = strong("The United States COVID Recent Trend")
                            , status = "success", solidHeader = F
                            , collapsible = F, width = 6
                            , column(12, align="center", tableOutput('us_tbl'))),
                        box(title = strong("Graphical Visualization")
                            , status = "success", solidHeader = F
                            , collapsible = F, width = 6
                            , column(12, align="center", plotOutput('us_graph')))
                        ),
                    hr(),
                    wellPanel(
                        fluidRow(
                            column(
                                width = 11,
                                align = "center",
                                dateRangeInput(inputId = "daterangeus" , h4("Date range for Analyzer Section"),
                                               start = as.character(min(kor_pdr$date)),
                                               end = as.character(max(kor_pdr$date)),
                                               min = as.character(min(kor_pdr$date)),
                                               max = as.character(max(kor_pdr$date)),
                                               format = "yyyy/mm/dd",
                                               separator = " - ")
                            )
                        )
                    ),
                    
                    hr(),
                    
                    a("COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", href="https://github.com/CSSEGISandData/COVID-19")
                    
                    

                ),
            
            tabItem(tabName = "korea_analysis", 
                    
                    fluidRow(

                        box(title = strong("Republic of Korea COVID Recent Trend")
                            , status = "success", solidHeader = F
                            , collapsible = F, width = 6
                            , column(12, align="center", tableOutput('kor_tbl'))),
                        box(title = strong("Graphical Visualization")
                            , status = "success", solidHeader = F
                            , collapsible = F, width = 6
                            , column(12, align="center", plotOutput('kor_graph')))
                    ),
                    
                    hr(),
                    wellPanel(
                        fluidRow(
                            column(
                                width = 11,
                                align = "center",
                                dateRangeInput(inputId = "daterangekor" , h4("Date range for Analyzer Section"),
                                               start = as.character(min(kor_pdr$date)),#"2020-01-22",
                                               end = as.character(max(kor_pdr$date)),#"2022-04-24",
                                               min = as.character(min(kor_pdr$date)),#"2020-01-22",
                                               max = as.character(max(kor_pdr$date)),#"2022-04-24",
                                               format = "yyyy/mm/dd",
                                               separator = " - ")
                            )
                        )
                    ),
                    
                    hr(),
                    
                    a("COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", href="https://github.com/CSSEGISandData/COVID-19")
                        
                    ),
            
#################################################################################

            tabItem(tabName = "summary", 
                    
                    box(title = strong("Today's United States' Daily Reproduction Number (TOP 5)")
                        ,status = "success", solidHeader = F
                        , collapsible = F, width = 12
                        , column(12, align="center", formattableOutput("us_rt"))),
                    
                    br(),
                    
                    box(title = strong("Today's Korean Daily Reproduction Number")
                        ,status = "success", solidHeader = F
                        , collapsible = F, width = 12
                        , column(12, align="center", formattableOutput("ko_rt"))),
                    
                    br(),
                    
                    p(strong("Effective Reproduction Number, R(t) "), "is the expected number of new infections caused by an infectious individuals."), 
                    p("Many methods can be applied to compute the value; this output has utilized", 
                      code(" estimate_Rt() "), "included in a package", code(" library(sars2pack)"),"."
                      ),
                    p("Value", strong(" higher "), "than", strong(" 1 "), "forecasts gradual increment of the COVID-19."),
                    p("Value", strong(" lower "), "than", strong(" 1 "), "forecasts gradual decrement of the COVID-19."),
                    
                    hr(),
                    
                    a("COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", href="https://github.com/CSSEGISandData/COVID-19")
                    
                    
                    )

        )
    )
)


################################################################################
####################################SERVER######################################
################################################################################

server <- function(input, output) {
    
################################REACTIVE######################################
    

    
    
    korplotdate <- reactive({
        kor_pdr_graph_test<-kor_pdr_graph_test%>%
            mutate(recovered = NULL)%>%
            filter(date>=input$daterangekor[1] &  date<=input$daterangekor[2])
    })
    
    usplotdate <- reactive({
        us_pdr_graph_test<-us_pdr_graph_test%>%
            mutate(recovered = NULL)%>%
            filter(date>=input$daterangeus[1] &  date<=input$daterangeus[2])
    })
    
    
    
################################TABLE######################################
    
    
    output$us_tbl <- renderTable({
        ustable<- usplotdate()%>%
            mutate(date = as.character(date))%>%
            mutate(recovered = NULL)
        head(ustable[rev(order(as.Date(ustable$date, format="%Y-%m-%d"))),],9)})

    output$kor_tbl <- renderTable({
        korstable<- korplotdate()%>%
            mutate(date = as.character(date))%>%
            mutate(recovered = NULL)
        head(korstable[rev(order(as.Date(korstable$date, format="%Y-%m-%d"))),],9)})
            

################################GRAPH######################################

    
    output$us_graph <- renderPlot({
       u<- ggplot(usplotdate(), aes(x=date)) + 
            geom_area(aes(y = confirmed), fill = NA ,color = "orange",    
                      lwd = 0.5,    
                      linetype = 1) +
            geom_area(aes(y = death), fill = NA ,color = "red",    
                      lwd = 0.5,    
                      linetype = 1) +
           scale_colour_manual("", 
                               values = c("confirmed"="orange", "death"="red")) 
          u  + theme_minimal()
        
    })
    
    output$kor_graph <- renderPlot({
        
       k<- ggplot(korplotdate(), aes(x=date)) + 
            geom_area(aes(y = confirmed), fill = NA ,color = "orange",    
                      lwd = 0.5,    
                      linetype = 1) +
            geom_area(aes(y = death), fill = NA ,color = "red",    
                      lwd = 0.5,    
                      linetype = 1) +
           scale_colour_manual("", 
                               values = c("confirmed"="orange", "death"="red")) 
        k + theme_minimal()
            
        
    })

##################################OVERVIEW BOX######################################   
##################################WORLD######################################      

        output$worldposbox <- renderValueBox({
        valueBox(
            value = paste(format(world_pdr$confirmed[nrow(world_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Confirmed Cases", 
            color = "yellow",
            icon = icon("stethoscope")
        )
    })
    
    output$worlddeathbox <- renderValueBox({
        valueBox(
            value = paste(format(world_pdr$death[nrow(world_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Death Cases", 
            color = "red",
            icon = icon("skull")
        )
    })
    
    output$worldrecbox <- renderValueBox({
        valueBox(
            value = paste(format(world_pdr$recovered[nrow(world_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Recovered Cases", 
            color = "green",
            icon = icon("smile")
        )
    })


################################US#######################################
    
    output$usposbox <- renderValueBox({
        valueBox(
            value = paste(format(us_pdr$confirmed[nrow(us_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Confirmed Cases", 
            color = "yellow",
            icon = icon("stethoscope")
        )

    })
    
    output$usdeathbox <- renderValueBox({
        valueBox(
            value = paste(format(us_pdr$death[nrow(us_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Death Cases", 
            color = "red",
            icon = icon("skull")
        )

    })
    
    output$usrecbox <- renderValueBox({
        valueBox(
            value = paste(format(us_pdr$recovered[nrow(us_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Recovered Cases", 
            color = "green",
            icon = icon("smile")
        )

    })
    
##################################Korea######################################
    

    output$korposbox <- renderValueBox({
        
        valueBox(
            value = paste(format(kor_pdr$confirmed[nrow(kor_pdr)], big.mark = ","), "", sep = " "),
            subtitle = "Confirmed Cases", 
            color = "yellow",
            icon = icon("stethoscope")
        )
    })
    
    output$kordeathbox <- renderValueBox({
        valueBox(
            value = paste(format(kor_pdr$death[nrow(kor_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Death Cases", 
            color = "red",
            icon = icon("skull")
        )
    })
    
    output$korrecbox <- renderValueBox({
        valueBox(
            value = paste(format(kor_pdr$recovered[nrow(kor_pdr)], big.mark = ","), "", sep = " "), 
            subtitle = "Recovered Cases", 
            color = "green",
            icon = icon("smile")
        )
    })
    
    output$us_rt <- renderFormattable({formattable(
        US_RT,
        list(
            `MEAN` = color_tile("white", "pink")
            )
        )
        }
    )
    
    output$ko_rt <- renderFormattable({formattable(
        SK_RT,
        list(
            `MEAN` = color_tile("white", "pink")
        )
    )
    }
    )

    
    
}



shinyApp(ui, server)