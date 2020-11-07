
# Interactive dashboard to forecast Le Cube climbing gym occupancy
##################################################################

# Carlo Knotz

library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(forecast)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)

# Setup
#######

# Load themes for dashboard
load("bluethemes.RData")

# Get metadata
sheet <- readLines("sheet.txt")
mail <- readLines("mail.txt")

#Authenticate
options(gargle_oauth_cache = ".secrets")
gs4_auth(email = mail,
         cache = ".secrets")

#Download data
cubedata <- read_sheet(ss=sheet)

# Remove first observation - originally test
cubedata <- cubedata[2:nrow(cubedata),]

# Change datetime format
cubedata$time <- substr(cubedata$time, 1, 16)
  cubedata$time <- as.POSIXct(cubedata$time, tz="Europe/Paris")
  cubedata$day <- weekdays(cubedata$time,
                           abbreviate = T)
  cubedata$hour <- format(cubedata$time,
                          format = "%H:%M:%S")

# Setting values to zero during closed hours
cubedata$occ[cubedata$hour %in% c("23:00:00","23:30:00","00:00:00","00:30:00","01:00:00",
                                  "01:30:00","02:00:00","02:30:00","03:00:00","03:30:00",
                                  "04:00:00","04:30:00","05:00:00","05:30:00","06:00:00",
                                  "06:30:00","07:00:00","07:30:00","08:00:00","08:30:00",
                                  "09:00:00","09:30:00") ] <- 0

cubedata$occ[cubedata$hour %in% c("20:30:00","21:00:00","21:30:00","22:00:00","22:30:00")
             & cubedata$day %in% c("Sat","Sun")] <- 0

# Linearly interpolating missing values
cubedata$occ_inter <- zoo::na.approx(cubedata$occ)


############################
# Cutting post-lockdown data
cubedata <- cubedata %>% 
    filter(time < as.POSIXct("2020-11-02 16:30:00",tz="Europe/Paris"))
############################


# Comparison figures
compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),mean) # Specific day/time averages
norm <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]

compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),max) # Specific day/time averages
max <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]


# Setting up dashboard
######################

ui <- dashboardPage(
  dashboardHeader(title = "Le Cube Forecast"),
  dashboardSidebar(collapsed = F,
    sidebarMenu(menuItem("Current occupancy & forecast",tabName = "data", icon = icon("dashboard")),
                menuItem("Background info", tabName = "info", icon = icon("info-circle", lib = "font-awesome")))),
  dashboardBody(
    tags$style(type="text/css",
    ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #f08c00;border-color: #f08c00;}
                             .irs-max {font-family: 'arial'; color: white;} .irs-min {font-family: 'arial'; color: white;}"),
    #shinyDashboardThemes(theme = "grey_light"),
    oceanblue,
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                column(width = 6,
                box(width = NULL, title = "Important Info",collapsible = F, solidHeader = T,
                    HTML("<p>As of November 4, 2020 and due to the new COVID-19 outbreak in Switzerland, Le Cube has been closed, along with all other climbing gyms, museums, restaurants, etc.
                         in the entire canton of Vaud. I will keep collecting data from their website, but the data shown here end on November 2 at 4pm.</p>
                         
                         <p>The data and dashboard will be updated once the situation changes.</p>")),
                box(width = NULL, title = paste0("How does it look at the gym (as of ",substr(tail(cubedata$hour,1),1,5),")?"),
                    collapsible = F, solidHeader = T,
                    valueBoxOutput("current"),
                    valueBoxOutput("normal"),
                    valueBoxOutput("max")
                    )),
                column(width = 6,
                box(width = NULL, title = "How will it look like from now on?", collapsible = F, solidHeader = T,
                   column(width=6,
                          br(),
                          br(),
                    actionBttn(inputId = "forecastbtn",
                               label = "(Re-)run forecast",
                               style = "material-flat",
                               color = "warning",
                               size = "xs")),
                   column(width = 6,
                    sliderInput(inputId = "forecastslider",
                                label = "Hours to forecast",
                                min = 5,
                                max = 36,
                                value = 15,
                                step = 1)),
                   column(width = 12,
                          plotOutput("forecast"))
                    ))),
              fluidRow(
                box(width = 12, title = "Show me all the data",collapsible = T, solidHeader = T,collapsed = T,
                    plotOutput("past"),
                    # sliderInput(inputId = "dataslider",
                    #             min = as.POSIXlt("2020-10-10 16:00:00", "%Y-%M-%D %H:%M:S",tz = "Europe/Paris"),
                    #             max = as.POSIXlt(as.character(tail(cubedata$time,1)), "%Y-%M-%D %H:%M:S",tz = "Europe/Paris"),
                    #             label = "Select data range (days)",
                    #             ticks = F,
                    #             step = .1,
                    #             value = c(as.POSIXct("2020-10-10 16:00:00", "%Y-%M-%D %H:%M:S", tz = "Europe/Paris")))
                    )
              )
              ),
      tabItem(tabName = "info",
              fluidRow(
                box(width = 12, solidHeader = T,collapsible = F,title = "Background & Methods",
                    HTML("<p>Crowded bouldering gyms aren't fun - and especially now that there is a significant risk that someone
                         in the gym might carry and spread the new coronavirus. Luckily, my local gym (Le Cube in Lausanne) has
                         a neat little dial on their website (<a href='http://www.lecube.ch/' target='_blank'>lecube.ch</a>) that tells how full the gym is at any given point in time.</p>
                         
                         <p>But why stop there when we also have tools to forecast how busy the gym will be over the next hours?
                         This is the idea behind this dashboard.</p>
                         
                         <p>The data for this dashboard are scraped from the lecube.ch website every full and half hour and stored
                         in the cloud (this started on the afternoon of October 10, 2020 and is ongoing).</p>
                         
                         <p>Now that several weeks worth of data have been collected, I built this dashboard, which retrieves the data, shows simple descriptive
                         information and features a forecast function to estimate how busy the gym will be over the next five to 36 hours.</p>
                         
                         <p>The forecast is estimated anew every time the dashboard is started and the '(Re-)run forecast' button is klicked.
                         The algorithm behind the forecast is a neural network for time-series data, specifically the <code>nnetar()</code>
                         function contained in the <code>forecast</code>-package (see <a target='_blank' href='https://otexts.com/fpp2/'>Hyndman & Athanasopoulos, 2018</a>), which estimates a feed-forward neural network with a single hidden node and a variable number of lagged values 
                         as inputs. The optimal configuration is determined automatically on a case-by-case basis.</p>
                         
                         <p>The code to scrape the data, sample code for the forecasting function, and the code for this dashboard
                         are deposited on my <a href='https://github.com/cknotz/lecube_forecast' target='_blank'>GitHub profile</a>.</p>
                         
                         <p>The next step will be to determine an optimal forecasting model by comparing the performances of a number of 
                         different estimators against each other. This will be done as soon as more data have been collected and there is enough
                         information for the estimators to learn the more subtle seasonal features in the data, for instance the difference between
                         weekends and weekdays.</p>
                         
                         <p>I am not affiliated with Le Cube (apart from my membership, which I paid for myself) and I hope they will
                         forgive me for using up a bit of their bandwith when I scrape their data.</p>"))
              ))))
)

server <- function(input, output, session) { 
  
output$past <- renderPlot({
  ggplot(data=cubedata,aes(x=time,y=occ_inter)) +
  geom_line(color = "#68e8ff") + #, 
  geom_hline(yintercept = 100,linetype="dashed",color="#e6fbff") +
  ylab("Occupancy (%)") +
  xlab("") +
  theme_oceanblue()
})

{
# Latest box
if(tail(cubedata$occ_inter,1)>=75){
  output$current <- renderValueBox({
    valueBox(paste0(tail(cubedata$occ_inter,1),"%"),
             subtitle = "Occupancy",
             color = "red",
             width = 12)
  })
}
if(tail(cubedata$occ_inter,1)>=50 & tail(cubedata$occ_inter,1)<75){
  output$current <- renderValueBox({
    valueBox(paste0(tail(cubedata$occ_inter,1),"%"),
             subtitle = "Occupancy",
             color = "orange",
             width = NULL)
  })
}
if(tail(cubedata$occ_inter,1)>=25 & tail(cubedata$occ_inter,1)<50){
  output$current <- renderValueBox({
    valueBox(paste0(tail(cubedata$occ_inter,1),"%"),
             subtitle = "Occupancy",
             color = "yellow",
             width = NULL)
  })
}
if(tail(cubedata$occ_inter,1)<25){
  output$current <- renderValueBox({
    valueBox(paste0(tail(cubedata$occ_inter,1),"%"),
             subtitle = "Occupancy",
             color = "green",
             width = NULL)
  })
}

# Mean box
if(norm>=75){
output$normal <- renderValueBox({
  valueBox(paste0(round(norm,0),"%"),
           subtitle = "Average for this day & hour",
           color = "red",
           width = 12)
})
}
if(norm<75 & norm>=50){
  output$normal <- renderValueBox({
  valueBox(paste0(round(norm,0),"%"),
           subtitle = "Average for this day & time",
           color = "orange",
           width = NULL)
  })
}
if(norm<50 & norm>=25){
  output$normal <- renderValueBox({
  valueBox(paste0(round(norm,0),"%"),
           subtitle = "Average for this day & time",
           color = "yellow",
           width = NULL)
  })
}
if(norm<25){
  output$normal <- renderValueBox({
  valueBox(paste0(round(norm,0),"%"),
           subtitle = "Average for this day & time",
           color = "green",
           width = NULL)
  })
}

# Max box
if(max>=75){
output$max <- renderValueBox({
  valueBox(paste0(round(max,0),"%"),
           subtitle = "Maximum observed for this day & time",
           color = "red")
})
}
if(max<75 & max>=50){
  output$max <- renderValueBox({
    valueBox(paste0(round(max,0),"%"),
           subtitle = "Maximum observed for this day & time",
           color = "orange")
})
}
if(max<50 & max>=25){
  output$max <- renderValueBox({
    valueBox(paste0(round(max,0),"%"),
           subtitle = "Maximum observed for this day & time",
           color = "yellow")
})
}
if(max<25){
  output$max <- renderValueBox({
    valueBox(paste0(round(max,0),"%"),
           subtitle = "Maximum observed for this day & time",
           color = "green")
})
}
} # Boxes with descriptive figures
  
# Forecasting & graph
observeEvent(input$forecastbtn,{
  
showModal(modalDialog("Computer's computin', please wait...", footer=NULL))
  
# Running forecast
b <- 30 # Time windom 
h <- isolate(input$forecastslider*2) 
  
fit <- nnetar(cubedata$occ_inter, lambda = "auto")
  fcast <- forecast(fit,h=h)
  preddata <- data.frame(vals = c(fcast$x,fcast$mean),
                       count = 1:(length(fcast$x)+length(fcast$mean)))
  preddata$ind <- as.factor(ifelse(preddata$count<=length(cubedata$time),"Observed","Forecast"))
  
  #if(length(cubedata$time)%%2==0){
  preddata$epoch <- seq(from = -(length(cubedata$time)-1)/2,length.out = length(preddata$vals),by=.5)
  #}else{
  #preddata$epoch <- seq(from = -(length(cubedata$time)-1)/2,length.out = length(preddata$vals),by=.5)
  #}
  

lo <- length(cubedata$occ)-b
  hi <- length(preddata$vals)

  print(as.character(length(preddata$vals)))
  removeModal()
  
output$forecast <- renderPlot({
ggplot(preddata[lo:hi,],aes(x = epoch,y=vals, group = ind,color = ind)) +
  geom_line() +
  scale_color_manual(values = c("#fcba04","#68e8ff")) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = seq(from = -b/2, to = h/2, by = 5),
                     minor_breaks = seq(from = -b/2, to = h/2, by=1)) +
  geom_vline(xintercept = 0, color = "#e6fbff", linetype = "dotted", size = 1.25) +
    xlab("Hours") +
    ylab("Occupancy (%)") +
    labs(caption = paste0("0 = ",format(tail(cubedata$time,1),"%b %d %H:%M"))) +
    theme_oceanblue() +
    theme(legend.title = element_blank(),
          panel.grid.minor.x = element_line(size = .1, color = "#e6fbff"),
          panel.grid.major.x = element_line(size = .25, color = "#e6fbff"))

})
})

}



shinyApp(ui, server)