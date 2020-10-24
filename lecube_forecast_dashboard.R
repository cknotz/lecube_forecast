
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

# Get metadata
sheet <- readLines("sheet.txt")
mail <- readLines("mail.txt")

#Authenticate
options(gargle_oauth_cache = ".secrets")
gs4_auth(email = mail,
         cache = ".secrets")

# Download data
cubedata <- read_sheet(ss=sheet)

# Remove first observation - originally test
cubedata <- cubedata[2:nrow(cubedata),]

# Change datetime format
cubedata$time <- substr(cubedata$time, 1, 16)
  cubedata$time <- as.POSIXct(cubedata$time)
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

# Comparison figures
compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),mean) # Specific day/time averages
norm <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]

compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),max) # Specific day/time averages
max <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]


# Setting up dashboard
######################

ui <- dashboardPage(
  dashboardHeader(title = " "),
  dashboardSidebar(collapsed = T,
    sidebarMenu(menuItem("Current occupancy & forecast",tabName = "data", icon = icon("dashboard")),
                menuItem("Background info", tabName = "info", icon = icon("info-circle", lib = "font-awesome")))),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(width = 6, title = paste0("How does it look at the gym (",substr(tail(cubedata$hour,1),1,5),")?"), collapsible = F, solidHeader = F,
                    valueBoxOutput("current"),
                    valueBoxOutput("normal"),
                    valueBoxOutput("max")
                    ),
                box(width = 6, title = "How will it look like over the next hours?", collapsible = F, solidHeader = T,
                    actionBttn(inputId = "forecastbtn",
                               label = "Run forecast",
                               style = "material-flat",
                               color = "warning",
                               size = "xs"),
                    plotOutput("forecast")
                    )
              ),
              fluidRow(
                box(width = 12, title = "Show me all the data",collapsible = T, solidHeader = F,collapsed = T,
                    plotOutput("past")
                    )
              )
              ),
      tabItem(tabName = "info")))
)

server <- function(input, output, session) { 
  
output$past <- renderPlot({
  ggplot(data=cubedata,aes(x=time)) +
  geom_line(aes(y=occ_inter)) +
  geom_hline(yintercept = 100,linetype="dashed",color="gray") +
  #stat_smooth(aes(y=occ),linetype="dashed",color="gray21", alpha=.2,size=.5) +
  ylab("Occupancy (%)") +
  xlab("") +
  theme_bw()
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
fit <- nnetar(cubedata$occ_inter)
  fcast <- forecast(fit,h=72)
  preddata <- data.frame(vals = c(fcast$x,fcast$mean),
                       count = 1:(length(fcast$x)+length(fcast$mean)))
  preddata$ind <- as.factor(ifelse(preddata$count<=length(fcast$x),"Observed","Forecast"))

lo <- length(cubedata$occ)-32
  hi <- length(preddata$vals)

  print(as.character(length(preddata$vals)))
  removeModal()
  
output$forecast <- renderPlot({
ggplot(preddata[lo:hi,],aes(x = count,y=vals)) +
  geom_line(aes(color = ind))

})
})

}



shinyApp(ui, server)