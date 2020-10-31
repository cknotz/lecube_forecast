
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(dashboardthemes)

setwd("/Users/carloknotz/Dropbox (IDHEAP)/Data/lecube_forecast")

### Ocean blue scheme
  oceanblue <- shinyDashboardThemeDIY(
  
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(230, 251, 255)"
    ,primaryFontColor = "rgb(230, 251, 255)"
    ,infoFontColor = "rgb(230, 251, 255)"
    ,successFontColor = "rgb(230, 251, 255)"
    ,warningFontColor = "rgb(230, 251, 255)"
    ,dangerFontColor = "rgb(230, 251, 255)"
    ,bodyBackColor = "rgb(28,46,74)"
  
    ### header
    ,logoBackColor = "rgb(28,46,74)"
  
    ,headerButtonBackColor = "rgb(28,46,74)"
    ,headerButtonIconColor = "rgb(230, 251, 255) "
    ,headerButtonBackColorHover = "rgb(28,46,74)"
    ,headerButtonIconColorHover = "rgb(230, 251, 255) "
  
    ,headerBackColor = "rgb(28,46,74)"
    ,headerBoxShadowColor = "rgb(35, 57, 93)"
    ,headerBoxShadowSize = "0px 0px 0px"
  
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
      direction = "down"
      ,colorStart = "rgb(28,46,74)"
      ,colorMiddle = "rgb(28,46,74)"
      ,colorEnd = "rgb(28,46,74)"
      ,colorStartPos = 50
      ,colorMiddlePos = 75
      ,colorEndPos = 100
    )
    ,sidebarPadding = 0
  
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
  
    ,sidebarShadowRadius = "1px 1px 1px"
    ,sidebarShadowColor = "rgb(35, 57, 93)"
  
    ,sidebarUserTextColor = "rgb(230, 251, 255) "
  
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
    ,sidebarTabTextColor = "rgb(230, 251, 255) "
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none none none"
    ,sidebarTabBorderColor = "rgb(28,46,74)"
    ,sidebarTabBorderWidth = 1
  
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
      direction = "right"
      ,colorStart = "rgb(35,57,93)"
      ,colorMiddle = "rgb(35,57,93)"
      ,colorEnd = "rgb(35,57,93)"
      ,colorStartPos = 0
      ,colorMiddlePos = 30
      ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(230, 251, 255)"
    ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
    ,sidebarTabBackColorHover = cssGradientThreeColors(
      direction = "right"
      ,colorStart = "rgb(21, 34, 56)"
      ,colorMiddle = "rgb(21, 34, 56)"
      ,colorEnd = "rgb(21, 34, 56)"
      ,colorStartPos = 0
      ,colorMiddlePos = 30
      ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(230, 251, 255)"
    ,sidebarTabBorderStyleHover = "none none none none"
    ,sidebarTabBorderColorHover = "rgb(35,57,93)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
    ### boxes
    ,boxBackColor = "rgb(35,57,93)"
    ,boxBorderRadius = 2
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(210,214,220)"
    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgb(15, 115, 72)"
    ,boxWarningColor = "rgb(252, 76, 4)"
    ,boxDangerColor = "rgb(130, 1, 25)"
  
    ,tabBoxTabColor = "rgb(28,46,74)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(230, 251, 255)"
    ,tabBoxTabTextColorSelected = "rgb(230, 251, 255)"
    ,tabBoxBackColor = "rgb(35,57,93)"
    ,tabBoxHighlightColor = "rgb(35,57,93)"
    ,tabBoxBorderRadius = 5
  
    ### inputs
    ,buttonBackColor = "rgb(28,46,74)"
    ,buttonTextColor = "rgb(230, 251, 255)"
    ,buttonBorderColor = "rgb(21, 34, 56)"
    ,buttonBorderRadius = 5
  
    ,buttonBackColorHover = "rgb(35,57,93)"
    ,buttonTextColorHover = "rgb(230, 251, 255)"
    ,buttonBorderColorHover = "rgb(21, 34, 56)"
  
    ,textboxBackColor = "rgb(28,46,74)"
    ,textboxBorderColor = "rgb(28,46,74)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(28,46,74)"
    ,textboxBorderColorSelect = "rgb(28,46,74)"
  
    ### tables
    ,tableBackColor = "rgb(35,57,93)"
    ,tableBorderColor = "rgb(35,57,93)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
  )
  
# Custom ggplot-theme
#####################
  
theme_oceanblue <- function () { 
    theme_bw(base_size=12, base_family="Arial") %+replace%
      theme(
        panel.background  = element_rect(fill = "#23395d", color = NA),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#23395d", color = NA),
        legend.background = element_rect(fill="transparent", color=NA),
        legend.key = element_rect(fill="transparent", color=NA),
        panel.grid = element_blank(),
        axis.line = element_line(color = "#e6fbff"),
        text = element_text(color = "#e6fbff"),
        axis.text = element_text(color = "#e6fbff"),
        axis.ticks = element_line(color = "#e6fbff"),
        legend.position = "bottom"
      )
}

save(oceanblue,theme_oceanblue, file = "bluethemes.RData")  
  
  
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )),
  dashboardBody(
    oceanblue,
    tabItems(
    tabItem(tabName = "dashboard",
    fluidRow(
      box(width=12,solidHeader = T,
          plotOutput("plot1")),
      box(width=12,solidHeader = T,
          dataTableOutput('table')
          )
    )
    ),
    tabItem(tabName = "widgets",
        h2("Widgets tab content"),
        fluidRow(
        tabBox(title = "First tabBox",id = "tabset1", height = "250px",
      # The id lets us use input$tabset1 on the server to find the current tab
      tabPanel("Tab1", "First tab content",
               actionButton("bttn", "Action button")),
      tabPanel("Tab2", "Tab content 2",
               textInput("text", "Text input:"))
    ),
    tabBox(title = "Infoboxes", id = "boxes", height = "250px",
           tabPanel("Tab3","Some boxes",
           infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
           ),
           tabPanel("tab4", "Stuff")
           )
      ),
    fluidRow(
      box(width = 12, title = "Some boxes, for real",solidHeader = T,
          valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
          valueBox(42, "The Answer", icon = icon("info-circle"), color = "yellow"),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE, color = "orange"),
          )
    )
    )
  )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(mpg, wt)) +
      geom_point(color = "#68e8ff", size = 3, alpha = .75) +
      stat_smooth(method = lm, fill = "#68e8ff", color = "#fcba04") +
      theme_oceanblue()
  })
  
  output$table <- renderDataTable(mtcars)
}

shinyApp(ui, server)