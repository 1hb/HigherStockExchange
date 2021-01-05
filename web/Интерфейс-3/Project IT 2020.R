
#install and connecting the necessary libraries
library(readr)

install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

install.packages('dplyr')
library(dplyr)

install.packages("data.table")
library(data.table)

install.packages("ggplot2")
library(ggplot2)

install.packages("lubridate")
library(lubridate)

install.packages("yaml")
library(yaml)

#loading dataset

setwd("C:/Users/1/Downloads") #specify the path to the Trade2.csv file in quotation marks
Trade2 <- read_csv("Trade2.csv")

#make some corrections

Trade2$TIME <- strptime(as.character(Trade2$TIME), "%H%M%S")
Trade2$TIME <- as.POSIXct(Trade2$TIME)
Trade2$TIME <- with(Trade2, TIME - years(as.integer(format(Sys.Date(), "%Y")) - 2015))
Trade2$TIME <- with(Trade2, TIME - months(as.integer(format(Sys.Date(), "%m")) - 3))
Trade2$TIME <- with(Trade2, TIME - days(as.integer(format(Sys.Date(), "%e")) - 2))

Trade2$TIME[is.na(Trade2$TIME)] <- '2015-03-02 10:00:00'

#generate final data frame

HIGH1 <- (as.data.frame(Trade2 %>% group_by(SECCODE) %>% summarise(HIGH = max(HIGH))))[,2]
LOW1 <- (as.data.frame(Trade2 %>% group_by(SECCODE) %>% summarise(LOW = max(LOW))))[,2]
OPEN1 <- (as.data.frame(Trade2 %>% group_by(SECCODE) %>% summarise(OPEN = first(OPEN))))
CLOSE1 <- (as.data.frame(Trade2 %>% group_by(SECCODE) %>% summarise(CLOSE = last(CLOSE))))[,2]
VOLUME1 <- (as.data.frame(Trade2 %>% group_by(SECCODE) %>% summarise(VOLUME = sum(VOLUME))))[,2]
CHANGE1 <- (as.data.frame(Trade2 %>% group_by(SECCODE) %>% summarise(CHANGE = ((last(CLOSE)/first(OPEN)-1)*100))))[,2]

final <- as.data.frame(OPEN1)
final$HIGH <- HIGH1
final$LOW <- LOW1
final$CLOSE <- CLOSE1
final$VOLUME <- VOLUME1
final$CHANGE <- CHANGE1

final$CHANGE <- round(final$CHANGE, digits = 1)
colnames(final)[1] <- 'Ticker'

#shinydashboard

library(DT)
ui <- dashboardPage(
  dashboardHeader(title = "Dashboardt"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard for stocks", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", width = 500, height = 400)
      ),
      box(selectInput("variable", "Choose ticker:", selected = "AFKS",
                      c("AFKS" = "AFKS",
                        "ROSN" = "ROSN",
                        "SBERP" = "SBERP")), width = 2),
      plotOutput("data")
    ),
    fluidPage(
      datatable(final,filter = list(position = 'top', clear = FALSE), options = list(
        pageLength = 10, autoWidth = TRUE),
      )
    )
  )
)


server <- function(input, output) {
  output$tab1 <- renderDataTable(summary)
  output$plot1 <- renderPlot({
    data1 <- Trade2[Trade2$SECCODE %in% input$variable, ]
    ggplot(data1[,c(2,4)], aes(x=TIME)) + 
      geom_line(aes(y=CLOSE), color = 'tomato') + 
      labs(title="Stock chart", 
           subtitle="Close prices during day", 
           caption="Source: TradingView.com", 
           y="Price", x='Time')+
      theme_bw() 
  })
}

shinyApp(ui, server)


