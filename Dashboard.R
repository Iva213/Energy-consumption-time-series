## IoT Analytics -- Dashboard 

## Set working directory 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/IoT Analytics")

#Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(plotly)
library(ggfortify)

################################################

##Dataset -- total KWh en sub-meters(???)

#Monthly -- PER YEAR
monthly.energy <- cbind(energy.merged.monthly.sub1, energy.merged.monthly.sub2$avg.sub2, energy.merged.monthly.sub3$avg.sub3)
monthly.energy <- monthly.energy[, c(1:3, 5, 6, 4)]
colnames(monthly.energy)[3:5] <- c("Kitchen", "Laundry", "Central.heating")
#monthly.energy$Month <- as.factor(monthly.energy$Month) -- don't use this with plot_ly
monthly.energy$Month <- month(monthly.energy$Month, label = TRUE, abbr = FALSE)
#Convert global active power from KW to WHr (x1000 /60)
monthly.energy$avg.global <- (monthly.energy$avg.global * 1000) / 60

#Weekly -- PER MONTH 
weekly.energy <- cbind(energy.merged.weekly.sub1, energy.merged.weekly.sub2$avg.sub2, energy.merged.weekly.sub3$avg.sub3)
weekly.energy <- weekly.energy[, c(1:4, 6, 7, 5)]
colnames(weekly.energy)[4:6] <- c("Kitchen", "Laundry", "Central.heating")
weekly.energy$Month <- month(weekly.energy$Month, label = TRUE, abbr = FALSE)
#Convert global active power from KW to WHr (x1000 /60)
weekly.energy$avg.global <- (weekly.energy$avg.global * 1000) / 60

#Time-series predictions
sub1.hw <- HW.analysis(energy.merged.weekly.sub1$avg.sub1)
sub2.hw <- HW.analysis(energy.merged.weekly.sub2$avg.sub2)
sub3.hw <- HW.analysis(energy.merged.weekly.sub3$avg.sub3)

#
f1 <- forecast(sub1.hw$sub.hw, h = 24, level = c(80, 90))

autoplot(f1) 
################################################

ui <- fluidPage(
  titlePanel("Energy consumption dashboard"),
 
  sidebarPanel( 
    conditionalPanel(
      condition = "input.tab1 == 1",
      radioButtons(inputId = "input_year_name",
                            label = "Choose a year:",
                            choices = c("2007", "2008", "2009"))),
      
      conditionalPanel(
        condition = "input.tab1 == 2",
        radioButtons(inputId = "input_month_name",
                    label = "Choose a month:",
                    choices = list("January", "February", "March", "April", "May", "June", "July", 
                                   "August", "September", "October", "November", "December"))),
    conditionalPanel(
      condition = "input.tab1 == 3",
      radioButtons(inputId = "input_sub_name",
                   label = "Choose sub-meter:",
                   choices = c("Kitchen", "Laundry room", "Central heating")))
    
      ),

  mainPanel(
    tabsetPanel(id = "tab1",
                tabPanel("Total power consumption", value = 1, plotlyOutput(outputId = "year.consumption")),
                tabPanel("Sub-meter consumption", value = 2, plotlyOutput(outputId = "month.consumption")),
                tabPanel("Trends", value = 3, plotOutput(outputId = "forecasting"))))
    
  
)

################################################

server <- function(input, output) { 
  
  output$year.consumption <- renderPlotly({plot_ly(monthly.energy %>% filter(Year == input$input_year_name), 
                                                   x = ~Month, y = ~Kitchen, 
                                                   name = "Kitchen", type = "scatter", mode = "lines") %>%
      add_trace(y = ~Laundry, name = "Laundry", mode = "lines") %>%
      add_trace(y = ~Central.heating, name = "AC/heater", mode = "lines") %>%
      add_trace(y = ~avg.global, name = "Global", mode = "lines") %>%
      layout(
             xaxis = list(title = "Month"),
             yaxis = list (title = "Power (Wh)"))})
  
  output$month.consumption <- renderPlotly({plot_ly(weekly.energy %>% 
                                                      filter(Year == input$input_year_name & Month == input$input_month_name), 
                                                    x = ~Week, y = ~Kitchen, name = "Kitchen", type = "scatter", mode = "lines") %>%
      add_trace(y = ~Laundry, name = "Laundry", mode = "lines") %>%
      add_trace(y = ~Central.heating, name = "AC/heater", mode = "lines") %>%
      add_trace(y = ~avg.global, name = "Global", mode = "lines") %>%
      layout(
             xaxis = list(title = "Week"),
             yaxis = list (title = "Power (Wh)"))})
  
  plot.test <- reactive({
    if("Kitchen" %in% input$input_sub_name) return(
      plot(sub1.hw$sub.hw, sub1.hw$forecast, xlim = c(2010, 2011), ylab = "Power (Wh)", 
           ylim = c(0, 15), main = "Predicted power consumption"))
    
    if("Laundry room" %in% input$input_sub_name) return(
      plot(sub2.hw$sub.hw, sub2.hw$forecast, ylim = c(0, 15), ylab = "Power (Wh)", main = "Predicted power consumption"))
    
    if("Central heating" %in% input$input_sub_name) return(
      plot(sub3.hw$sub.hw, sub3.hw$forecast, ylim = c(0, 15), ylab = "Power (Wh)", main = "Predicted power consumption")
      )
    autoplot(sub2.hw$forecast)
    
  })
    
  output$forecasting <- renderPlot({
    selected.plot = plot.test()
    print(selected.plot)
    })
    
}

################################################

shinyApp(ui, server)


