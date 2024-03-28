pacman::p_load(shiny,tidyverse, lubridate, knitr, DT, ggplot2, plotly, ggthemes, 
               ggfortify, forecast, MLmetrics, tsbox, xts, imputeTS, tseries, hrbrthemes, ggstatsplot)

data <-read_rds("data/weather_data_imputed.rds")

ui <- fluidPage(
    titlePanel("Exploratory Data Analysis Module"),
    sidebarLayout(
        sidebarPanel(
          titlePanel("Correlation Factors"),
          selectInput(inputId = "variable1",
                      label = "Select Variable 1",
                      choices = c("Mean Temperature" = "mean_monthly_temperature",
                                  "Min Temperature" = "min_monthly_temperature",
                                  "Max Temperature" = "max_monthly_temperature",
                                  "Rainfall" = "monthly_rainfall"),
                      selected = "Mean Temperature"),
          
          selectInput(inputId = "variable2",
                      label = "Select Variable 2",
                      choices = c("Rainfall" = "monthly_rainfall",
                                  "Mean Temperature" = "mean_monthly_temperature",
                                  "Min Temperature" = "min_monthly_temperature",
                                  "Max Temperature" = "max_monthly_temperature"),
                      selected = "Rainfall"),
          
          titlePanel("Comparision of Temperature or Rainfall Across the Years"),
          selectInput(inputId = "variable3",
                      label = "Select Data",
                      choices = c("Mean Temperature" = "mean_monthly_temperature",
                                  "Min Temperature" = "min_monthly_temperature",
                                  "Max Temperature" = "max_monthly_temperature",
                                  "Rainfall" = "monthly_rainfall"),
                      selected = "Mean Temperature"),
          
          selectInput(inputId = "shstation",
                      label = "Select a weather station",
                      choices = c("Admiralty" = "Admiralty",
                                  "Ang Mo Kio" = "Ang Mo Kio",
                                  "Changi" = "Changi", 
                                  "Choa Chu Kang (South)" = "Choa Chu Kang (South)",
                                  "Clementi" = "Clementi",
                                  "East Coast Parkway" = "East Coast Parkway",
                                  "Jurong Island" = "Jurong Island",
                                  "Jurong (West)" = "Jurong (West)",
                                  "Newton" = "Newton",
                                  "Pasir Panjang" = "Pasir Panjang", 
                                  "Sentosa Island"  = "Sentosa Island",
                                  "Tai Seng" = "Tai Seng",
                                  "Tuas South" = "Tuas South"),
                      selected = "Admiralty"),

        ),
        mainPanel("main panel",
                  fluidRow(
                    column(6, plotOutput(outputId = "correlation")),
                    column(6, plotOutput(outputId = "meancomparison")),
                    column(6, plotOutput(outputId = "boxplot"))        
        )))
        )
    


server <- function(input, output) {

  ######################################### Select data based on user input ###################################################### 
  
   output$correlation <- renderPlot({
    ggscatterstats(
      data = data,
      x = !!input$variable1,
      y = !!input$variable2,
      xlab = "Variable 1", ## label for the x-axis
      ylab = "Variable 2", ## label for the y-axis
      marginal = FALSE
    )
  })

  
  output$meancomparison <- renderPlot({
    ggbetweenstats(
      data = data %>% group_by(input$shstation),
      x = tdate, 
      y = !!input$variable3,
#      grouping.var = !!input$shstation,
      type = "p",
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE
    )
  })

  output$boxplot <- renderPlot({
    ggplot(data = data %>% filter(station = !!input$shstation),
           aes(y= !!input$variable1,
               x = as.factor(tdate))) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 60,
                                       size = 6))
  })
  
   
  }
  
    


# Run the application 
shinyApp(ui = ui, server = server)
