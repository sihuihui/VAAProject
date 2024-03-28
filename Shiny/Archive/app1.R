pacman::p_load(tidyverse, lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip)

weatherdata <-read_rds("data/weather_data_imputed.rds")


ui <- fluidPage(
  navbarPage("Forecasting Modules",
  tabPanel("Exponential Smoothing",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "sh_station",
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
               hr(),
               selectInput(inputId = "sh_variable",
                           label = "Select the variable to forecast",
                           choices = c("Mean Temperature" = "mean_monthly_temperature",
                                       "Maximum Temperature" = "max_monthly_temperature",
                                       "Minimum Temperature" = "min_monthly_temperature",
                                       "Total Rainfall" = "monthly_rainfall"),
                           selected = "mean_monthly_temperature"),
               
               hr(), 
               checkboxGroupInput(inputId = "sh_decompose",
                           label = "Select the STL decompositions to visualise",
                           choices = c("Observed" = "observed",
                                       "Season" = "season",
                                       "Trend" = "trend",
                                       "Remainder" = "remainder",
                                       "Seasonal Adjusted" = "seasadj")),
               
               hr(),
               actionButton(inputId = "sh_plotdata",
                            label = "Show my Data!"),
              
               
               hr(),
               sliderInput("sh_traindata",
                           label = "Select the amount of Training Data to use", 
                           min = 0.6,
                           max = 1,
                           value = 0.8,
                           step = 0.05),
               
               hr(),
               sliderInput("sh_forecasthorizon",
                           label = "Select the Forecast Horizon", 
                           min = 1,
                           max = 120,
                           value = 36,
                           step = 1),
               
               hr(), 
               selectInput(inputId = "sh_seasonalperiod",
                           label = "Select a seasonal frequency",
                           choices = c("Auto" = "auto",
                                       "1 month" = "1",
                                       "3 months" = "3",
                                       "12 months" = "12"),
                           selected = "auto"),
               
               hr(),
               radioButtons(inputId = "sh_error",
                            label = "Select the form of error term",
                            choices = c("Auto" = "auto",
                                        "Additive" = "additive",
                                        "Multiplicative" = "multiplicative"),
                            selected = "auto"), 
               hr(),
               radioButtons(inputId = "sh_trend",
                            label = "Select the form of trend term",
                            choices = c("Auto" = "auto",
                                        "Additive" = "additive",
                                        "Multiplicative" = "multiplicative"),
                            selected = "auto"), 
               hr(),
               radioButtons(inputId = "sh_season",
                            label = "Select the form of season term",
                            choices = c("Auto" = "auto",
                                        "Additive" = "additive",
                                        "Multiplicative" = "multiplicative",
                                        "None" = "none"),
                            selected = "auto"), 
               hr(),
               radioButtons(inputId = "sh_damp",
                            label = "Select the form of damping term",
                            choices = c("Auto" = "auto",
                                        "Damped" = "damped",
                                        "None" = "none"),
                            selected = "auto"), 
               hr(),
               actionButton(inputId = "sh_forecast",
                            label = "Forecast!")
               
             ), 
             mainPanel(
               plotOutput("sh_SelectedDataPlot"),
               tableOutput("sh_SelectedDataTable"),
               plotOutput("sh_DecompositionPlot"),
               plotOutput("sh_ForecastPlot1"),
               tableOutput("sh_AccuracyTable"),
               plotOutput("sh_ForecastPlot2")
             )
           )),
  tabPanel("ARIMA")
  )
)


server <- function(input, output) {
  
## Filter Data based on selected station and weather variable 
  selectedData <- eventReactive(input$sh_plotdata, {
    weatherdata %>%
      filter(station %in% input$sh_station)
      # mutate(MTHYEAR = my(as.Date(tdate)))
      #select(tdate,input$sh_variable)
  })
  
  output$sh_SelectedDataPlot <- renderPlot({
    req(selectedData())
    ggplot(data = selectedData(),
           aes_string(x = "tdate", y = input$sh_variable)) +
      geom_line() + 
      theme_ipsum_rc()
  })
  
## Plot filtered Data based on selected variable 
  #output$sh_SelectedDataTable <- renderTable({
    #req(selectedData())
    #selectedData()
    #})
  
  decompositionplot <- reactive({
    req(selectedData())
    plot_stl_diagnostics(selectedData(), tdate, input$sh_variable,
                         .feature_set = input$sh_decompose)
  })
  
  output$sh_DecompositionPlot <- renderPlot({
    req(decompositionplot())
    decompositionplot()
  })
    


  
  
  }
  

# Run the application 
shinyApp(ui = ui, server = server)
