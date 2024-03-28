pacman::p_load(tidyverse, shiny, bslib, 
               lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip, workflows, patchwork, thematic, showtext, glue, bsicons,
               tmap, sf,terra, gstat, automap, ggstatsplot
               )

weatherdata <-read_rds("data/weather_data_imputed.rds")

weatherdata_summary <-  weatherdata %>%
  group_by(station)%>%
  summarise(average_temperature = round(mean(mean_monthly_temperature),1),
            max_temperature = max(max_monthly_temperature),
            min_monthly_temperature = min(min_monthly_temperature),
            total_rainfall = round(sum(monthly_rainfall), 0)) 

weatherdata_cda <- weatherdata %>%
  mutate(MONTH = month(tdate),
         YEAR = year(tdate))

##### Geospatial data #######
wdata_sf <- read_rds("data/weatherdata_wstations.rds")

mpsz2019 <- st_read(dsn = "data/geospatial",
                    layer = "MPSZ-2019") %>% 
  st_transform(crs = 3414)
##### Geospatial data #######

####### Aesthetics of the dashboard ###################
# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  bslib = "shiny",
  base_font = font_google("Roboto")
) 

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")
####### Aesthetics of the dashboard ###################

####### Overview Page's Value Boxes ###################

vbs <- list(
  fill = FALSE, 
  #1st value box 
  value_box(
    title = "Highest Monthly Temp",
    value = textOutput("hotyear"),
    showcase = bs_icon("thermometer-high"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#fa2e05", fg = "#f0f2f5"),
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  #2nd value box 
  value_box(
    title = "Coolest Monthly Temp",
    value = textOutput("coolyear"),
    showcase = bs_icon("thermometer-low"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#e7ebbc", fg ="#1a1818"),
    full_screen = FALSE, fill = TRUE, height = NULL
    #p("At XX station on MM YYYY")
  ),
  
  #3rd value box 
  value_box(
    title = "Average Monthly Temp",
    value = textOutput("avgtemp"),
    showcase = bs_icon("thermometer-sun"),
    showcase_layout = "top right",
    theme = "orange",
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  #4th value box 
  value_box(
    title = "Lowest Monthly Rainfall",
    value = textOutput("lowrain"),
    showcase = bs_icon("cloud-drizzle"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#a1d5f0", fg = "#1a1818"),
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  #5th value box 
  value_box(
    title = "Highest Monthly Rainfall",
    value = textOutput("highrain"),
    showcase = bs_icon("cloud-rain-heavy"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#065299", fg = "#f0f2f5"),
    full_screen = FALSE, fill = TRUE, height = NULL
    
  ),
  
  #6th value box 
  value_box(
    title = "Average Monthly Rainfall",
    value = textOutput("avgrain"),
    showcase = bs_icon("umbrella-fill"),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#6dc2a3",fg = "#1a1818"),
    full_screen = FALSE, fill = TRUE, height = NULL
  )
)

####### Overview Page's Value Boxes ###################

####################### UI Start ######################################
ui <- page_navbar(
  title = "Rain or Shine: Exploring the mysteries of Singapore Weather",
  nav_spacer(),
  nav_panel(title = "Dashboard",
            layout_columns(
              col_widths = c(5,7,6,6),
              row_heights = c(2,3), 
              
              #value boxes 
              card(layout_column_wrap(
                  !!!vbs)),
              
              #datatable 
              card(card_body(DT::dataTableOutput(outputId = "dtTable"))),
              
              #map 
              card(
                height = 350,
                card_header("Temperature and Rainfall Readings Across Singapore"),
                layout_sidebar(
                  sidebar = sidebar(
                    selectInput(
                      inputId = "map_input",
                      label = "Select a Variable",
                      choices = c("Mean Temperature" = "avg_temperature",
                                  "Maximum Temperature" = "max_temperature",
                                  "Minimum Temperature" = "min_temperature",
                                  "Total Rainfall" = "total_rainfall"),
                      selected = "avg_temperature")),
                  card_body(tmapOutput("mapplot")))                   
              ), 
              
              #plot 
              card(
                card_header("Monthly Temperature and Rainfall Readings"), 
                layout_sidebar(
                  sidebar = sidebar(
                    #open = FALSE,
                    selectInput(
                      inputId = "plot_input1",
                      label = "Select a Station",
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
                    
                    selectInput(
                      inputId = "plot_input2",
                      label = "Select a Variable",
                      choices = c("Mean Temperature" = "mean_monthly_temperature",
                                  "Maximum Temperature" = "max_monthly_temperature",
                                  "Minimum Temperature" = "min_monthly_temperature",
                                  "Total Rainfall" = "monthly_rainfall"),
                      selected = "mean_monthly_temperature")
                    ),
                  card_body(plotOutput(outputId = "stationplot")))
  ))),
  
  nav_panel(title = "Comparison Sandbox", p("Second page content.")),
  
# comparsion analysis
  nav_panel(title = "Comparison Analysis", 
            navset_card_tab(
              sidebar = sidebar("Please make the following selections",
                                selectInput(inputId = "cda_variable",
                                            label = "Variable to compare",
                                            choices = c("Mean Temperature" = "mean_monthly_temperature",
                                                        "Maximum Temperature" = "max_monthly_temperature",
                                                        "Minimum Temperature" = "min_monthly_temperature",
                                                        "Total Rainfall" = "monthly_rainfall"),
                                            selected = "MEAN_TEMP"), 
                                
                                selectInput(inputId = "cda_testtype",
                                            label = "Type of Statisitcal Approach",
                                            choices = c("Parametric" = "p",
                                                        "Nonparametric" = "np",
                                                        "Robust" = "robust",
                                                        "Bayes" = "bayes"),
                                            selected = "np"),
                                
                                
                                sliderInput(inputId = "cda_conflevel",
                                            label = "Confidence Level",
                                            min = 0.95,
                                            max = 0.99,
                                            value = 0.95,
                                            step = 0.01), 
                                
                                selectInput(inputId = "cda_pairwise",
                                            label = "Pairwise Display",
                                            choices = list("significant" = "s", "non-significant" = "ns"),
                                            selected = "s"),
                                
                                actionButton(inputId = "cda_plotdata",
                                             label = "Start Comparing!")),
              
              nav_panel("Comparison Between Years",
                        card_body(plotOutput("cda_YearsPlot"))),
              nav_panel("Comparison Between Months", 
                        card_body(plotOutput("cda_MonthsPlot"))), 
              nav_panel("Comparison Between Stations",
                        card_body(plotOutput("cda_StationsPlot")))

            
            )), # End of Comparison Analysis 
  nav_menu(
    title = "Forecasting",
    nav_panel(title = "Exponential Smoothing",
              navset_card_tab(
                sidebar = sidebar("Please make the following selections",
                                  selectInput(inputId = "sh_station",
                                              label = "Weather Station",
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
                                              selected = "Changi"),
                                  selectInput(inputId = "sh_variable",
                                              label = "Variable to forecast",
                                              choices = c("Mean Temperature" = "mean_monthly_temperature",
                                                          "Maximum Temperature" = "max_monthly_temperature",
                                                          "Minimum Temperature" = "min_monthly_temperature",
                                                          "Total Rainfall" = "monthly_rainfall"),
                                              selected = "mean_monthly_temperature"), 
                                  actionButton(inputId = "sh_plotdata",
                                               label = "Start!")),
                nav_panel("Seasonal-Trend-Loess (STL) Decomposition",
                            layout_sidebar(
                              sidebar = sidebar(
                                checkboxGroupInput(inputId = "sh_decompose",
                                                   label = "Select visualisation",
                                                   choices = c("Observed" = "observed",
                                                               "Season" = "season",
                                                               "Trend" = "trend",
                                                               "Remainder" = "remainder",
                                                               "Seasonal Adjusted" = "seasadj"),
                                                   selected = c("observed","season", "trend", "remainder"))),
                              card_body(plotOutput("sh_DecompositionPlot")))                   
                ),
                nav_panel("Validation and Forecasting",
                          layout_sidebar(
                            sidebar = sidebar(
                              sliderInput("sh_traindata",
                                          label = "Select the amount of Training Data to use", 
                                          min = 0.6,
                                          max = 1,
                                          value = 0.8,
                                          step = 0.05),
                              
                              sliderInput("sh_forecasthorizon",
                                          label = "Select the Forecast Horizon", 
                                          min = 1,
                                          max = 120,
                                          value = 36,
                                          step = 1),
                              
                              selectInput(inputId = "sh_seasonalperiod",
                                          label = "Select a seasonal frequency",
                                          choices = c("Auto" = "auto",
                                                      "1 month" = "1",
                                                      "3 months" = "3",
                                                      "12 months" = "12"),
                                          selected = "auto"),
                              
                              radioButtons(inputId = "sh_error",
                                           label = "Select the form of error term",
                                           choices = c("Auto" = "auto",
                                                       "Additive" = "additive",
                                                       "Multiplicative" = "multiplicative"),
                                           selected = "auto"), 
                              radioButtons(inputId = "sh_trend",
                                           label = "Select the form of trend term",
                                           choices = c("Auto" = "auto",
                                                       "Additive" = "additive",
                                                       "Multiplicative" = "multiplicative"),
                                           selected = "auto"), 
                              radioButtons(inputId = "sh_season",
                                           label = "Select the form of season term",
                                           choices = c("Auto" = "auto",
                                                       "Additive" = "additive",
                                                       "Multiplicative" = "multiplicative",
                                                       "None" = "none"),
                                           selected = "auto"), 
                              radioButtons(inputId = "sh_damp",
                                           label = "Select the form of damping term",
                                           choices = c("Auto" = "auto",
                                                       "Damped" = "damped",
                                                       "None" = "none"),
                                           selected = "auto"), 
                              actionButton(inputId = "sh_forecast",
                                           label = "Validate and Forecast!")
                            ), 
                            card(plotOutput("sh_ValidationPlot")),
                            card(plotOutput("sh_ForecastPlot")),
                            layout_column_wrap(
                            value_box(
                              title = "MAE",
                              value = textOutput("mae"),
                              #showcase = bs_icon("clipboard2-check"),
                              #showcase_layout = "top right",
                              theme = value_box_theme(bg = "#FDF8AC", fg = "#080707"),
                              full_screen = FALSE, fill = TRUE, height = NULL
                            ),
                            value_box(
                              title = "MAPE",
                              value = textOutput("mape"),
                              #showcase = bs_icon("clipboard2-check"),
                              #showcase_layout = "top right",
                              theme = value_box_theme(bg = "#FDF8AC", fg = "#080707"),
                              full_screen = FALSE, fill = TRUE, height = NULL
                            ),
                            value_box(
                            title = "RMSE",
                            value = textOutput("rmse"),
                            #showcase = bs_icon("clipboard2-check"),
                            #showcase_layout = "top right",
                            theme = value_box_theme(bg = "#FDF8AC", fg = "#080707"),
                            full_screen = FALSE, fill = TRUE, height = NULL
                          ))
                                      
                          ))
              )),# end of ETS
    nav_panel(title = "ARIMA",
              
              
              
              ), #end of ARIMA nav_panel 
    align = "left"
      )
    )
####################### UI END ######################################


####################### SERVER START ######################################
server <- function(input, output){
  
  output$hotyear <- renderText({
    paste(max(weatherdata$max_monthly_temperature),"°C")
  })
  
  output$coolyear <- renderText({
    paste(min(weatherdata$min_monthly_temperature),"°C")
  })
  
  output$avgtemp <- renderText({
    paste(round(mean(weatherdata$mean_monthly_temperature),1),"°C")
  })
  
  output$lowrain <- renderText({
    paste(min(weatherdata$monthly_rainfall),"mm")
  })
  
  output$highrain <- renderText({
    paste(max(weatherdata$monthly_rainfall),"mm")
  })
  
  output$avgrain <- renderText({
    paste(round(mean(weatherdata$monthly_rainfall),0),"mm")
  })
  
  output$dtTable <- DT::renderDataTable({
    DT::datatable(data = weatherdata_summary,
                  options = list(pageLength = 3),
                  rownames = FALSE,
                  colnames = c('Station', 'Mean Temp', 'Max Temp', 'Min Temp', 'Total Rainfall'),
                  DT:::DT2BSClass(c('compact', 'cell-border')))
  })
  
##### Geospatial Map Server #######
  output$mapplot <- renderTmap({
    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    
    tm_shape(mpsz2019, 
             bbox = st_bbox(wdata_sf)) + 
      tm_borders() + 
      tm_shape(wdata_sf) + 
      tm_dots(col = input$map_input) 
    
  })
##### Geospatial Map Server #######
  
 ##### Plot in Overview Page Server ###### 
  selected_weatherstation <- reactive({ 
    weatherdata %>%
    filter(station %in% input$plot_input1) %>%
    select(date = tdate, value = input$plot_input2)
  }) 
    
  output$stationplot <- renderPlot({
    req(selected_weatherstation())
    ggplot(data = selected_weatherstation(),
           aes_string(x = "date", y = "value")) +
      geom_line()
  })
############# Plot in Overview Page Server End #######################
  
############# Comparison Analysis Server Start ########################
  weatherdata_cda_comparison <- eventReactive(input$cda_plotdata, {
    weatherdata_cda 
  })
  
  output$cda_YearsPlot <- renderPlot({
    ggbetweenstats(
      data = weatherdata_cda_comparison(),
      x = YEAR, 
      y = !!input$cda_variable,
      type = input$cda_testtype,
      pairwise.display = input$cda_pairwise,
      conf.level = input$cda_conflevel,
      results.subtitle = TRUE,
      messages = FALSE
      #title="Distribution of Rainfall across 10 years (2014 to 2023)",
      #ylab = "Rainfall volume (mm)",
      #xlab = "Year",
      #ggsignif.args = list(textsize = 5)
    ) 
  })

  
  output$cda_MonthsPlot <- renderPlot({
    ggbetweenstats(
      data = weatherdata_cda_comparison(),
      x = MONTH, 
      y = !!input$cda_variable,
      type = input$cda_testtype,
      pairwise.display = input$cda_pairwise,
      conf.level = input$cda_conflevel,
      results.subtitle = TRUE,
      messages = FALSE
      #title="Distribution of Rainfall across 10 years (2014 to 2023)",
      #ylab = "Rainfall volume (mm)",
      #xlab = "Year",
      #ggsignif.args = list(textsize = 5)
    ) 
  })
  
  output$cda_StationsPlot <- renderPlot({
    ggbetweenstats(
      data = weatherdata_cda_comparison(),
      x = station, 
      y = !!input$cda_variable,
      type = input$cda_testtype,
      pairwise.display = input$cda_pairwise,
      conf.level = input$cda_conflevel,
      results.subtitle = TRUE,
      messages = FALSE
      #title="Distribution of Rainfall across 10 years (2014 to 2023)",
      #ylab = "Rainfall volume (mm)",
      #xlab = "Year",
      #ggsignif.args = list(textsize = 5)
    ) 
  })
  
############# Compare Between Years Server End ########################
  
############# Decomposition Page Server Start ########################
  
  ## Filter Data based on selected station and weather variable 
  selectedData <- eventReactive(input$sh_plotdata, {
    weatherdata %>%
      filter(station %in% input$sh_station)%>%
      select(station, date = tdate, value = input$sh_variable)
  })
  
  output$sh_DecompositionPlot <- renderPlot({
    req(selectedData())
    plot_stl_diagnostics(selectedData(), date, value,
                         .feature_set = input$sh_decompose,
                         .interactive = FALSE)
  })
############# Decomposition Page Server ########################
  
############# Start of Validation and Forecast Page Server ########################
  ## split the data based on the training proportion chosen 
  splits <- eventReactive(input$sh_forecast,{
    req(selectedData())
    initial_time_split(selectedData(), prop = input$sh_traindata)
  })
  
  train_data <- reactive({ 
    training(splits()) 
  })
  
  test_data <- reactive({ 
    testing(splits()) 
  })
  
  ## Fit the model based on the parameters 
  model_fit_ets <- reactive({
    exp_smoothing(
      seasonal_period = input$sh_seasonalperiod, 
      error = input$sh_error, 
      trend = input$sh_trend,
      season = input$sh_season, 
      damping = input$sh_damp
    ) %>%
      set_engine(engine = "ets") %>%
      fit(value ~ date, data = train_data())
  })
  
  ## Add Fitted Model to a Model table 
  model_ets_table <- reactive({
    req(model_fit_ets())
    modeltime_table(model_fit_ets())
  })
  
  
  ## Calibrate model to test data
  calibration_ets <- reactive({
    req(model_ets_table(), test_data())
    model_ets_table() %>%
      modeltime_calibrate(new_data = test_data())
  })
  
  calibration_results <- reactive({
    req(calibration_ets())
    calibration_ets() %>%
      modeltime_forecast(new_data = test_data(),
                         actual_data = selectedData())
  })
  
  ## Plot forecasted and actual test data
  output$sh_ValidationPlot <- renderPlot({
    req(calibration_results())
    calibration_results() %>%
      plot_modeltime_forecast(.interactive = FALSE, .title = "Plot using Test Data")
  })
  
  ## refit to full dataset & forecast forward 
  refit <- reactive({
    calibration_ets() %>%
      modeltime_refit(data = selectedData())
  })
  
  ## Plot the forecasted horizon 
  output$sh_ForecastPlot <- renderPlot({
    req(refit())
    refit() %>%
      modeltime_forecast(h = input$sh_forecasthorizon, actual_data = selectedData()) %>%
      plot_modeltime_forecast(.interactive = FALSE) 
  })
  
  
  calibration_table <- reactive({
    req(model_ets_table(), test_data())
    model_ets_table() %>%
      modeltime_calibrate(test_data())
  })
  
  forecast_table <- reactive({
    req(calibration_table)
    calibration_table() %>%
      modeltime_accuracy() %>%
      select(.model_desc, mae, mape, mase, rmse)
  })
  
  output$mae <- renderText({
    round(forecast_table()$mae,2)
  })
  
  output$mape <- renderText({
    round(forecast_table()$mape,2)
  })
  
  output$rmse <- renderText({
    round(forecast_table()$rmse,2)
  })
############# End of Validation and Forecast Page Server ########################
  
}

####################### End of SERVER ######################################

shinyApp(ui, server)