pacman::p_load(tidyverse, shiny, bslib, 
               lubridate, DT, ggplot2, plotly, ggthemes, hrbrthemes, timetk, modeltime, tidymodels, 
               xgboost, recipes, parsnip, workflows, patchwork, thematic, showtext, glue, bsicons,
               tmap, sf,terra, gstat, automap, ggstatsplot, ggridges,ggrepel,ggsignif,gifski, 
               gganimate,ggiraph,magick,car, shinycssloaders
)

weatherdata <-read_rds("data/weather_data_imputed.rds")
weatherdata2 <- read_rds("data/weatherdata2.rds")

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

grid <- terra::rast(mpsz2019,
                    nrows = 690,  
                    ncols = 1075)

xy <- terra::xyFromCell(grid, 
                        1:ncell(grid))

coop <- st_as_sf(as.data.frame(xy), 
                 coords = c("x", "y"),
                 crs = st_crs(mpsz2019))

coop <- st_filter(coop, mpsz2019)

##### Geospatial data #######

########### EDA data ################
rainfall_data_year <- read_csv("data/rainfall_data_year.csv")
rainfall_data_month <- read_csv("data/rainfall_data_month.csv")
rainfall_data_stn <- read_csv("data/rainfall_data_stn.csv")
temp_year <- read_csv("data/temp_year.csv")
temp_month <- read_csv("data/temp_month.csv")
temp_stn <- read_csv("data/temp_stn.csv")
combined_data <- read_csv("data/combined_data.csv")
combined_data2 <- read_csv("data/combined_data2.csv")
combined_data3 <- read_csv("data/combined_data3.csv")
########### EDA data ################

###### hline values for cycle plots ########

hline.data <- rainfall_data_month %>%
  group_by(month) %>%
  summarise(avgvalue = mean(monthly_rainfall))

hline_mean_temp.data <- temp_month %>%
  group_by(year) %>%
  summarise(avgvalue = mean(meantemp))

hline_max_temp.data <- temp_month %>%
  group_by(year) %>%
  summarise(avgvalue = mean(maxtemp))

hline_min_temp.data <- temp_month %>%
  group_by(year) %>%
  summarise(avgvalue = mean(mintemp))

hline_mean_temp_mth.data <- temp_month %>%
  group_by(month) %>%
  summarise(avgvalue = mean(meantemp))

hline_max_temp_mth.data <- temp_month %>%
  group_by(month) %>%
  summarise(avgvalue = mean(maxtemp))

hline_min_temp_mth.data <- temp_month %>%
  group_by(month) %>%
  summarise(avgvalue = mean(mintemp))

###### hline values for cycle plots ########

####### Aesthetics of the dashboard ###################
# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  "navbar-bg" = "#EEEDDB",
  bg = "#F6F7FA",
  fg = "#2D2F32",
  primary = "#0B63B5",
  
  # Controls the accent (e.g., hyperlink, button, etc) colors
  secondary = "#22556E",
  base_font = c("Helvetica", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  
  # Can also add lower-level customization
  "input-border-color" = "#FEB200"
) 

# Let thematic know to use the font from bs_lib
#thematic_shiny(font = "Roboto")

####### Aesthetics of the dashboard ###################

####### Overview Page's Value Boxes ###################

vbs <- list(
  fill = FALSE, 
  
  #1st value box 
  value_box(
    title = "Highest Monthly Temperature",
    value = textOutput("hotyear"),
    showcase = bs_icon("thermometer-high",size = 40),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#FE6402", fg = "#f0f2f5"),
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  #2nd value box 
  value_box(
    title = "Average Monthly Temperature",
    value = textOutput("avgtemp"),
    showcase = bs_icon("thermometer-sun", size = 40),
    showcase_layout = "top right",
    theme = value_box_theme(bg ="#FEB200"),
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  #3rd value box 
  value_box(
    title = "Lowest Monthly Temperature",
    value = textOutput("coolyear"),
    showcase = bs_icon("thermometer-low", size = 40),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#FDF8AC", fg ="#1a1818"),
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  
  #4th value box 
  value_box(
    title = "Highest Monthly Rainfall ",
    value = textOutput("highrain"),
    showcase = bs_icon("cloud-rain-heavy", size = 40),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#052E55", fg = "#DBDFE3"),
    full_screen = FALSE, fill = TRUE, height = NULL
    
  ),
  
  #5th value box 
  value_box(
    title = "Average Monthly Rainfall ",
    value = textOutput("avgrain"),
    showcase = bs_icon("umbrella-fill", size = 40),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#0B63B5",fg = "#DBDFE3"),
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  
  #6th value box 
  value_box(
    title = "Lowest Monthly Rainfall ",
    value = textOutput("lowrain"),
    showcase = bs_icon("cloud-drizzle", size = 40),
    showcase_layout = "top right",
    theme = value_box_theme(bg = "#92BCE3", fg = "#1a1818"),
    full_screen = FALSE, fill = TRUE, height = NULL
  )
)

####### Overview Page's Value Boxes ###################

####################### UI Start ######################################
ui <- page_navbar(
  bg = "#94AFBF",

  title = "Rain, Hail or Shine: Exploring the Mysteries of the Sky",
  theme = bslib::bs_theme(
    bg = "white",
    fg = "#2D2F32",
    primary = "#0B63B5",
  ),
  nav_spacer(),
  nav_panel(title = "Overview",
            layout_columns(
              col_widths = c(5,7,6,6),
              row_heights = c(2,3), 
              
              #value boxes 
              card(layout_column_wrap(
                !!!vbs)),
              
              #datatable 
              card(card_body(DT::dataTableOutput(outputId = "dtTable")%>% withSpinner(color="#0B63B5"))),
              
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
                      selected = "total_rainfall")),
                  card_body(tmapOutput("mapplot")%>% withSpinner(color="#0B63B5")))                   
              ), 
              
              #plot 
              card(
                card_header("Temperature and Rainfall Readings by Stations"), 
                layout_sidebar(
                  sidebar = sidebar(
                    #open = FALSE,
                    selectInput(
                      inputId = "plot_input1",
                      label = "Select a Station",
                      choices = c("All" = "All", 
                                  "Admiralty" = "Admiralty",
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
                      selected = "All"),
                    
                    selectInput(
                      inputId = "plot_input2",
                      label = "Select a Variable",
                      choices = c("Mean Temperature" = "mean_monthly_temperature",
                                  "Maximum Temperature" = "max_monthly_temperature",
                                  "Minimum Temperature" = "min_monthly_temperature",
                                  "Total Rainfall" = "monthly_rainfall"),
                      selected = "mean_monthly_temperature")
                  ),
                  card_body(plotlyOutput(outputId = "stationplot")
                            %>% withSpinner(color="#0B63B5")))
              ))),

  # Exploratory Data Analysis
  
  nav_panel(title = "Exploring Singapore's Weather",
            navset_card_tab(
  
              nav_panel("By Years",
                        layout_columns(
                          col_widths = c(6,6),
                          
                          card(
                            navset_bar(
                              bg = "white",
                              nav_panel("Temperature",
                                        layout_sidebar(
                                          sidebar = sidebar(
                                            selectInput(inputId = "eda_variable1",
                                                        label = "Variable to display",
                                                        choices = c("Mean Temperature" = "meantemp",
                                                                    "Maximum Temperature" = "maxtemp",
                                                                    "Minimum Temperature" = "mintemp"),
                                                        selected = "meantemp")),
                                          card_body(plotOutput("eda_tempdistribution")%>% withSpinner(color="#0B63B5"))
                                        )),
                              
                              nav_panel("Rainfall",
                                card_body(plotOutput("eda_rfdistribution")%>% withSpinner(color="#0B63B5"))
                              ))), 
                          
                          card(
                            navset_tab(
                              nav_panel("Temperature",card_body(fill = TRUE,plotlyOutput("eda_tempdetailed")%>% withSpinner(color="#0B63B5"))),
                              nav_panel("Rainfall",card_body(fill = TRUE,plotlyOutput("eda_rfdetailed")%>% withSpinner(color="#0B63B5")))
                            )),
                          )),
                        
            nav_panel("By Months",
                      layout_columns(
                        col_widths = c(6,6),
      
                        card(card_header("Rainfall Fluctuations by Months"),
                             card_body(plotlyOutput("eda_rfmonthly")%>% withSpinner(color="#0B63B5"))
                        ),
                        
                        card(card_header("Temperature Fluctuations by Months"),
                             #full_screen = TRUE,
                             #fill = TRUE,
                             #height = 500,
                             navset_tab(
                               nav_panel("Mean Temperature",
                                         card_body(plotlyOutput("eda_meantempmonthly")%>% withSpinner(color="#0B63B5"))),
                               nav_panel("Maximum Temperature",
                                         card_body(plotlyOutput("eda_maxtempmonthly")%>% withSpinner(color="#0B63B5"))),
                               nav_panel("Minimum Temperature",
                                         card_body(plotlyOutput("eda_mintempmonthly")%>% withSpinner(color="#0B63B5")))
                             )
                        )
                      )),
            
            nav_panel("By Stations",
                      layout_columns(
                        col_widths = c(6,6),
                        card(
                          card_body(plotlyOutput(outputId = "plot24")%>% withSpinner(color="#0B63B5"))),
                        
                        card(
                          card_body(plotlyOutput(outputId = "plot25")%>% withSpinner(color="#0B63B5"))
                        )
                          
                        )),
            
            nav_panel("Comparisons Between Stations and Variables",
                      layout_columns(
                        col_widths = c(6,6),
                        
                        card("Plot 1",
                          layout_sidebar(
                            sidebar = sidebar(
                              selectInput(
                                inputId = "compare_variable1",
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
                              
                              selectInput(inputId = "compare_variable2",
                                          label = "Variable to display",
                                          choices = c("Mean Temperature" = "mean_monthly_temperature",
                                                      "Maximum Temperature" = "max_monthly_temperature",
                                                      "Minimum Temperature" = "min_monthly_temperature",
                                                      "Total Rainfall" = "monthly_rainfall"),
                                          selected = "mean_monthly_temperature"),
                              
                              textInput(inputId = "compare_title1",
                                        label = "Chart Title",
                                        value = ""), 
                              
                              textInput(inputId = "compare_xaxis1",
                                        label = "X Axis Title",
                                        value = ""), 
                              
                              textInput(inputId = "compare_yaxis1",
                                        label = "Y Axis Title",
                                        value = "")),
                      
                            card_body(plotlyOutput("compare_weather1")%>% withSpinner(color="#0B63B5"))
                          )), 
                        
                        card("Plot 2",
                             layout_sidebar(
                               sidebar = sidebar(
                                 selectInput(
                                   inputId = "compare_variable3",
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
                                   selected = "Changi"),
                                 
                                 selectInput(inputId = "compare_variable4",
                                             label = "Variable to display",
                                             choices = c("Mean Temperature" = "mean_monthly_temperature",
                                                         "Maximum Temperature" = "max_monthly_temperature",
                                                         "Minimum Temperature" = "min_monthly_temperature",
                                                         "Total Rainfall" = "monthly_rainfall"),
                                             selected = "mean_monthly_temperature"),
                                 textInput(inputId = "compare_title2",
                                           label = "Chart Title",
                                           value = ""), 
                                 
                                 textInput(inputId = "compare_xaxis2",
                                           label = "X Axis Title",
                                           value = ""), 
                                 
                                 textInput(inputId = "compare_yaxis2",
                                           label = "Y Axis Title",
                                           value = "")),
                               card_body(plotlyOutput("compare_weather2")%>% withSpinner(color="#0B63B5"))
                             )), 
                      ))
            )), # end of EDA 
  
  # confirmatory data analysis
  nav_panel(title = "Confirmatory Data Analysis", 
            navset_card_tab(
              sidebar = sidebar("Make the following selections",
                                selectInput(inputId = "cda_variable",
                                            label = "Variable to compare",
                                            choices = c("Mean Temperature" = "mean_monthly_temperature",
                                                        "Maximum Temperature" = "max_monthly_temperature",
                                                        "Minimum Temperature" = "min_monthly_temperature",
                                                        "Total Rainfall" = "monthly_rainfall"),
                                            selected = "mean_monthly_temperature"), 
                                
                                selectInput(inputId = "cda_testtype",
                                            label = "Type of Statisitcal Approach",
                                            choices = c("Parametric" = "p",
                                                        "Nonparametric" = "np",
                                                        "Robust" = "robust",
                                                        "Bayes" = "bayes"),
                                            selected = "np"),
                                
                                
                                sliderInput(inputId = "cda_conflevel",
                                            label = "Confidence Level",
                                            min = 0.90,
                                            max = 0.99,
                                            value = 0.95,
                                            step = 0.01), 
                                
                                selectInput(inputId = "cda_pairwise",
                                            label = "Pairwise Display",
                                            choices = list("Significant" = "s", "Non-significant" = "ns"),
                                            selected = "s"),
                                
                                actionButton(inputId = "cda_plotdata",
                                             label = "Initiate Comparison")),
              
              nav_panel("Comparison Between Years",
                        card_body(plotOutput("cda_YearsPlot")%>% withSpinner(color="#0B63B5"))),
              nav_panel("Comparison Between Months", 
                        card_body(plotOutput("cda_MonthsPlot")%>% withSpinner(color="#0B63B5"))), 
              nav_panel("Comparison Between Stations",
                        card_body(plotOutput("cda_StationsPlot")%>% withSpinner(color="#0B63B5")))

            )), # End of CDA 
  nav_menu(
    title = "Forecasting",
    nav_panel(title = "ETS",
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
                                               label = "Initiate Decomposition")),
                nav_panel("Seasonal-Trend-Loess Decomposition",
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
                            card_body(plotlyOutput("sh_DecompositionPlot")%>% withSpinner(color="#0B63B5")))                   
                ),
                nav_panel("Validation and Forecasting",
                          layout_sidebar(
                            sidebar = sidebar(
                              sliderInput("sh_traindata",
                                          label = "Select Amount of Training Data to use", 
                                          min = 0.6,
                                          max = 1,
                                          value = 0.8,
                                          step = 0.05),
                              
                              sliderInput("sh_forecasthorizon",
                                          label = "Select Forecast Horizon", 
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
                                           label = "Initiate Validation and Forecasting")
                            ), 
                            card(plotlyOutput("sh_ValidationPlot")%>% withSpinner(color="#0B63B5"),
                                 fill = FALSE),
                            card(plotlyOutput("sh_ForecastPlot")%>% withSpinner(color="#0B63B5"), 
                                 fill = FALSE),
                            layout_column_wrap(
                              value_box(
                                title = "MAE",
                                value = textOutput("mae"),
                                theme = value_box_theme(bg = "#FDF8AC", fg = "#080707"),
                                #full_screen = FALSE, fill = TRUE, height = NULL
                              ),
                              value_box(
                                title = "MAPE",
                                value = textOutput("mape"),
                                theme = value_box_theme(bg = "#FDF8AC", fg = "#080707"),
                                #full_screen = FALSE, fill = TRUE, height = NULL
                              ),
                              value_box(
                                title = "RMSE",
                                value = textOutput("rmse"),
                                theme = value_box_theme(bg = "#FDF8AC", fg = "#080707"),
                                #full_screen = FALSE, fill = TRUE, height = NULL
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
                  options = list(pageLength = 3, lengthMenu = c(3,13)),
                  rownames = FALSE,
                  colnames = c('Station', 'Mean Temp', 'Max Temp', 'Min Temp', 'Total Rainfall'),
                  DT:::DT2BSClass(c('compact', 'cell-border')))
  })
  
  ##### Geospatial Map Server Start #######
  
  output$mapplot <- renderTmap({
    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    
    tm_shape(mpsz2019, 
             bbox = st_bbox(wdata_sf)) + 
      tm_borders(alpha = 0.2) + 
      tm_shape(wdata_sf) + 
      tm_bubbles(col = input$map_input, size = input$map_input,
                 border.col = "black",
                 border.lwd = 1) +
      tm_compass(type="8star", size = 2) +
      tm_scale_bar() +
      tm_grid(alpha =0.2) 
  })
    
  ##### Geospatial Map Server End #######
  
  ##### Plot in Overview Page Server ###### 
  selected_weatherstation <- reactive({ 
    weatherdata2 %>%
      filter(station %in% input$plot_input1) %>%
      select(date = tdate, value = input$plot_input2)
  }) 
  
  output$stationplot <- renderPlotly({
    req(selected_weatherstation())
    D1 <- ggplot(data = selected_weatherstation(),
           aes_string(x = "date", y = "value")) +
      geom_line() +       
      theme_minimal() +
      theme(panel.border = element_rect(color = "lightgrey",linetype = "dashed", fill = NA, size = 1)) +
      labs(title=paste0(input$plot_input1," Station(s) from 2014 to 2023"),
           x="Year",
           y= paste0(input$plot_input2))
    
    D1 <- ggplotly(D1)
    
    D1

  })
  ############# Plot in Overview Page Server End #######################
  
  ############# EDA Server Start ########################
  
  ### By years 
  
  #Ridge plot
  output$eda_tempdistribution <- renderPlot({
    
    ggplot(temp_month, 
           aes_string(x = input$eda_variable1, 
                      y = "as.factor(year)", 
                      fill = "0.5 - abs(0.5-stat(ecdf))")) +
      stat_density_ridges(geom = "density_ridges_gradient", 
                          calc_ecdf = TRUE) +
      scale_fill_viridis_c(name = "Tail probability",
                           direction = -1,
                           option="turbo")+
      theme_ridges()+
      scale_color_discrete(name = "Year") +
      labs(title=paste0("Distribution of ", input$eda_variable1," from 2014 to 2023"),
           y="Year",
           x="Temperature (°C)")
    
  })
  
  output$eda_rfdistribution <- renderPlot({
    ggplot(rainfall_data_month, 
           aes(x = monthly_rainfall,
               y = as.factor(year), 
               fill = 0.5 - abs(0.5-stat(ecdf)))) +
      stat_density_ridges(geom = "density_ridges_gradient", 
                          calc_ecdf = TRUE) +
      scale_fill_viridis_c(name = "Tail probability",
                           direction = -1,
                           option="turbo")+
      theme_ridges()+
      labs(title="Distribution of Monthly Rainfall from 2014 to 2023",
           y="Year",
           x="Rainfall Volume (mm)")
  })
  
  # Line / bar chart 
  
  output$eda_tempdetailed <- renderPlotly({
    combined_data2 <- reshape2::melt(temp_month, id.vars = c("year", "month"), variable.name = "temperature_type")
    
    p17 <- ggplot(combined_data2, aes(x = month, 
                                      y = value,
                                      color = temperature_type)) +
      geom_line() +
      facet_wrap(~ year, scales = "free")+
      labs(title = "Temperature Trends from 2014 to 2023",
           y = "Temperature (°C)",
           x = "Month",
           color = "Temperature Type") +
      scale_x_continuous(breaks = seq(1,12, 1)) +
      scale_color_manual(values = c("turquoise", "violetred2", "steelblue2"), 
                         labels = c("Mean", "Max", "Min")) +
      theme_minimal(base_family = "Helvetica")+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            text=element_text(size=10),
            legend.position = "none",
            panel.spacing.x = unit(0.5, "lines"),
            panel.spacing.y = unit(1, "lines"))
    
    p17 <- ggplotly(p17, tooltip = "all") %>%
      layout(legend = list(x = 0.6, y = 0.08))
    
    p17
  })
  
  output$eda_rfdetailed <- renderPlotly({
    ggplotly(ggplot(rainfall_data_month,
                    aes(y=monthly_rainfall,
                        x = as.factor(month),
                        fill = as.factor(year))) +
               geom_bar(stat = "identity")+
               facet_wrap(~year, scales = "free") +
               labs(title="Monthly rainfall each year from 2014 to 2023",
                    y = "Rainfall volume (mm)",
                    x = "Month") +
               theme_minimal()+
               theme(panel.spacing.y = unit(0.3, "lines"),text=element_text(size=10),
                     legend.position = "none")+
               scale_fill_discrete(name = "Year"))
  })
  
  ### By Months
  
  output$eda_rfmonthly <- renderPlotly({
    ggplot() +
      geom_line(data = rainfall_data_month,
                aes(x = year,
                    y = monthly_rainfall,
                    group = month,
                    colour = as.factor(month)))+
      geom_hline(aes(yintercept=avgvalue),
                 data=hline.data,
                 linetype=6,
                 colour="red",
                 size=0.5)+
      facet_wrap(~month, scales = "free")+
      labs(title = "Rainfall by Months from 2014 to 2023",
           colour = "Month") +
      xlab("Year")+
      ylab("Rainfall volume (mm)")+
      theme_minimal(base_family = "Helvetica")+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            legend.position = "none")
    
  })
  
  output$eda_meantempmonthly<- renderPlotly({
    
    ggplot() +
      geom_line(data = temp_month,
                aes(x = year,
                    y = meantemp,
                    group = month,
                    colour = as.factor(month)))+
      geom_hline(aes(yintercept=avgvalue),
                 data=hline_mean_temp_mth.data,
                 linetype=6,
                 colour="red",
                 size=0.5)+
      facet_wrap(~month,scales = "free")+
      labs(title = "Mean temperature by Months from 2014 to 2023")+
      xlab("Year")+
      ylab("Degrees (°C)")+
      #scale_color_discrete(name = "Year")+
      theme_minimal(base_family = "Helvetica")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            legend.position = "none")
  })

  output$eda_maxtempmonthly <- renderPlotly({
    
    ggplot() +
      geom_line(data = temp_month,
                aes(x = year,
                    y = maxtemp,
                    group = month,
                    colour = as.factor(month)))+
      geom_hline(aes(yintercept=avgvalue),
                 data=hline_max_temp_mth.data,
                 linetype=6,
                 colour="red",
                 size=0.5)+
      facet_wrap(~month,scales = "free")+
      labs(title = "Max temperature by Months from 2014 to 2023")+
      xlab("Year")+
      ylab("Degrees (°C)")+
      #scale_color_discrete(name = "Year")+
      theme_minimal(base_family = "Helvetica")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            legend.position = "none")

  })

  output$eda_mintempmonthly <- renderPlotly({
    ggplot() +
      geom_line(data = temp_month,
                aes(x = year,
                    y = mintemp,
                    group = month,
                    colour = as.factor(month)))+
      geom_hline(aes(yintercept=avgvalue),
                 data=hline_min_temp_mth.data,
                 linetype=6,
                 colour="red",
                 size=0.5)+
      facet_wrap(~month,scales = "free")+
      labs(title = "Max temperature by Months from 2014 to 2023")+
      xlab("Year")+
      ylab("Degrees (°C)")+
      #scale_color_discrete(name = "Year")+
      theme_minimal(base_family = "Helvetica")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            legend.position = "none")
    
  })


  ### By Stations 
  output$plot24 <- renderPlotly({
    
    ggplotly(ggplot(rainfall_data_stn,
                    aes(y=yearly_rainfall,
                        x = year,
                        group = station,
                        color = station)) +
               geom_line() +
               facet_wrap(~station,scales = "free_x") +
               labs(title="Rainfall Trends Across Stations from 2014 to 2023",
                    y = "Rainfall volume (mm)",
                    x = "Year") +
               theme_minimal() +
               theme(axis.text.x=element_text(angle=90,hjust=1),
                     panel.spacing.y = unit(0.05,"lines"),
                     legend.position = "none")) 
    
  })
  
  output$plot25 <- renderPlotly({
    
    p29 <- ggplot(combined_data3, aes(x = year, 
                                      y = value,
                                      color = temperature_type)) +
      geom_line() +
      facet_wrap(~ station,scales = "free_x")+
      labs(title = "Temperature Trends Across Stations from 2014 to 2023",
           y = "Temperature (°C)",
           x = "Year",
           color = "Temperature Type") +
      scale_x_continuous(breaks = seq(2014,2023, 1)) +
      scale_color_manual(values = c("turquoise", "violetred2", "steelblue2"), 
                         labels = c("Mean", "Max", "Min")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            panel.spacing.y = unit(0.05,"lines"),
            #panel.border = element_rect(color = "lightgrey",linetype = "dashed", fill = NA, size = 1)
            )
    
    p29 <- ggplotly(p29, tooltip = "all")%>%
      layout(legend = list(x = 0.7, y = 0))
    
    p29
  })
  
  
  ### Comparison 
  selected_weatherstation1 <- reactive({ 
    weatherdata %>%
      filter(station %in% input$compare_variable1) %>%
      select(date = tdate, value = input$compare_variable2)
  }) 
  
  output$compare_weather1 <- renderPlotly({
    req(selected_weatherstation1())
    
    w1 <- ggplot(data = selected_weatherstation1(),
           aes_string(x = "date", y = "value")) +
      geom_line() + theme_minimal() + 
      labs(title = input$compare_title1) +
      xlab(input$compare_xaxis1) +
      ylab(input$compare_yaxis1)
   
    w1 <- ggplotly(w1, tooltip = "all")
    
    w1
  })
  
  selected_weatherstation2 <- reactive({ 
    weatherdata %>%
      filter(station %in% input$compare_variable3) %>%
      select(date = tdate, value = input$compare_variable4)
  }) 
  
  output$compare_weather2 <- renderPlotly({
    req(selected_weatherstation2())
    
    w2 <- ggplot(data = selected_weatherstation2(),
                 aes_string(x = "date", y = "value")) +
      geom_line() + theme_minimal() + 
      labs(title = input$compare_title2) +
      xlab(input$compare_xaxis2) +
      ylab(input$compare_yaxis2)
    
    w2 <- ggplotly(w2, tooltip = "all") 
    
    w2
  })
  
  
  ############# EDA Server End ########################
  
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
      messages = FALSE, 
      title= paste("Comparison of", input$cda_variable,"across 10 years (2014 to 2023)"),
      ylab = input$cda_variable,
      xlab = "Years",
      ggsignif.args = list(textsize = 5)
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
      messages = FALSE,
      title= paste("Comparison of", input$cda_variable, "across months"),
      ylab = input$cda_variable,
      xlab = "Months",
      ggsignif.args = list(textsize = 5)
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
      messages = FALSE,
      title= paste("Comparison of", input$cda_variable, "across weather stations"),
      ylab = input$cda_variable,
      xlab = "Weather Stations",
      ggsignif.args = list(textsize = 5)
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
  
  output$sh_DecompositionPlot <- renderPlotly({
    req(selectedData())
    plot_stl_diagnostics(selectedData(), date, value,
                         .feature_set = input$sh_decompose) %>%
      layout(xaxis = list(tickfont = list(size = 12)),
             yaxis = list(tickfont = list(size = 12)),
             strip.text.x = element_text(size=15)
             ) 
      
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
  output$sh_ValidationPlot <- renderPlotly({
    req(calibration_results())
    calibration_results() %>%
      plot_modeltime_forecast(.title = "Plot using Test Data")
  })
  
  ## refit to full dataset & forecast forward 
  refit <- reactive({
    calibration_ets() %>%
      modeltime_refit(data = selectedData())
  })
  
  ## Plot the forecasted horizon 
  output$sh_ForecastPlot <- renderPlotly({
    req(refit())
    refit() %>%
      modeltime_forecast(h = input$sh_forecasthorizon, actual_data = selectedData()) %>%
      plot_modeltime_forecast( .title = "Plot based on Forecast Horizon") 
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