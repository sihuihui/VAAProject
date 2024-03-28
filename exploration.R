exploreUI <- function(id) {
  
  tagList(
    fluidRow(
      column(
        width = 2,
        box(
          title = "Select Data for First Chart",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          
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
          
          selectInput(inputId = "sh_variable",
                      label = "Select the variable to forecast",
                      choices = c("Mean Temperature" = "mean_monthly_temperature",
                                  "Maximum Temperature" = "max_monthly_temperature",
                                  "Minimum Temperature" = "min_monthly_temperature",
                                  "Total Rainfall" = "monthly_rainfall"),
                      selected = "mean_monthly_temperature"),
          
          # Input: Dates
          dateRangeInput(
            ns("dates"),
            "Date range",
            start = "2014-01-01",
            end = "2023-12-01",
            format = "MM-yyyy"
          )
        )
      ),
      
      column(
        width = 5,
        box(
          title = "Chart 1",
          closable = FALSE,
          width = NULL,
          #height = "400px",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "navy",
          plotlyOutput(
            outputId = "Chart1",
            height = "500px"
          )
        )
      )
      

    ))
}

exploreServer <- function(id, data, left, right) {
  moduleServer(
    id,
    function(input, output, session) {

      selectedData <- reactive(
        weatherdata %>%
          filter(station %in% input$sh_station)
        # mutate(MTHYEAR = my(as.Date(tdate)))
        #select(tdate,input$sh_variable)
      )
      
      ## Plot filtered Data based on selected variable 
      SelectedDataPlot <- ggplot(data = selectedData(),
               aes_string(x = "tdate", y = input$sh_variable)) +
          geom_line() + 
      
      
      output$chart1 <- renderPlotly({
        ggplotly(SelectedDataPlot())
      })
      

      



    }
  )
}