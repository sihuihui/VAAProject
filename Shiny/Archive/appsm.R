pacman::p_load(tidyverse,ggridges,ggrepel,ggthemes,ggstatsplot,ggsignif,hrbrthemes,patchwork,dplyr, gifski, 
               gapminder,plotly,gganimate,ggiraph,magick)

weather <-read_rds("data/weather_data_imputed.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Confirmatory Data Analysis"),
  tabsetPanel(
    tabPanel("Are changes in rainfall and temperature over the years statistically significant?",
             sidebarLayout(
               sidebarPanel(
                 selectInput("test1",
                             "Type of test:",
                             c("parametric"="p",
                               "non-parametric"="np",
                               "robust"="r",
                               "bayes factor"="bf"),
                             selected = "np"),
                 selectInput("plot_type1",
                             "Plot Type",
                             c("Boxplot"="box",
                               "Violin"="violin",
                               "Box-Violin"="boxviolin"),
                             selected = "boxviolin"),
                 selectInput("confidence_level1",
                             "Confidence Level",
                             c("99%"="0.99",
                               "95%"="0.95"),
                             selected = "0.99"),
                 radioButtons("pairwise_display1",
                              "Pairwise Display",
                              c("Significant"="significant",
                                "Non-significant"="non-significant"),
                              selected = "significant")
               ),
               mainPanel(plotOutput("Plot1"))
             )),
    tabPanel("Are certain months really drier/wetter or hotter/cooler?",
             sidebarLayout(
               sidebarPanel(
                 selectInput("test2",
                             "Type of test:",
                             c("parametric"="p",
                               "non-parametric"="np",
                               "robust"="r",
                               "bayes factor"="bf"),
                             selected = "np"),
                 selectInput("plot_type2",
                             "Plot Type",
                             c("Boxplot"="box",
                               "Violin"="violin",
                               "Box-Violin"="boxviolin"),
                             selected = "boxviolin"),
                 selectInput("confidence_level2",
                             "Confidence Level",
                             c("99%"="0.99",
                               "95%"="0.95"),
                             selected = "0.99"),
                 radioButtons("pairwise_display2",
                              "Pairwise Display",
                              c("Significant"="significant",
                                "Non-significant"="non-significant"),
                              selected = "significant")
               ),
               mainPanel(plotOutput("Plot2"))
             )),
    tabPanel("Are certain locations drier/wetter or hotter/cooler?",
             sidebarLayout(
               sidebarPanel(
                 selectInput("test3",
                             "Type of test:",
                             c("parametric"="p",
                               "non-parametric"="np",
                               "robust"="r",
                               "bayes factor"="bf"),
                             selected = "np"),
                 selectInput("plot_type3",
                             "Plot Type",
                             c("Boxplot"="box",
                               "Violin"="violin",
                               "Box-Violin"="boxviolin"),
                             selected = "boxviolin"),
                 selectInput("confidence_level3",
                             "Confidence Level",
                             c("99%"="0.99",
                               "95%"="0.95"),
                             selected = "0.99"),
                 radioButtons("pairwise_display3",
                              "Pairwise Display",
                              c("Significant"="significant",
                                "Non-significant"="non-significant"),
                              selected = "significant")
               ),
               mainPanel(plotOutput("Plot3"))
             ))
  )
)

# Define server logic
server <- function(input, output) {
  output$Plot1 <- renderPlot({
    
    weather <- mutate(weather,year=year(lubridate::ymd(tdate)))
    
    ggbetweenstats(
      data = weather,
      x = year, 
      y = monthly_rainfall,
      results.subtitle = TRUE,
      type = input$test1,
      plot.type = input$plot_type1,
      conf.level = input$confidence_level1,
      pairwise.display = input$pairwise_display1,
      messages = FALSE,
      title="Distribution of Rainfall across years",
      ylab = "Rainfall volume (mm)",
      xlab = "Year",
      ggsignif.args = list(textsize = 5)
    ) +
      theme(text = element_text(size = 20), plot.title=element_text(size=20))
  })
    
    output$Plot2 <- renderPlot({
      
      weather <- mutate(weather,month=month(lubridate::ymd(tdate)))
      
      ggbetweenstats(
        data = weather,
        x = month, 
        y = monthly_rainfall,
        type = input$test2,
        plot.type = input$plot_type2,
        conf.level = input$confidence_level2,
        pairwise.display = input$pairwise_display2,
        messages = FALSE,
        title="Distribution of Median Rainfall across months (2014 to 2023)",
        ylab = "Rainfall volume (mm)",
        xlab = "Month",
        ggsignif.args = list(textsize = 5)
      ) +
        theme(text = element_text(size = 20), plot.title=element_text(size=20))
      })
      
      output$Plot3 <- renderPlot({
          ggbetweenstats(
            data = weather,
            x = station, 
            y = monthly_rainfall,
            type = input$test3,
            plot.type = input$plot_type3,
            conf.level = input$confidence_level3,
            pairwise.display = input$pairwise_display3,
            messages = FALSE,
            title="Distribution of Monthly Rainfall by station (2014 to 2023)",
            ylab = "Rainfall volume (mm)",
            xlab = "Station",
            ggsignif.args = list(textsize = 5)
          ) +
            theme(text = element_text(size = 20), plot.title=element_text(size=20))
          
        })
    }

# Run the application 
shinyApp(ui = ui, server = server)



