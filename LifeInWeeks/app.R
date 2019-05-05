library(shiny)

ui <- fluidPage(
  
  titlePanel('Life in Weeks'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      wellPanel(dateInput(inputId = 'birth',
                          label = 'Choose Your Birthday',
                          value = '2000-01-01',
                          format = 'yyyy/mm/dd'),
                
                numericInput(inputId = 'lifespan', 
                             label = 'Life Expectancy', 
                             value = 90, 
                             min = 60, 
                             max = 90)),
      
      wellPanel(h5('Source: '),
                tags$a('Your Life in Weeks',
                       href = 'https://waitbutwhy.com/2014/05/life-weeks.html'),
                
                h5('Code: '),
                tags$a('Try2Shiny',
                       href = 'https://github.com/rrtbmxl/Try2Shiny'))
      
    ),
    
    mainPanel(
      plotOutput("lifeplot")
    )
    
  )
  
)


server <- function(input, output) {
  
  observe({
    
    output$lifeplot <- renderPlot({
      
      library(tidyverse)
      library(lubridate)
      
      birth <- input$birth
      lifespan <- input$lifespan
      
      life <- tibble(year = rep(1:lifespan, each = 52), 
                     week = rep(1:52, lifespan),
                     total_week = (year - 1 ) * 52 + week,
                     past = ifelse(total_week <= (year(Sys.time()) - year(birth)) * 52 + week(Sys.time()) - week(birth) - 1,
                                   1, 0) %>% as.factor())
      
      life_plot <- life %>% ggplot(aes(week, year, fill = past)) + 
        geom_point(shape = 22, size = 3, show.legend = FALSE) + 
        scale_y_reverse(breaks = c(1, seq(0, lifespan, 5)), labels = c(1, seq(0, lifespan, 5)), 
                        expand = c(0, 0.5)) + 
        scale_x_continuous(breaks = c(1, seq(0, 50, 5)), labels = c(1, seq(0, 50, 5)),
                           expand = c(0, 0.5), position = "top") + 
        scale_fill_manual(values = c('white', '#E69F00')) + 
        labs(x = 'Week of the Year', y = 'Age', 
             caption = 'rrtbmxl') +
        theme_minimal() + 
        theme(axis.title.x = element_text(hjust = 0, size = 12),
              axis.title.y = element_text(hjust = 1, size = 12),
              axis.text = element_text(size = 10),
              plot.caption = element_text(face = 'italic'))
      
      life_plot
      
    },
    
    height = 960 * as.numeric(input$lifespan) / 90,
    width = 720,
    res = 80
    
    )
    
  })
  
}


shinyApp(ui = ui, server = server)