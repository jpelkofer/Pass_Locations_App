library(shiny)
library(tidyverse)

pass_plays <- read_csv("pass_plays.csv")



# UI
ui <- pageWithSidebar(
  headerPanel("Pass Location App"),
  sidebarPanel(
    uiOutput("select_defense"),
    uiOutput("select_offense"),
    uiOutput("select_receiver")
  ),
  
  mainPanel(
    plotOutput("passPlot", height = "600px")
  )
)

#server
server <- function(input, output, session) {
  
  rec <- reactive({
    pass_plays %>%
      filter(offense == input$posteam)
  })
  
  output$select_defense <- renderUI({
    selectInput("defense", "Defense:", choices = sort(pass_plays$defensive_team), selected = TRUE)
  })
  
  output$select_offense <- renderUI({
    selectizeInput("offense", "Offense:", choices = sort(pass_plays$posteam), selected = TRUE)
  })
  
  output$select_receiver <- renderUI({
    off_rec <- reactive({
      pass_plays %>%
        filter(posteam == input$offense) %>%
        pull(receiver) %>%
        as.character()
    })
    
    selectInput("receiver", "Receiver", choices = sort(off_rec()), selected = TRUE)
    
  })
  
  output$passPlot <- renderPlot({
    pass_plays %>%
      na.omit() %>%
      filter(defensive_team %in% c(input$defense)) %>%
      ggplot(aes(pass_location, air_yards)) +
      geom_point(aes(colour = as.factor(reception)), size = 10, position = "jitter", alpha = .5) +
      scale_colour_manual(values = c("0" = "red", "1" = "darkgreen"), labels = c("Incomplete", "Complete")) +
      scale_x_continuous(breaks = c(1, 2, 3), labels = c("left", "middle", "right"), limits = c(.5, 3.5)) +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(-10, 60)) +
      geom_point(aes(pass_location, air_yards, shape = input$receiver), data = pass_plays %>%
                   filter(posteam %in% c(input$offense) &
                            receiver %in% c(input$receiver)),
                 position = "jitter", size = 4, alpha = .75) +
      labs(y = "Air Yards",
           x = "Pass Location",
           title = "Pass Defense vs. Receiver Targets",
           subtitle = "*Points do not indicate exact field position. They are binned into left side, middle, and right side of field. 
      Points will shift horizontally within bins when inputs are changed.",
           caption = "Data: nflscrapR, Chart: @pelky33",
           shape = "Target",
           colour = "Pass") +
      theme_bw() +
      theme(plot.title = element_text(size = 24),
            axis.title = element_text(size = 16))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)