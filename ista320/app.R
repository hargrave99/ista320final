#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
ufc_data <- read_csv("ufc_data.csv")
glimpse(ufc_data)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("The Ultimate Fighting Championship"),
    sidebarLayout(
        sidebarPanel(
            selectInput("weightclass_id", label = "Weight Class", 
                        choices = ufc_data %>% 
                            select(WeightClass) %>% 
                            distinct() %>%
                            arrange(WeightClass) %>% 
                            as.list()),
            plotOutput("fighter_dif")
        ),
        mainPanel(
            plotOutput("fighter_bar"),
            plotOutput("fighter_line")
        )
    )
)



server <- function(input, output) {
    
    

    output$fighter_dif <- renderPlot({
        ufc_data %>%
            filter(WeightClass == input$weightclass_id) %>%
            group_by(Date, result) %>%
            summarize(mean_splm = mean(splm, na.rm = TRUE)) %>%
            ggplot(aes(x = Date,
                       y = mean_splm,
                       color = result)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = "Strikes Landed per minute over time")
    })
    
    output$fighter_bar <- renderPlot({
        ufc_data %>%
            filter(WeightClass == input$weightclass_id) %>%
            group_by(result) %>%
            summarize(mean_takedown_acc = mean(takedown_acc, na.rm = TRUE)) %>%
            ggplot(aes(x = result,
                       y = mean_takedown_acc)) +
            geom_bar(stat = "identity") +
            labs("Average takedown accuracy between winners and losers of fights")
    })
    output$fighter_line <- renderPlot({
        ufc_data %>%
            ggplot(aes(x = Date,
                       y = sub_average,
                       group = WeightClass)) +
            geom_line() +
            facet_wrap(~WeightClass, ncol = 3) +
            labs(title = "Submission average per weight class over time")
    })
}

shinyApp(ui = ui, server = server)

