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
library(foreign)

anes <- readRDS("chinese_military_threat.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("American Perspectives on China"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("binwidth",
                                     "Width of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
                     ),
                     mainPanel(plotOutput("distPlot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Joshua Berry and I study political science. 
             You can reach me at jberry@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        anes %>%
            filter(china_mil %in% c(1, 2, 3)) %>%
            ggplot(aes(x = china_mil)) +
            geom_histogram(bins = input$binwidth, fill = "firebrick3") +
            scale_x_discrete(limits = c("1", "2", "3"),
                             labels = c("Major threat", "Minor threat", "not a threat")) +
            labs(title = "Americans' Perspectives on the Chinese Military",
                 subtitle = "Policy and attitudes toward China, ANES Survey 2012",
                 x = "Chinese Military Threat",
                 y = "Count") +
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
