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
library(shinythemes)

anes <- readRDS("chinese_military_threat.rds")
ccouncil <- 
    load("Chicago_Council/ICPSR_36806/DS0001/36806-0001-Data.rda")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    theme = shinytheme("slate"),
    tabPanel("Model",
             fluidPage(
                 titlePanel("American Perspectives on China"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("binwidth",
                                     "Width of bins:",
                                     min = 1,
                                     max = 20,
                                     value = 5)
                     ),
                     mainPanel(plotOutput("distPlot1"), 
                               plotOutput("distPlot2"),
                               plotOutput("distPlot3")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("To be modified later...
             Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Link to Github repo: 
               https://github.com/jberry2/milestones_final_project.git.
               For this project I intend to study American attitudes
               towards China over a longitudinal time period. I am 
               particuarly interested in how identity factors into American
               attitudes to China. For instance, is a white Republican from
               Alabama more likely to have negative attitudes towards China
               than an Asian-American Democrat Californian? In other words,
               how might Social Identity Theory predict American attitudes
               to China on different dimensions? I plan to use a number of
               datasets to answer these questions including the American 
               National Election Studies (ANES) timseries datasets and 
               the Pew Global Attitudes surveys. Both of these datasets have
               thousands of participants and are conducted by well-respected 
               research organizations within the academic community. ANES is
               conducted by a combined team from the University of Michigan and
               Stanford University. It is run before and after every national 
               presidential election in the United States. The Pew Global
               Attitudes & Trends Survey is conducted by the Pew Research 
               Center on a biannual basis and includes respondents from over
               15 countries each iteration. My hope is to combine the
               relevant datasets over time and see how attitudes towards
               China have changed longitudinally and specifically whether
               there have been correlations in changes in American public
               opinion and increased funding in Chinese public diplomacy
               efforts. This final project for Government 50 will constitute
               one component of my Senior Thesis, required for graduation"),
             h3("About Me"),
             p("My name is Joshua Berry and I study political science at
             Harvard College. 
             You can reach me at jberry@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {

output$distPlot1 <- renderPlot({
anes %>%
    filter(china_mil %in% c(1, 2, 3)) %>%
    ggplot(aes(x = china_mil)) +
    geom_histogram(bins = input$binwidth, fill = "firebrick3", 
                   color = "white") +
    scale_x_discrete(limits = c("1", "2", "3"),
                labels = c("Major threat", "Minor threat", "not a threat")) +
    labs(title = "Americans' Perspectives on the Chinese Military",
            subtitle = "Policy and attitudes toward China, ANES Survey 2012",
            x = "Chinese Military Threat",
            y = "Count") +
            theme_bw()
    })

output$distPlot2 <- renderPlot({
da36806.0001 %>%
    ggplot(aes(x = Q45_6)) +
    geom_histogram(fill = "indianred", binwidth = 1) +
    scale_x_discrete(limits = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    labs(title = "Americans' Feeling Thermometer Towards China ",
            subtitle = "0 = Very Cold; 50 = Not particuarly Warm or Cold;
                100 = Very Warm; Chicago Council Survey 2016",
            x = "American Feelings",
            y = "Count") +
         theme_bw()
    })

output$distPlot3 <- renderPlot({
da36806.0001 %>%
    ggplot(aes(x = Q45_6, fill = Q1025)) +
    geom_boxplot() +
    facet_wrap(~ Q1025) +
    scale_fill_manual(values = c("salmon", "dodgerblue", "gold", "black")) +
    scale_fill_discrete(name = "Political Identification",
                        labels = c("Republican", "Democrat",
                            "Independent", "Not asked Party Identification")) +
    labs(title = "Americans' Feeling Thermometer Towards China ",
        subtitle = "0 = Very Cold; 50 = Not particuarly Warm or Cold;
       100 = Very Warm; Chicago Council Survey 2016",
             x = "American Feelings",
             y = "Count") +
        theme_bw()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
