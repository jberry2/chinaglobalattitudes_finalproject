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
library(prettyR)
library(rstanarm)
library(leaflet)
library(gganimate)
library(markdown)

anes <- readRDS("chinese_military_threat.rds")
ccouncil <- 
    load("Chicago_Council/ICPSR_36806/DS0001/36806-0001-Data.rda")

pew <- readRDS("pew.RDS")

basic_model <- readRDS("basic_model.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "The Impact on Global Trade Flows on International Public Opinion
    towards China",
    theme = shinytheme("simplex"),
    tabPanel("Exploring Public Opinion Towards China",
             fluidPage(
                 titlePanel("American Perspectives on China"),
                 h3("Background and Motivations"),
                 p("Much has been said in the academic literature about 
                   'China shock,' or the impact of rising Chinese exports to on 
                   other developed economies after China's accession to the 
                   World Trade Organization in 2001. Academics have theorized 
                   that the rise of Chinese trade has lead to a number of 
                   pernicious effects on developed democracies from claims that 
                   Chinese trade has depressed manufacturing employment totals 
                   in Western countries by upwards of 15%, that Chinese trade 
                   has accelerated global political polarization, to falling 
                   international trust in the democratic process. Yet, little 
                   research has been conducted on how the 'China shock' effect 
                   has affected global public opinion towards China itself, the 
                   country behind billions of dollars in annual trade flows. 
                   Utilizing regression analysis and predictive modeling to 
                   look at public opinion among G20 countries, this project 
                   attempts to analyze how international trade flows and 
                   macro-economic data can potentially predict attitudes towards
                   China on the individual, micro level."),
                 br(),
                 br(),
                 
                 
                 fluidRow(
                   column(5, 
                          h3("Global Attitudes Towards China"),
                          p("	The People’s Republic of China was established on October 1, 
               1949. Since then, the Chinese nation has seen a remarkable rise 
               in prosperity, growing from a small agricultural country, torn 
               increasingly apart by years of war, to a country today that 
               boasts the world’s second largest economy and is home to over 
               1.4 billion people. Yet, since opening itself to the global 
               economy under the leadership of Deng Xiaoping during the 
               “reform and openness” (gaige kaifang) period of the 1980s, the
               Chinese state has put most of its emphasis on developing 
               traditional elements of hard power, including investments in the
               Chinese economy and in the People’s Liberation Army. Under the
               leadership of Xi Jinping, however, the Chinese Communist Party 
               (CCP) has begun to shift its emphasis towards the development of 
               “soft power” as a technique to increase Chinese standing on the 
               international stage."),
                          
                          
                          
                          sliderInput("dates", "Choose a Range of Dates",
                                      min = as.Date("2009-01-11", "%Y-%m-%d"),
                                      max = as.Date("2019-05-05", "%Y-%m-%d"), 
                                      value = c(as.Date("2009-01-11", timeFormat = "%Y-%m-%d"),
                                                as.Date("2019-05-05", "%Y-%m-%d")))),
                   
                   
                   column(7, 
                          plotOutput("plot_of_opinion_over_time"))
                 ),
                 br(),
                 br(),
                 fluidRow(
                   column(7, 
                         plotOutput("plot_of_country_opinion_over_time")),
                   column(5, 
                          h3("What countries are included in this research?"),
                          p("This research includes data for 17 of the 20 states that compose the 
                            Group of 20 (G20). These countries were selected in my research because they,
                            respectively, make up the world's 20 largest economies. Size of economy is
                            one of the few variables that create similarities between these entities though,
                            as political systems, geography, and economic specialization differ widely between
                            all of these countries. Excluded from my analysis are the member-states of
                            Saudi Arabia, China, and the European Union. China was excluded because I am
                            not interested in collecting Chinese opinion on international issues. Saudia 
                            Arabia and the European Union were excluded because these two entitites are
                            not included in Pew Research's annual Global Attitudes & Trends survey."),
                          
                          # This is a drop down menu that allows users to choose their country
                          
                          selectInput("COUNTRY", "Country", c("Argentina", "Australia", "Brazil", "Canada", "France",
                                                              "Germany", "India", "Indonesia", "Italy", "Japan",
                                                              "Mexico", "Russia", "South Africa", "South Korea",
                                                              "Turkey", "United Kingdom", "United States")))
                 ),
                 br(),
                 br(),
                 
                 fluidRow(
                   
                   column(4, 
                          h3("Trade Flows of Selected G20 Countries"),
                          p("Economies come in all sizes. While G20 members account for 85 percent
                            of the world economy, 75 percent of global trade, and two-thirds of the world's population,
                            G20 members are also home to more than half of the world's poor. Within the G20 itself,
                            however, there is much discprency in the size of each members' economy and also
                            the degree to which each economy interacts with China.")),
                   column(8, 
                          br(),
                          
                          # This is the interactive plot that has size of each economy over time.
                          
                          plotOutput("plot1"))
                 ),
                 br(),
                 br(),
                 
                 
                 # This last part of the page has an interactive map showing trade flow proportion
                 
                 h3("What is the proportion of GDP  each country traded with China in 2019?"),
                 p("Each G20 member state has traded extensively with China over the last decade.
                   Explore the map below to see where trade is coming from and going, as well as the 
                   level of interdependence each economy has with China."),
                 
                 # Have to use leafletOutput here
                 
                 leafletOutput("map")
                 
             )),
    tabPanel("Modeling Global Attitudes",
             fluidPage(
               titlePanel("Building a Model to Predict the Effect of Trade on Global Attitudes Towards China"),
               
               h2("The Question: Does China Shock Lead to Global Antipathy Towards China?"),
               p("It is undisputed that the entry of China into the G20 in 2001 has had an outsized
               effect on world affairs. Understanding the impact that China's economic rise has had 
                            on people's feelings towards China is an unressolved question though.The following
                 page considers several different models for predicting the change in global atittudes towards China 
                 between the years of 2009 to 2019"),
               br(),
               br(),
               
               fluidRow(
                 column(5, 
                        h3("Focusing on Trade-to-GDP Ratios"),
                        p("The trade-toGDP ratio is an indicator of the relative importance of
                          international trade in the economy of a country. It is calculated by dividing the
                          aggregate value of imports amd exports over a period of time by the 
                          gross domestic product for the same period. The trade-to-GDP ratio is often 
                          used a measurement of the openess of a country to international trade and only can
                          also be interpreted as an indicator of the degree of globalization of an economy. 
                          By limiting by trade-to-GDP ratio
                          to focus only on a G20 member's trade with China, this can be used as a proxy
                          for international interaction with China. Larger economies (such as the USA) that trade 
                          heavily with China are shielded from many effects of trade due to the overall size of 
                          their economies. Countries that appear to have low absolute values of trade with China
                          might actually have a significant chunk of their overall economic output originating
                          from or heading towards China.")),
                 column(7, 
                        br(), 
                        
                        # And here's the plot, created in the server
                        
                        plotOutput("basic_plot"))),
               br(),
               br(),
               
               # This next section explains my three original models, along with
               # a table that shows the variables in each
               
               fluidRow(
                 column(9, 
                        tableOutput("var_table")),
                 column(3, 
                        h3("Model Building Process"), 
                        p("Initially, I constructed three different models to predict attitudes towards China
                                                                      for G20 member states with three levels
                                                                      of complexity. The simplest model used only one continuous 
                                                                      variable,  import flow from China as a proportion of GDP. The medium model incorporated
                                                                      a factor variable for people's viws on the economic situation of their country, based on my hypothesis
                                                                      that people less satisfied with their country's economy will assign more blame towards China. The third and most complex model considered several
                                                                      additional variables that I thought could also affect attitudes towards China; include people's ratings of their satisfaction with
                                                                      their country, people's beliefs in democracy, people's beliefs in whether their children will have a better or
                                                                      worse life, and whether people use the interet and social media. I also created two alternative simple models that looked
                          at the effects of exports to China and also total trade flow."))
                 
               ),
               br(),
               br(),
               
               fluidRow(
                 
                 # This row explains how I selected the models and shows a corresponding table
                 
                 column(8, 
                        h3("Model Outputs"),
                        p("This section shows model output for the five models graphed. Pick and choose using the selection
                          criteria below to choose a table to view. Each table continues regression output for the selected
                          model."),
                        br(),
                        ),
                 column(4, 
                        tableOutput("models_table"))),
               br(),
               br(),
               
               fluidRow(
                 
                 # This row explains how I selected the models and shows a corresponding table
                 
                 column(8, 
                        h3("Model Selection"),
                        p("As shown, the medium complexity model had the smallest RMSE,
                                     in cross validation, but it was not significantly different 
                                     enough from the simple model that would indicate the extra 
                                     complexity was worthwhile."),
                        br(),
                        p("Why might these more complex models have been less effective at predicting
                                     lost revenue? I think some of it may have had to do with the high variation
                                     in factors unexplained by any of the models; namely, the indicator variable here of. Survey participants were asked to 'Estimate 
                                     how much your organization's revenue  has decreased as a result of the coronavirus?', but
                                     it is possible that some people may have either provided an incorrect estimate or
                                     inadvertently included ambiguous events (i.e. do you count a concert that was scheduled
                                     for the future but will likely be canceled as lost revenue?). Furthermore, while I limited
                                     the survey response dates for this analysis from March 13th to June 1st in order to decrease
                                     the impact of time as a variable, one would expect an overall increase in lost revenue over even
                                     this short span of time (ultimately, I chose not to include time in the model due to the high 
                                     variation in number of responses per day). Because the number of lost attendees naturally
                                     matches with the span of time/method by which individual organizations calculated their lost revenue,
                                     it makes sense that it works effectively as a predictor on its own.")),
                 column(4, 
                        tableOutput("models_table"))),
               br(),
               br(),
               
               # This row has a table and interpretation for my final model
               
               fluidRow(
                 column(7, 
                        h3("Interpreting the Final Model"),
                        p("The median of the posterior distribution of estimated revenue 
                                     loss for an organization with 0 lost attendees is $29,220.80, 
                                     suggesting that the pandemic has had a severe economic impact 
                                     on non-presenter arts organizations. For every additional lost 
                                     attendee, the predicted revenue loss to small arts organizations 
                                     is $1.45 (95% confidence interval: $1.24 to $1.65). The median 
                                     of the posterior distribution for sigma, the true standard 
                                     deviation of the lost revenue of small arts organizations, is 
                                     $31,490, suggesting that there is great variation, with some 
                                     organizations losing next to nothing and others losing over 50% 
                                     of their annual budget (note that this only takes into account the March-May timeframe).")),
                 column(4, 
                        offset = 1, 
                        tableOutput("final_mod"))),
               br(),
               br(),
               
               # This row has my lovely histogram that predicts lost 
               # revenue for individual and average organizations based 
               # on lost attendees
               
               fluidRow(
                 column(8, 
                        plotOutput("pred_plot")),
                 column(4, 
                        h3("Predicting Revenue Loss"),
                        p("Use the slider below to make predictions about how much
                                     an organization of a budget between $100,000 and $249,000 
                                     with a certain number of lost attendees may have lost in revenue
                                     during the first few months of the pandemic."),
                        
                        # This slider allows the user to choose the number
                        # of lost attendees. It starts at 0 and
                        # the user can choose from there in increments
                        # of 200
                        
                        sliderInput("attendees", "Number of Attendees",
                                    min = 0, max = 40000,
                                    value = 0, step = 200))))
               
               
                 
               ),
    tabPanel("American Perspectives on China",
             fluidPage(
               titlePanel("Model"),
               mainPanel(
                 plotOutput("basic_plot")
               ))),
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

output$distPlot4 <- renderPlot({
    fit_obj <- stan_glm(Q45_6 ~ Q1025 -1,
                        data = da36806.0001,
                        refresh = 0)
    
fit_obj %>% 
  as_tibble() %>% 
  select(-sigma) %>% 
  mutate(Democrat = `Q1025(2) Democratic`, Republican = `Q1025(1) Republican`,
    Neither = `Q1025(3) Neither`) %>%
  pivot_longer(cols = c(`Q1025(2) Democratic`,`Q1025(1) Republican`,
                         `Q1025(3) Neither`),
                 names_to = "parameter",
                 values_to = "Attitude") %>% 
  ggplot(aes(x = Attitude, color = parameter)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                      alpha = 0.5, 
                      bins = 100, 
                      position = "identity") +
  scale_color_manual(name = "Party Affiliation",
                     labels = c("Republican", "Democrat", "Independent"),
                      values = c("firebrick1", "dodgerblue", "ivory4")) +
  labs(title = "Posterior Probability Distribution",
      subtitle = "Average attitude toward China; Chicago Council Survey 2016",
        x = "Attitude",
        y = "Probability") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  geom_vline(xintercept = 50, linetype = 'dashed')
})

output$basic_plot <- renderPlot({
  basic_model %>% 
  as_tibble() %>% 
    ggplot(aes(x = `(Intercept)`)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   fill = "lightblue4", 
                   color = "gray97",
                   bins = 100) +
    labs(title = "Posterior Probability Distribution of Global Attitudes
        Towards China",
        subtitle = "The Effect of Total Trade Flow on Favorability 
             Towards China (1 = Very unfavorable, 4 = Very favorable)",
         x = "Attitudes Towards China",
         y = "Probability") +
    theme_classic()
 })

# output$myTable <- renderTable({ 
#  basic_model <- stan_glm(data = pew,
 #                         formula = fav_china_scale ~ trade_flow_prop,
 #                         refresh = 0)
  
#  print(basic_model, digits = 4)
  
 # tbl_regression(basic_model, intercept = TRUE) %>%
 #   as_gt() %>%
  #  tab_header(title = "Regression of Global Attitudes Torwards China",
  #             subtitle = "The Effect of Total Trade Flow on Favorability 
  #           Towards China (1 = Very unfavorable, 4 = Very favorable)") %>%
 #   tab_source_note(md("Pew Global Attidues & Trends Survey (2009-2019), 
     #                IMF World Trade Flows (2009-2019),
    #                 World Bank G20 GDPs (2019-2019)"))}
# )



}

# Run the application 
shinyApp(ui = ui, server = server)
