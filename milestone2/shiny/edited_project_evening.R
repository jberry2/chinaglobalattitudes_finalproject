#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# loading libraries

library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(gtsummary)
library(broom.mixed)

# loading RDS and objects

anes <- readRDS("chinese_military_threat.rds")

pew <- readRDS("pew.RDS")

basic_export_model <- readRDS("basic_export_model.RDS")

basic_import_model <- readRDS("basic_import_model.RDS")

basic_model <- readRDS("basic_model.RDS")

complex_export_model <- readRDS("complex_export_model.RDS")

medium_export_model <- readRDS("medium_export_model.RDS")

simple_export_demographic_model <- readRDS("simple_export_demographic_model.RDS")

model_variables <- readRDS("model_variables.RDS")

america_model <- readRDS("america_model.RDS")

load("36806-0001-Data.rda")


ui <- navbarPage(
  
  # This panel gives a general overview of my project. It is a splash page.
  
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
               
               
               column(7),
               plotOutput("country_facet_plot")),
             br(),
             br(),
             br(),
             br(),
             br(),
             
             fluidRow(
               column(7, plotOutput("country_opinion")),
               column(5, h3("What countries are included in this research?"),
                      p("This research includes data for 17 of the 20 states that compose the 
     Group of 20 (G20). These countries were selected in my research because 
     they, respectively, make up the world's 20 largest economies. Size of 
     economy is one of the few variables that create similarities between these
     entities though, as political systems, geography, and economic 
     specialization differ widely between all of these countries. Excluded from
     my analysis are the member-states of Saudi Arabia, China, and the European 
     Union. China was excluded because I am not interested in collecting Chinese
     opinion on international issues. Saudia Arabia and the European Union were 
     excluded because these two entitites are not included in Pew Research's 
     annual Global Attitudes & Trends survey."),
                      
                      # This is a drop down menu that allows users to choose their country
                      
                      selectInput("COUNTRY", "Country", c("Argentina", "Australia", "Brazil",
                                                          "Canada", "France","Germany", "India", "Indonesia", "Italy",
                                                          "Japan", "Mexico", "Russia", "South Africa", "South Korea",
                                                          "Turkey", "United Kingdom", "United States")))
             ),
             br(),
             br(),
             
             fluidRow(
               
               column(4, 
                      h3("Trade Flows of Selected G20 Countries"),
                      p("Economies come in all sizes. While G20 members account for 85 percent
    of the world economy, 75 percent of global trade, and two-thirds of the 
    world's population, G20 members are also home to more than half of the 
    world's poor. Within the G20 itself, however, there is much discprency in 
    the size of each members' economy and also the degree to which each economy
    interacts with China.")),
               column(8, 
                      br(),
                      
                      # This is the interactive plot that has size of each economy over time.
                      
                      tabsetPanel(
                        tabPanel("GDP Growth by Year", 
                                 plotOutput("gdp_billions")), 
                        tabPanel("Chinese Exports to _____ Country, by Year", 
                                 plotOutput("export_billions")), 
                        tabPanel("Chinese Imports from _____ Country, by Year", 
                                 plotOutput("import_billions")))
               )
             ),
             br(),
             br(),
             
             
             # This last part of the page has a  map showing trade flow proportion. Created
             # seperately.
             
             h3("What is the proportion of GDP  each country traded with China in 2019?"),
             p("Each G20 member state has traded extensively with China over the last 
   decade. Explore the map below to see where trade is coming from and going,
   as well as the level of interdependence each economy has with China."),
             HTML('<center><img src="map_trade_crop.png" width="1400"></center>'))),
  
  tabPanel("American Perspectives on China",
           
           fluidRow(
             column(4, 
                    h3("To be written later"),
                    p("The trade-to-GDP ratio is an indicator of the relative importance of
    international trade in the economy of a country. It is calculated by
    dividing the aggregate value of imports amd exports over a period of time by
    the gross domestic product for the same period. The trade-to-GDP ratio is 
    often used a measurement of the openess of a country to international trade 
    and only can also be interpreted as an indicator of the degree of
    globalization of an economy. By limiting by trade-to-GDP ratio
    to focus only on a G20 member's trade with China, this can be used as a 
    proxy for international interaction with China. Larger economies (such as
    the USA) that trade heavily with China are shielded from many effects of
    trade due to the overall size of their economies. Countries that appear to 
    have low absolute values of trade with China might actually have a 
    significant chunk of their overall economic output originating
    from or heading towards China.")),
             
             # I saved the animation as an html, so used 
             # htmlOutput to get it
             
             column(8,
                    htmlOutput("usa_animation")),
           ),
           
           br(),
           br(),
           
           # This text explains the graphs below
           
           h3("American Attitudes Towards China"),
           p("American views towards China continue to trend negative. According to
       polling from both Pew and the Chicago council, negative views have
       continued to climb since Donald Trump took office. This public opinion
       trend holds importance to how elite level politicians navigate
       increasingly tense relations between the world's two largest 
       economies."),
           
           br(),
           br(),
           
           fluidRow(
             column(8, 
                    
                    selectInput("rd","Select a visualization to view", 
                                choices = c("American Feeling Thermometer (2016)",
                                            "Attitudes by Party",
                                            "Posterior Probability Distribution by Party",
                                            "Views on the Chinese Military"),
                                selected = "American Feeling Thermometer (2016)"),
                    uiOutput('usa_choose_plot'))),
           
           br(),
           br(),
           br(),
           br()),
  
  tabPanel("Modeling Global Attitudes",
           fluidPage(
             titlePanel("Building a Model to Predict the Effect of Trade on Global Attitudes Towards China"),
             
             # This panel explains my models.
             
             h4("The Question: Does China Shock Lead to Global Antipathy Towards China?"),
             p("It is undisputed that the entry of China into the G20 in 2001 has had an outsized
  effect on world affairs. Understanding the impact that China's economic rise has had 
  on people's feelings towards China is an unressolved question though.The following
  page considers several different models for predicting the change in global atittudes towards China 
  between the years of 2009 to 2019"),
             br(),
             br(),
             fluidRow(
               column(5, 
                      br(),
                      br(),
                      h3("Focusing on Trade-to-GDP Ratios"),
                      br(),
                      p("The trade-to-GDP ratio is an indicator of the relative importance of
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
                      plotOutput("basic_plot"),
                      br(), 
               ),
               br(),
               br(),
               
               # This next section explains my original models, along with
               # a table that shows the variables in each. I created it in
               # my gather rmd.
               
               fluidRow(
                 column(9,
                        tableOutput("variable_table")),
                 column(3, 
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h3("Choosing Models"), 
                        p("Initially, I constructed three different models to predict attitudes towards China
      for G20 member states with three levels
      of complexity. The simplest model used only one continuous 
      variable,  import flow from China as a proportion of GDP. The medium model incorporated
      a factor variable for people's viws on the economic situation of their country, based on my hypothesis
      that people less satisfied with their country's economy will assign more blame towards China. The third and most complex model considered several
      additional variables that I thought could also affect attitudes towards China; include people's ratings of their satisfaction with
      their country, people's beliefs in democracy, people's beliefs in whether their children will have a better or
      worse life, and whether people use the interet and social media. I also created two alternative simple models that looked
      at the effects of exports to China and also total trade flow. Lastly, I also replicated my original basic model of Chinese import flow
      but added demographic conditions such as age and gender to see if these had an effect on predictive power."))
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
                        br()
                 ),
                 column(8, 
                        tabsetPanel(
                          tabPanel("Simple Imports from China Model", 
                                   gt_output("simple_export")), 
                          tabPanel("Simple Imports from China Model with included Demographics", 
                                   gt_output("demographic_export")),
                          tabPanel("Simple Chinese Total Trade Model", 
                                   gt_output("simple_trade")), 
                          tabPanel("Simple Exports to China Model", 
                                   gt_output("simple_import")),
                          tabPanel("Medium Imports from China Model", 
                                   gt_output("medium_export")), 
                          tabPanel("Complex Imports from China Model", 
                                   gt_output("complex_export")))  
                 )),
               br(),
               br(),
               
               fluidRow(
                 
                 # This row explains how I selected the models and shows a corresponding table
                 
                 column(8, 
                        h3("Model Selection"),
                        p("As shown, each model has very similar standard error scores. This
            is likely, in part, because of how large my created Pew dataset
            is. Choosing a model is therefore more of a matter of preference. 
            I choose the simple imports model as it is the most intuitive to
            understand and its simplicitly allows for easy statistical
            interpretation."),
                        br(),
                        p("Why might these more complex models not be more effective at predicting
    global attitudes than the simple model? I think some of it may have to do
    with the size of my overall dataset and how disconnected some of these 
    variables might be to trade. In other words, while it may matter for some
    countries whether you are a man or a woman (because of factory work and 
    other forms of employment that have recently flowed to China), this 
    potential difference is washed away by the diversity of people and 
    atittudes in the survey, though.")),
                 column(2,
                        gt_output("metrics_table")
                 ),
                 br(),
                 br(),
                 
                 # This row has a table and interpretation for my final model
                 
                 fluidRow(
                   column(8, 
                          h3("Interpreting the Final Model"),
                          p("The median of the linear regression model of predicted attitudes 
       towards China on a 1-4 scale is 2.419, 
       suggesting that G20 citizens  have an overall slight positive outlook
       about China. For every additional percentage point in Chinese arriving
       import value-to-GDP though, the predicted attitude change
       is -.037 (95% confidence interval: -.0415 to -.0325). The median 
       of the posterior distribution for sigma, the true standard 
       deviation of change in atittudes, is 
       .8967, suggesting that there is a good deal of variation in attitudes
      among the G20 countries. This makes sense when plotting the linear
      regressions of attitude change for each country as seen below.",
                            align = "center")),
                   column(8, 
                          offset = 1,
                          tabsetPanel(
                            tabPanel("Linear trend of aggregated G20 population's 
                     attitude towards China", 
                                     plotOutput("trend_G20")),
                            tabPanel("Linear trend of attitude towards China
                                     by country", 
                                     plotOutput("trend_country")))
                   )),
                 br(),
                 br())))),
  
  # I had to comment out my big live model so the projecty
  # could deploy online. I will still show it live tomorrow though.
  
  #  fluidRow(
  #      column(8,
  #     plotOutput("basic_model_posterior")
  #     ),
  #    column(4, 
  #    h3("Predicting Median G20 Public Opinion towards
  #        China based on Chinese Imports-to-GDP Ratios
  #         (Chinese Import Dependence)"),
  #       p("Use the slider below to make predictions about how much
  #        global attitudes will change towards China with increased
  #         levels of Chinese goods exported to their home country.
  #        0 = 0% import-to-GDP ratio; 100 = 100% import-to-GDP ratio"),
  
  # This slider allows the user to choose the number
  # of lost attendees. It starts at 0 and
  # the user can choose from there in increments
  # of 200
  
  #sliderInput("user_percentage", "Percentage Dependence on Importing Chinese Goods
  #                                      as Ratio of GDP",
  #                                   min = 0, max = 100,
  #                                    value = 0, step = 1))))))),
  tabPanel("About", 
           
           # an about page with info about myself
           
           h2("About this project"),
           br(),
           h4("Project Background and Motivations"),
           p("For this project I intend to study American attitudes
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
               one component of my Senior Thesis, required for graduation."),
           p("The data on public opinion  came from", 
             a("The Pew Research Center's Global Attitudes and Trends polling series.", 
               href = "https://www.pewresearch.org/global/"),
             "The data on trade flows sectors was reported from the International 
        Monetary Fund and can be accessed", 
             a("on their website.", href = "https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85"),
             "The data on GDP for each country was reported from the Organization
         for Economic Co-operation and Development and can be accessed", 
             a("on their website here.", href = "https://data.oecd.org/gdp/gross-domestic-product-gdp.htm")),
           p("For the code used to create this project, check out my", 
             a("Github Repository.", 
               href = "https://github.com/jberry2/milestones_final_project")),
           br(),
           h4("Acknowlegements"),
           p("First and foremost, I would like to thank my TF Mitchell who was a great
    teacher to have during this difficult semester. I would also like to
      thank the TF Wyatt Hurt for his assistance during the last build of
      this project as well as my friends PK Kumar and Ryan Zhang who
      often times provided mental and logistical support when completing
      this course's problem sets and tutorials."), 
           h2("About Me"),
           br(),
           p("My name is Joshua Berry and I study political science at
       Harvard College. I'm interested in all things international relations,
       the outdoors, and sports. Although I try my best to go hiking and biking,
       you can often find me on Sundays watching the NY Giants playing a 
       bad to mediocore football game.
       You can reach me at jberry@college.harvard.edu."))
  
)



server <- function(input, output) {
  
  output$gdp_billions <- renderPlot({
    
    #a simple plot that shows linear trend over time. I replicated an example
    # from pset 3
    pew %>%
      ggplot(aes(x = year, y = gdp_billions)) +
      geom_line(color = "blue") +
      facet_wrap(~COUNTRY) +
      labs(title = "GDP of Country by Year",
           subtitle = "The Effect of Total Trade Flow on Favorability 
             Towards China (1 = Very unfavorable, 4 = Very favorable)",
           x = "Year",
           y = "GDP in Billions of US Dollars") +
      theme_classic()
  })
  
  output$export_billions <- renderPlot({
    
    # same principle as the previous plot
    
    pew %>%
      ggplot(aes(x = year, y = gdp_billions)) +
      geom_line(color = "blue") +
      facet_wrap(~COUNTRY) +
      labs(title = "GDP of Country by Year",
           subtitle = "The Effect of Total Trade Flow on Favorability 
             Towards China (1 = Very unfavorable, 4 = Very favorable)",
           x = "Year",
           y = "GDP in Billions of US Dollars") +
      theme_classic()
  })
  
  output$import_billions <- renderPlot({
    
    # same principles as previous two plots
    
    pew %>%
      ggplot(aes(x = year, y = china_imports_billions)) +
      geom_line(color = "green") +
      facet_wrap(~COUNTRY) +
      labs(title = "Chinese Imports by Year",
           subtitle = "The amount of Imports that China has recieved from other 
    G20 members has varied over time",
           x = "Year",
           y = "Import Value in Billions of US Dollars") +
      theme_classic()
  })
  
  
  output$variable_table <- render_gt({
    
    # originally I did not have render_gt which lead to some issues with opening 
    # my shiny
    
    model_variables %>%
      gt() %>%
      tab_header("Explanation of Variables Included in Different Models")
  })
  
  output$simple_export <- render_gt({
    
    # this table served as the base for the rest of my tables below. I followed
    # the example from the table we created in pset 8.
    tbl_regression(basic_export_model, intercept = TRUE,
                   estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
      as_gt() %>%
      tab_header(title = "Simple Imports Regression of Global Attitudes Torwards
             China", subtitle = "The Predicted Effect of Imports from China 
             Dependence on Favorability Towards China (1 = Very unfavorable,
             4 = Very favorable)") %>%
      tab_source_note(md("Pew Global Attidues & Trends Survey (2009-2019), 
                     IMF World Trade Flows (2009-2019),
                     World Bank G20 GDPs (2019-2019)"))
    
  })
  
  output$simple_trade <- render_gt({
    
    # same description as above
    
    tbl_regression(basic_model, intercept = TRUE,
                   estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
      as_gt() %>%
      tab_header(title = "Simple Total Trade Regression of Global Attitudes 
               Torwards China", 
                 subtitle = "The Predicted Effect of Chinese Total Trade Flow 
               Dependence on Favorability Towards China (1 = Very unfavorable,
               4 = Very favorable)") %>%
      tab_source_note(md("Pew Global Attidues & Trends Survey (2009-2019), 
                     IMF World Trade Flows (2009-2019),
                     World Bank G20 GDPs (2019-2019)"))
  })
  
  output$simple_import <- render_gt({
    
    # same description as above
    
    tbl_regression(basic_import_model, intercept = TRUE,
                   estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
      as_gt() %>%
      tab_header(title = "Simple Exports Regression of Global Attitudes Torwards 
               China",
                 subtitle = "The Predicted Effect of Exports to China Dependence 
               on Favorability Towards China (1 = Very unfavorable,
               4 = Very favorable)") %>%
      tab_source_note(md("Pew Global Attitudes & Trends Survey (2009-2019), 
                     IMF World Trade Flows (2009-2019),
                     World Bank G20 GDPs (2019-2019)"))
  })
  
  output$medium_export <- render_gt({
    
    # same description as above
    
    tbl_regression(medium_export_model, intercept = TRUE,
                   estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
      as_gt() %>%
      tab_header(title = "Medium Regression of Global Attitudes Torwards China",
                 subtitle = "The Predicted Effect of Imports from China Dependence
               on Favorability Towards China (1 = Very unfavorable,
               4 = Very favorable)") %>%
      tab_source_note(md("Pew Global Attidues & Trends Survey (2009-2019), 
                     IMF World Trade Flows (2009-2019),
                     World Bank G20 GDPs (2019-2019)"))
  })
  
  output$complex_export <- render_gt({
    
    # same description as above
    
    tbl_regression(complex_export_model, intercept = TRUE,
                   estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
      as_gt() %>%
      tab_header(title = "Complex Regression of Global Attitudes Torwards China",
                 subtitle = "The Predicted Effect of Imports from China Dependence
               on Favorability Towards China (1 = Very unfavorable,
               4 = Very favorable)") %>%
      tab_source_note(md("Pew Global Attidues & Trends Survey (2009-2019), 
                     IMF World Trade Flows (2009-2019),
                     World Bank G20 GDPs (2019-2019)"))
  })
  
  output$demographic_export <- render_gt({
    
    # same description as above although now the tables get a bit longer and 
    # more complicated
    
    tbl_regression(simple_export_demographic_model, intercept = TRUE,
                   estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
      as_gt() %>%
      tab_header(title = "Simple Regression of Global Attitudes
               (with Demographics) Torwards China",
                 subtitle = "The Predicted Effect of Imports from China Dependence
               on Favorability Towards China (1 = Very unfavorable, 
               4 = Very favorable)") %>%
      tab_source_note(md("Pew Global Attidues & Trends Survey (2009-2019), 
                     IMF World Trade Flows (2009-2019),
                     World Bank G20 GDPs (2019-2019)"))
  })
  
  output$metrics_table <- render_gt({
    
    # this is a self created table, inserting in standard error. I tried to
    # get RMSE but because each regression is a different length from the total
    # Pew dataset due to many missing data occurrences and questions not
    # being asked in certain years, I was unable to find rmse either by hand
    # or by the metrics function. Therefore, I went with an option that, although
    # not optimal, gets the job done.
    
    tibble("Model Name" = c("Simple Model", "Simple Demographics Model",
                            "Medium Model", "Complex Model", 
                            "Simple Exports to China Model", 
                            "Simple Total Trade Model"),
           "Standard Error" = c(.8967, .8930, .8935, .8873, .8975, .8975)) %>%
      gt() %>%
      tab_header("Comparing Basic Metrics")
  })
  
  output$country_opinion <- renderPlot({
    
    # This plot is based off work from exam 3. I made light edits and added
    # an input so people could select their country to display.
    
    pew %>%
      group_by(COUNTRY, year) %>%
      summarize(country_fav_prop = sum(fav_china_logistic, na.rm = TRUE)/n(),
                country_unfav_prop = sum(fav_china_logistic == 0, na.rm = TRUE)/n()) %>%
      filter(COUNTRY == input$COUNTRY) %>%
      ggplot(aes(x = year)) +
      geom_line(aes(y = country_fav_prop, color = "steelblue")) +
      geom_line(aes(y = country_unfav_prop, color = "darkred")) +
      theme(axis.text = element_text(size = 5),
            axis.text.x = element_text(angle = 45),
            strip.text = element_text(size = 7),
            panel.grid = element_blank(), 
            panel.spacing.x = unit(3, "mm"),
            axis.ticks = element_blank()) +
      scale_x_continuous(breaks = c(2009:2019),
                         labels = c("2009", "", "", "", "", "", "", "", "", "", "2019")) +
      labs(title = "Evaluations of China across ____, a G20 economy",
           subtitle = "% who have a(n) view of China",
           x = "Year",
           y = "Public Opinion") +
      scale_color_discrete(name = "Favorable/Unfavorable", 
                           labels = c("Unfavorable", "Favorable")) +
      theme_linedraw()
  })
  
  output$distPlot2 <- renderPlot({
    
    # this plot isn't used and is a base for selecting the plot to show
    # on panel 2.
    
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
  
  output$trend_country <- renderPlot({
    
    # this plot isn't pretty but shows the clear negative linear regression trend.
    
    pew %>%
      ggplot(aes(y = fav_china_scale, x = china_export_prop)) +
      geom_point(alpha = 100, color = "gray") +
      geom_smooth(method = "glm", formula = y~x) +
      facet_wrap(~ COUNTRY) +
      labs(title = "Trends Differ According to Country",
           x = "Ratio of Chinese Import Value to GDP (in USD)",
           y = "Attitudes towards China") +
      theme_bw()
  })
  
  output$trend_G20 <- renderPlot({
    
    # same principle as above
    
    
    pew %>%
      ggplot(aes(y = fav_china_scale, x = china_export_prop)) +
      geom_point(alpha = 100, color = "lavender") +
      geom_smooth(method = "glm", formula = y~x) +
      labs(title = "Yet among the aggregated G20 itself, viwes shift negative",
           x = "Ratio of Chinese Import Value to GDP (in USD)",
           y = "Attitudes towards China") +
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
  
  # output$basic_model_posterior <- renderPlot({
  #  new_obs <- tibble(china_export_prop = input$user_percentage)
  # 
  #  posterior_predict(basic_export_model, newdata = new_obs) %>%
  #   as_tibble() %>%
  #   mutate(across(everything(), as.numeric)) %>%
  #   ggplot(aes(x = `1`)) +
  #  geom_histogram(aes(y = after_stat(count/sum(count))),
  #                 bins = 150, color = "white", fill = "blue")  +
  #  labs(title = "Predictive Posterior Probability Distribution",
  #      subtitle = "For a __% change in Chinese Imports by X Country as 
  #       percentage of GDP",
  #      x = "Predicted G20 Median Attitude on Favorability Towards China 
  #     (1 = Very unfavorable, 4 = Very favorable)",
  #      y = "Probability") +
  #  scale_y_continuous(labels = scales::percent_format()) +
  # theme_classic()
  # })
  
  output$distPlot4 <- renderPlot({
    
    # this plot is a base for the one used below with selection criteria
    
    america_model %>% 
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
    
    # this plot shows a basic posterior probability distribution from the chosen
    # model I adopted
    
    basic_export_model %>% 
      as_tibble() %>% 
      ggplot(aes(x = `(Intercept)`)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     fill = "lightblue4", 
                     color = "gray97",
                     bins = 100) +
      labs(title = "Posterior Probability Distribution of Global Attitudes 
         Towards China",
           subtitle = "Predicted Effect of a 1% increase in Chinese Imports to 
         GDP ration
          on Favorability Towards China (1 = Very unfavorable, 
         4 = Very favorable)",
           x = "Predicted G20 Attitude Towards China",
           y = "Probability") +
      theme_classic()
  })
  
  output$usa_animation <- renderUI({
    
    # animation creation code created in a different rmd.
    
    includeHTML("animation.html")
  })
  
  
  
  output$country_facet_plot <- renderPlot({
    
    # based on a plot from chapter 3
    
    pew %>%
      group_by(COUNTRY, year) %>%
      summarize(country_fav_prop = sum(fav_china_logistic, na.rm = TRUE)/n(),
                country_unfav_prop = sum(fav_china_logistic == 0, na.rm = TRUE)/n()) %>%
      ggplot(aes(x = year)) +
      geom_line(aes(y = country_fav_prop, color = "steelblue")) +
      geom_line(aes(y = country_unfav_prop, color = "darkred")) +
      facet_wrap(~ COUNTRY) +
      theme(axis.text = element_text(size = 5),
            axis.text.x = element_text(angle = 45),
            strip.text = element_text(size = 7),
            panel.grid = element_blank(), 
            panel.spacing.x = unit(3, "mm"),
            axis.ticks = element_blank()) +
      scale_x_continuous(breaks = c(2009:2019),
                         labels = c("2009", "", "", "", "", "", "", "", "", "", "2019")) +
      labs(title = "Increasingly negative evaluations of China across G20 economies",
           subtitle = "% who have a(n) view of China",
           x = "Year",
           y = "Public Opinion") +
      scale_color_discrete(name = "Favorable/Unfavorable",
                           labels = c("Unfavorable", "Favorable")) +
      theme_linedraw()
  })
  
  
  output$usa_choose_plot <- renderUI({
    if(input$rd=="American Feeling Thermometer (2016)"){
      
      # a plot that allows for human selection using input. Used if-else 
      # statements and also copied code that can be found above, simply intserting
      # this code into the right places and giving it the right names as well
      
      output$plot1<-renderPlot({
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
      plotOutput("plot1")
    }
    
    
    else if(input$rd=="Attitudes by Party"){
      output$plot2<-renderPlot({
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
      plotOutput("plot2")
    }
    
    
    else if(input$rd=="Posterior Probability Distribution by Party"){
      output$plot3<-renderPlot({
        america_model %>% 
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
      plotOutput("plot3")
    }
    
    else if(input$rd=="Views on the Chinese Military"){
      output$plot4<-renderPlot({
        anes %>%
          filter(china_mil %in% c(1, 2, 3)) %>%
          ggplot(aes(x = china_mil)) +
          geom_histogram(fill = "firebrick3", binwidth = 1, color = "white") +
          scale_x_discrete(limits = c("1", "2", "3"),
                           labels = c("Major threat", "Minor threat", "not a threat")) +
          labs(title = "Americans' Perspectives on the Chinese Military",
               subtitle = "Policy and attitudes toward China, ANES Survey 2012",
               x = "Chinese Military Threat",
               y = "Count") +
          theme_bw()
      })
      plotOutput("plot4")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
