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
                    br(), 
                    
                    # And here's the plot, created in the server
                    
                    tableOutput("basic_plot"))),
           br(),
           br(),
           
           # This next section explains my three original models, along with
           # a table that shows the variables in each
           
           fluidRow(
             column(9 
             ),
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
                    br()
             ),
             column(4 
             )),
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
             ),
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
                      offset = 1 
               ),
               br(),
               br(),
               
               # This row has my lovely histogram that predicts lost 
               # revenue for individual and average organizations based 
               # on lost attendees
               
               fluidRow(
                 column(8 
                 ),
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
             
             
             
           ))),

tabPanel("American Perspectives on China",
         
         fluidRow(
           column(4, 
                  h3("Understanding Sector-Wide Impact"),
                  p("To help put into perspective the widespread,
                                     national economic crisis that COVID-19 has caused 
                                     for arts organizations, take a look at unemployment rates.
                                     While unemployment rates for artists were lower than
                                     the general population at the beginning of 2020, they skyrocketed
                                     during the pandemic and have yet to come anywhere close to pre-pandemic
                                     levels. This suggests the need for special relief legislation target specifically
                                     at helping artists and arts organizations.")),
           
           # I saved the animation as an html, so used 
           # htmlOutput to get it
           
           column(8 
           ),
         ),
         
         # This row shows how cases didn't seem to have much of an
         # impact on financial wellbeing of arts organizations
         
         fluidRow(
           column(8, 
                  
                  # Within this tab, there's a baby tab that
                  # lets you choose which of the three graphs 
                  # you want to see
                  
                  tabsetPanel(
                  ))
         ),
         
         # This text explains that
         
         column(4, 
                h3("How did statewide case numbers affect arts organizations?"),
                p("Did arts organizations around the nation feel the effects
                                   of the pandemic similarly? The answer seems to be yes (at least
                                   for the March-May timeframe, which is what's shown in the plot).
                                   Despite some states such as New York, New Jersey, and Massachussets
                                   having much higher rates of COVID-19, arts organizations with
                                   budgets between $100,000 and $249,999 (the same sub-group predicted
                                   in the model) self-rated the economic damage done to their organizations
                                   at similar levels around the nation. This
                                   lack of an obvious relationship between statewide COVID-19 case rates and 
                                   economic impact on arts organizations, as shown by the nearly flat trendline,
                                   suggests that programs at the national level could be an effective
                                   way to address this widespread economic hardship."))),

tabPanel("Interpretation and Conclusion",
         fluidPage(
           titlePanel("So What?"),
           mainPanel(
             tableOutput("basic_plot")),
           
           fluidRow(
             column(5, 
                    h3("Beyond Numbers: Stories of the Pandemic's Impact"),
                    p("At the end of the survey, participants were asked an optional
                                       open response question: Is there anything else you would like to 
                                       share about the impact of COVID-19 on your organization? People
                                       responded to this prompt in a variety of ways. Some expressed 
                                       fear about the unknown nature of the pandemic and its impact on 
                                       the arts sector, or detailed losses in the forms of cancelled 
                                       performances, slashed income, and social isolation. Others 
                                       detailed the innovative ways their organization had adapted to
                                       provide services for their community and expressed hope that 
                                       the arts could serve as a unifying and comforting force for 
                                       a fractured society. In later months, many respondents expressed
                                       a sense of exhaustion at the difficult and often demoralizing work
                                       of being an artist or arts administrator in this time. Many responses
                                       touched on multiple of these themes."),
                    br(),
                    p("Working with large datasets can sometimes feel very distant from
                                         the individual lives and stories that make up your data. I hope that
                                         by exploring some of the most commonly used words in participants'
                                         responses to these questions and reading through a few of the actual
                                         responses(organized by theme), you can begin to get a sense of the 
                                         profound impact of COVID-19 on the individuals who help produce art
                                         in this country"))
             
           ))),

tabPanel("About", 
         
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
               one component of my Senior Thesis, required for graduation.
               Check out my:"),
         a("github repo here.", 
           href = "https://github.com/jberry2/milestones_final_project"),
         p("The data on public opinion  came from", 
           a("The Pew Research Center's Global Attitudes and Trends polling series.", 
             href = "https://www.pewresearch.org/global/"),
           "The data on trade flows sectors was reported from the International 
            Monetary Fund and can be accessed", 
           a("on their website.", href = "https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85"),
           "The data on GDP for each country was reported from the Organization
            for Ec0nomic Co-operation and Development and can be accessed", 
           a("on their website here.", href = "https://data.oecd.org/gdp/gross-domestic-product-gdp.htm")),
         p("For the code used to create this project, check out my", 
           a("Github Repository.", 
             href = "https://github.com/jberry2/milestones_final_project")),
         br(),
         h4("Acknowlegements"),
         p("First and foremost, I would like to thank my TF Mitchell, whose patience, kindness, and penchant
                            for R-related memes gave me the motivation I needed to complete this project."), 
         h2("About Me"),
         br(),
         p("My name is Joshua Berry and I study political science at
             Harvard College. 
             You can reach me at jberry@college.harvard.edu."))