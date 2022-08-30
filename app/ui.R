source("main.R")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    
    # hide error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    br(),
    
    actionButton("show", "How to Use"),
    shinyBS::bsPopover("show", title = "How to Use", content = "This app is designed to provide comarisons between countries for which we import fruit/vegetables from. For a given fruit/vegetable product, the carbon footprints can be compared to make greener choices. Data are sourced from HMRC trade data and supplimented by DfT port data.
            The numbers are only estimates and rely on some key assumptions. Namely that everything is driven from the EU to UK, and everything not in the EU is shipped/flown - depending on what time of port they arrive into.
            It is not possible to get open-source raw shipping data (for obvious reasons) so these numbers are best used as comarisons between countries rather than absolutes. \n
            \n To use this app: \n
            1) Fill in the 'Select Fruit/Vegetable' box to select the product you are interested in \n
            2) Enter country of origin you are interested in (the countries avaliable are auto-completed by the data the app has access to - if the country does not appear then the app does not have the necessary data) \n 
            3) Use bar chart to compare carbon footprints of transporting that product to the UK"),
    
    br(),
    br(),
    
    fluidRow(
        
        column(4,
               
               selectInput("commodity",
                           "Select Fruit/Vegetable",
                           choices = c(unique(trade_data$description), ""),
                           selected = unique(trade_data$description)[1],
                           width = "100%")
               ),
        column(4,
               selectInput("country",
                           "Country:",
                           choices = unique(trade_data$country)
                           )
               ),
        column(2,
               
               valueBoxOutput("likely_mode_transport"),
               shinyBS::bsTooltip("likely_mode_transport",
                                  title = "Most likely transportation method taken, using HMRC port data. EU imports not well recorded so assumed to all be Road (+English Channel). Cyprus removed as Google API could not link it to UK"),
        ),
        column(2,
               
               valueBoxOutput("ranking")
               ),

               align = "center"
            ),
        
        

    
        tabsetPanel(
            tabPanel("Product Overview",
                       
                    fluidRow(
                        column(12,
                               
                               plotlyOutput("route_plot")
                               
                               )
                            ),
                    
                    fluidRow(
                        column(12,
                               numericInput("plot_emmission_y_axis", "Change Emmissions Upper Limit",
                                            min = 0, max = 2.1e7, value = 300, step = 100),
                               plotlyOutput("plot_emmissions", height = "auto"))
                    )
            ),
            
            tabPanel("Compare Products",
                     
                     br(),
                     
                     br(),
                     
                     fluidRow(
                         column(3,
                     
                                 selectInput("commodity_compare",
                                             "Select Comparison Fruit/Vegetable",
                                             choices = c(unique(trade_data$description)),
                                             selected = "Fresh or chilled sweet peppers",
                                             width = "100%"),
                                 selectInput("country_compare",
                                             "Comparison Country:",
                                             choices = unique(trade_data$country)),
                                
                                align = "centre"
                                ),
                         
                         column(9,
                                plotOutput("plot_compare_emmissions")
                                )
                              )
 
                      )
            )
  )
    
)



