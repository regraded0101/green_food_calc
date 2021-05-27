#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                        "Country",
                        choices = trade_data %>% distinct(country) %>% pull()),
            selectInput("commodity", 
                        "Commodity",
                        choices = distinct(trade_data, description))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            DT::DTOutput("table"),
            valueBoxOutput("emm_rank"),
            DT::DTOutput("better_emmissions"),
            DT::DTOutput("worse_emmissions")
        )
    )
))
