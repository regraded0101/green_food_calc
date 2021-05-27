#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    output$table <- renderDT({
        
        get_emmisions(import_country = input$country,
                      commodity = input$commodity)
        
        
    })

    output$emm_rank <- renderValueBox({
        
        valueBox("Percentile: ",
                 get_emmis_rank(commodity = input$commodity,
                                import_country = input$country))
        
        
    })
    
    output$better_emmissions <- renderDataTable({
        
        datatable(get_better_emmis(commodity = input$commodity,
                                   import_country = input$country),
                  caption = "Less Polluting Travel")
        
    })
    
    output$worse_emmissions <- renderDataTable({
        
        
        datatable(get_worse_emmis(commodity = input$commodity,
                                  import_country = input$country),
                  caption = "More Polluting Travel")
        
    })
    
})
