

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # reactive function that gets most likely form of transport from commodity & country
    
    get_transport <- reactive({
    transport <- 
        trade_data %>%
        filter(description == input$commodity,
               country == input$country) %>%
        distinct(country, .keep_all = TRUE) %>%
        mutate(likely_transport_type = case_when(likely_transport_type == "Road/Sea_EC" ~ "Road",
                                                 NA ~ "Unknown",
                                                 TRUE ~ likely_transport_type)) %>%
        pull(likely_transport_type)
    
    return(transport)
        
    })
    
    get_all_commodity_countries <- reactive({
        
        all_commodity_countries <- 
            trade_data %>% 
            filter(description == input$commodity,
                   !is.na(co2_emmissions)) %>%
            arrange(co2_emmissions) %>%
            distinct(country, .keep_all = TRUE)
        
        return(all_commodity_countries)
    })
    
    output$ranking <- renderValueBox({
        
        all_commodity_countries <- get_all_commodity_countries()
       
        ranking <- scales::ordinal(which(all_commodity_countries$country == input$country))
        
        valueBox(ranking, subtitle = "Country Ranking")
        
    })
    
    output$plot_emmissions <- renderPlot({
        
        # return NULL if nothing selected
        if(input$commodity=="") return(NULL)
        
        all_commodity_countries <- 
            get_all_commodity_countries() %>%
            arrange(desc(co2_emmissions))
        # create a column to mark which country has been selected
        
        all_commodity_countries <- 
            all_commodity_countries %>%
            mutate(selected = case_when(country == input$country ~ 1,
                                        TRUE ~ 0))
        
         plot <- 
             ggplot(data = all_commodity_countries, aes(x = co2_emmissions, 
                                                    y = fct_inorder(country), 
                                                    fill = factor(selected))) +
             scale_fill_manual(values= c("lightgrey", "#005b96")) +
             geom_col() +
             theme(panel.background = element_blank(),
                   axis.title = element_blank(),
                   legend.position = "none") +
             coord_cartesian(xlim = c(0,input$plot_emmission_y_axis))
         
        return(plot)
        
    },
    height = function() 20* nrow(get_all_commodity_countries()))
    
    output$likely_mode_transport <- renderValueBox({
        
        
        transport_type <- 
            get_transport()
        
        valueBox(transport_type, subtitle = "Likely transportation mode", width = 12)
    })
    
    output$route_plot <- renderPlotly({
        
        if(input$commodity=="") return(NULL)
        
        # set settings for geo plot
        geo <- list(
            showland = TRUE,
            showlakes = TRUE,
            showcountries = TRUE,
            showocean = TRUE,
            countrywidth = 0.5,
            landcolor = toRGB("grey90"),
            lakecolor = toRGB("white"),
            oceancolor = toRGB("white"),
            projection = list(
                type = 'orthographic',
                rotation = list(
                    lon = 3,
                    lat = 10,
                    roll = 0
                )
            ),
            lonaxis = list(
                showgrid = TRUE,
                gridcolor = toRGB("gray40"),
                gridwidth = 0.5
            ),
            lataxis = list(
                showgrid = TRUE,
                gridcolor = toRGB("gray40"),
                gridwidth = 0.5
            )
        )
        
        # get all possible routes
        routes_possible <- 
            sea_air_road_paths %>%
            filter(group == input$country)
        
        # get the most likely route
        likely_route <- 
            get_all_commodity_countries() %>%
            filter(country == input$country) %>%
            pull(likely_transport_type)
        
        # get the path of the most likely transport
        likely_route_path <- 
            routes_possible %>%
            filter(type == likely_route)
        
        
        plot_geo() %>%
            add_trace(data = likely_route_path, name = "Route Likely Taken", x = ~lon, y = ~lat, color = I("red"), type = "scatter", mode = "lines", line = list(dash="dot")) %>%
            layout(
                showlegend = FALSE, geo = geo
            )
        
        
        
        
    })
    
    # create donut plot to compare countries
    
    output$plot_compare_emmissions <- renderPlot({
        
        # pull emmissions data for selected & comparion products
        selected_emissions <- 
            trade_data %>%
            filter(description == input$commodity,
                   country == input$country) %>%
            distinct(country, .keep_all = TRUE) %>%
            pull(co2_emmissions)
        
        comp_emmissions <-
            trade_data %>%
            filter(description == input$commodity_compare,
                   country == input$country_compare) %>%
            distinct(country, .keep_all = TRUE) %>%
            pull(co2_emmissions)
        
        
        
        # create df and manipulate for donut plot
        comp_emmis_df <- data.frame(country = c(paste0(input$country, " (selected)"), paste0(input$country_compare, " (comparison)")),
                                    commodity = c(input$commodity, input$commodity_compare),
                                    emmission = c(selected_emissions, comp_emmissions)
        )
        
        comp_emmis_df <- comp_emmis_df %>% mutate(perc_emm = emmission/sum(emmission),
                                                  ymax = cumsum(perc_emm),
                                                  ymin = c(0, head(ymax, n = -1)))
        
        
        plot <-
            ggplot(comp_emmis_df, aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = country)) +
            geom_rect() +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            labs(fill = "") +
            ggtitle("Carbon Dioxide Emmissions") +
            scale_fill_manual(values = c("#DF791301", "#005b96")) 
        
        return(plot)
        
    })
    
    
    # automatically update the countries avaliable to select based on commodity code
    observe({
        
        updateSelectInput(session, "country",
                          choices = trade_data %>% filter(description == input$commodity, !is.na(co2_emmissions)) %>% distinct(country) %>% pull() %>% sort())
    })
    
    observe({
        
        updateSelectInput(session, "country_compare",
                          choices = trade_data %>% filter(description == input$commodity_compare, !is.na(co2_emmissions)) %>% distinct(country) %>% pull() %>% sort())
        
        
    })
    
    # Create instructions popup
    observeEvent(input$show, {
        showModal(modalDialog(
            title = "Instructions to use app",
            "This app is designed to provide comarisons between countries for which we import fruit/vegetables from. For a given fruit/vegetable product, the carbon footprints can be compared to make greener choices. Data are sourced from HMRC trade data and supplimented by DfT port data.
            The numbers are only estimates and rely on some key assumptions. Namely that everything is driven from the EU to UK, and everything not in the EU is shipped/flown - depending on what time of port they arrive into.
            It is not possible to get open-source raw shipping data (for obvious reasons) so these numbers are best used as comarisons between countries rather than absolutes. \n
            \n To use this app: \n
            1) Fill in the 'Select Fruit/Vegetable' box to select the product you are interested in \n
            2) Enter country of origin you are interested in (the countries avaliable are auto-completed by the data the app has access to - if the country does not appear then the app does not have the necessary data) \n 
            3) Use bar chart to compare carbon footprints of transporting that product to the UK"
        ))
    })
    
})
