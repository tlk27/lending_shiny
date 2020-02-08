
shinyServer(function(input, output) {
    
    ####################
    #### Home  Page ####
    ####################
    ### Totals Boxes ###
    ####################
    
    output$tot_req <- renderInfoBox({
        loan_tots() %>% 
            summarise(loan_sum = sum(loan_amnt)) %>% 
            .$loan_sum %>% 
            scales::dollar() %>% 
        infoBox(
                title= "Total Loans Requested ($)",
                # value = sum(loans_c[, 'loan_amnt']),
                icon = icon("search-dollar"),
                color = "olive",
                width = 3,
                fill = TRUE)
    })
    
    output$tot_funded <- renderInfoBox({
        loan_tots() %>% 
            summarise(fund_sum = sum(funded_amnt),) %>% 
            .$fund_sum %>% 
            scales::dollar() %>% 
            infoBox(
                title= "Total Loans Funded ($)",
                icon = icon("hand-holding-usd"),
                color = "green",
                width = 3,
                fill = TRUE)
    })
    
    output$tot_paid <- renderInfoBox({
        loan_tots() %>% 
            summarise(paid_sum = sum(total_pymnt)) %>% 
            .$paid_sum %>% 
            scales::dollar() %>% 
            infoBox(
                title= "Total Loans Paid ($)",
                icon = icon("money-check-alt"),
                color = "olive",
                width = 3,
                fill = TRUE)
    })
    
    ####################
    ##### Purpose ######
    ####################
    output$purp_bar <- renderGvis({
        purp_vis = loan_purps %>%
            group_by(purpose) %>% 
            summarise(n_purp = n(purpose))
        
        gvisBarChart(n_puprxvar='purpose', 
            
        )
    })
    

    ####################
    ##### Map Page #####
    ####################

    
    # show map using googleVis
    output$map <- renderGvis({
        gvisGeoChart(loans_c, locationvar = "addr_state", input$selected,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto"))
        # using width="auto" and height="auto" to
        # automatically adjust the map size
    })
    
    # show histogram using googleVis
    output$hist <- renderGvis(
        gvisHistogram(loans_c[,input$selected, drop = FALSE]))
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(loans_c, rownames=FALSE) %>% 
            formatStyle(input$selected,  
                        background="skyblue", fontWeight='bold')
        # Highlight selected column using formatStyle
    })
    
    # show statistics using infoBox
    # output$maxBox <- renderInfoBox({
    #     max_value <- max(loans_c[,input$selected])
    #     max_state <- 
    #         loans_c$addr_state[loans_c[,input$selected]==max_value]
    #     infoBox(max_state, max_value, icon = icon("hand-o-up"))
    # })
    # output$minBox <- renderInfoBox({
    #     min_value <- min(loans_c[,input$selected])
    #     min_state <- 
    #         loans_c$addr_state[loans_c[,input$selected]==min_value]
    #     infoBox(min_state, min_value, icon = icon("hand-o-down"))
    # })
    # output$avgBox <- renderInfoBox(
    #     infoBox(paste("AVG.", input$selected),
    #             mean(loans_c[,input$selected]), 
    #             icon = icon("calculator"), fill = TRUE))
}
)









