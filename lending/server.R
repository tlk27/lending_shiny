
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
                icon = icon("search-dollar"),
                color = "olive",
                width = 3,
                fill = TRUE)
    })
    
    output$tot_funded <- renderInfoBox({
        loan_tots() %>% 
            summarise(fund_sum = sum(funded_amnt)) %>% 
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
        purp_vis = loans_c %>%
            mutate(purpose = gsub('_', ' ', purpose)) %>% 
            group_by(purpose) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n))
        
        gvisBarChart(purp_vis, xvar = 'purpose', yvar = 'n',
                     options = list(width = 'auto', height = 'auto',
                                    hAxes="[{title:'Frequency'}]",
                                    vAxes="[{title:'Purpose'}]",
                                    chartArea =
                                         "{'width': '85%', right: '10'}",
                                    hAxis= "{'showTextEvery': '1'}",
                                    vAxis= "{'showTextEvery': '1'}",
                                    title="Frequency by Loan Purpose")

        )
    })
    
   
    output$purp_time = renderPlot({
        colors_ = c(RColorBrewer::brewer.pal(name="Set1", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 6))
        
        loans_c %>%
            filter(purpose == input$show_purp) %>%
            select(purpose, issue_d) %>%
            mutate(issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y'),
                   issue_d = as.Date(issue_d),
                   purpose = gsub('_', ' ', purpose)) %>%
            group_by(issue_d, purpose) %>%
            summarize(frequency = n()) %>%
            ggplot(aes(x=issue_d, y=frequency)) +
            geom_line(aes(color = purpose, group=purpose), stat = 'identity') +
            scale_x_date(date_labels = "%m-%Y", date_breaks = "12 months") +
            scale_color_manual(values = colors_) +
            ylab('Frequency') +
            xlab('Time') +
            ggtitle('Issued Loan Purposes Over Time') +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13),
                  title = element_text(size = 16),
                  legend.text = element_text(size = 11))
        
    }) 
    

    output$purp_tab <- renderDataTable({
        
        demo_table = dt_vars()  %>%
            summarize_all(.funs =  list( min = min,
                                         max = max,
                                         mean = mean,
                                         median = median,
                                         sd = sd ) ) %>%
            gather('stat', 'val') %>%
            separate(stat, into = c("variable", "stat"), sep = "_") %>%
            spread(stat, val) %>%
            select(variable, min, max, median, mean, sd)
        
        datatable(demo_table, rownames=FALSE, options = list(paging = F, searching = F))

    })

    output$home_tab <- renderDataTable({
        home_table = loan_purps() %>% 
            select(home_ownership) %>%
            mutate_at(.vars=c("home_ownership"), tolower) %>% 
            group_by(home_ownership) %>% 
            count() %>%
            arrange(desc(n)) %>% 
            select('home ownership' = home_ownership, n)
        
        datatable(home_table, rownames=FALSE, options = list(paging = F, searching = F))
        
    })
    
    output$app_type <- renderDataTable({
        app_table = loan_purps() %>% 
            select(application_type) %>%
            mutate_at(.vars=c("application_type"), tolower) %>%
            group_by(application_type) %>% 
            count() %>% 
            arrange(desc(n)) %>% 
            select('application type' = application_type, n)
        
        datatable(app_table, rownames=FALSE, options = list(paging = F, searching = F))
    })
    
 
    ####################
    ##### Fund Page ####
    ####################
       
    output$fund_pay = renderDygraph({
        pay_xts = xts::xts(payments()[,-1], as.Date(payments()$issue_d) )
        
        pay_xts %>% 
            dygraph(main = "Funded Loan Amounts vs. Total Received Payments by Month") %>%
            dyAxis(
                "y",
                label = "Dollars ($)",
                valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                axisLabelWidth = 70,
                axisLabelFontSize = 10
            ) %>%
            dyLegend(show = "always", hideOnMouseOut = FALSE, width = 525) %>% 
            dyOptions(colors = RColorBrewer::brewer.pal("qual", "Dark2")) %>% 
            dySeries("rec_monthly", label = "Total Payments Received \n") %>%
            dySeries("funded_monthly", label = "Total Loans Funded \n") %>%
            dyOptions(stackedGraph = TRUE) %>%
            dyRangeSelector(height = 20)
        
    })
    
    ####################
    #### Grade Page ####
    ####################
    
    output$loan_grades = renderPlot({

        loans_c %>%
        filter(grade == input$show_grades) %>%
            select(grade, issue_d) %>%
            mutate(issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y'),
                   issue_d = as.Date(issue_d)) %>%
            group_by(issue_d, grade) %>%
            summarize(frequency = n()) %>%
            ggplot(aes(x=issue_d, y=frequency)) +
            geom_line(aes(color = grade, group=grade), stat = 'identity') +
            scale_x_date(date_labels = "%m-%Y", date_breaks = "12 months") +
            scale_colour_brewer(palette = "Set1") +
            ylab('Frequency') +
            xlab('Time') +
            ggtitle('Issued Loan Grades Over Time') +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13),
                  title = element_text(size = 16),
                  legend.text = element_text(size = 11))
            
    }) 
    
    output$purp_grade = renderPlot({
        
        loan_purps() %>%
            filter(grade == input$show_grades2) %>% 
            group_by(purpose, grade) %>% 
            summarise(n = n()) %>% 
            ggplot(aes(x=purpose, y=n)) +
            geom_bar(aes(fill=grade), stat='identity', position = 'dodge') +
            coord_flip() + 
            scale_fill_brewer(palette = "Set1") +
            xlab('Purpose') +
            ylab('Frequency') +
            ggtitle('Purpose of Loan by Grade') +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13),
                  title = element_text(size = 16),
                  legend.text = element_text(size = 11))
    })
    
    output$grade_freq <- renderDataTable({
        grade_table = loan_purps() %>% 
            select(grade) %>%
            group_by(grade) %>% 
            count() %>% 
            arrange(desc(n))
        
        datatable(grade_table, rownames=FALSE, options = list(paging = F, searching = F))
    })
    
    output$purp_g_freq <- renderDataTable({
        g_purp_table = loan_purps() %>% 
            select(grade, purpose) %>%
            group_by(purpose, grade) %>% 
            count() %>%
            spread(key = grade, value = n)
        
        datatable(g_purp_table, rownames=FALSE, options = list(paging = F, searching = F))
    })

    ####################
    ##### Map Page #####
    ####################

    # show map using googleVis
    output$map <- renderGvis({
        gvisGeoChart(loan_maps(), locationvar = "addr_state", input$selected,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto",
                                  title='U.S. Map by Input Selection'))
        # using width="auto" and height="auto" to
        # automatically adjust the map size
    })
    
    # show histogram using googleVis
    output$hist <- renderGvis(
        gvisHistogram(loans_c[,input$selected, drop = FALSE]))
    
}
)









