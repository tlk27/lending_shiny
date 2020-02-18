
shinyServer(function(input, output) {
    
    ####################
    #### Home  Page ####
    ####################
    
    ####################
    ##### Map Page #####
    ####################
    
    output$map <- renderGvis({
        gvisGeoChart(loan_maps(), locationvar = "addr_state", input$selected,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto",
                                  title='U.S. Map by Input Selection'))
        
    })
    
    
    ####################
    ##### Purpose ######
    ####################
    
    ## Plot: Purpose Frequencies
    output$purp_bar <- renderGvis({
        purp_vis = loans_c %>%
            mutate(purpose = gsub('_', ' ', purpose)) %>%
            group_by(purpose) %>%
            summarise(n = n()) %>%
            arrange(desc(n))
        
        gvisBarChart(purp_vis, xvar = 'purpose', yvar = 'n',
                     options = list(width = 'auto', height = 400,
                                    hAxes="[{title:'Frequency'}]",
                                    vAxes="[{title:'Purpose'}]",
                                    chartArea =
                                       "{'width': '85%', right: '10'}",
                                    hAxis= "{'showTextEvery': '3'}",
                                    #vAxis= "{'showTextEvery': '1'}",
                                    title="Frequency by Loan Purpose",
                                    explorer="{actions:['dragToZoom', 'rightClickToReset']}"))
        
    })
    
    
    ## Plot: Purpose x Time Frequencies
    output$purp_time = renderGvis({
        
            pt_plot = grades_df() %>%
                mutate(issue_d = as.Date(issue_d)) %>% 
                group_by(issue_d, purpose) %>%
                summarise(frequency = n()) %>% 
                spread(key = purpose, value = frequency)
        
        
        gvisLineChart(pt_plot, xvar="issue_d", yvar= c("debt consolidation", "credit card",        "moving",             "car",               "other",              "home improvement",  
                                                       "medical",            "small business",     "major purchase",     "vacation",           "wedding",            "house",             
                                                       "renewable energy",   "educational"),
                      options = list(width = 'auto', height = 400,
                                     hAxes="[{title:'Time'}]",
                                     vAxes="[{title:'Frequency'}]",
                                     hAxis= "{'showTextEvery': '1'}",
                                     vAxis= "{'showTextEvery': '1'}",
                                     title="Frequency of Issued Loans by Purpose Over Time",
                                     explorer="{actions:['dragToZoom', 'rightClickToReset']}",
                                     bar="{groupWidth:'100%'}"))
    })
        
    
    ## DT: Income Stats
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
    
    ## DT: Home Ownership Frequencies
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
    
    ## DT: Application Type
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
                color = "black",
                width = 5,
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
                color = "maroon",
                width = 5,
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
                color = "purple",
                width = 5,
                fill = TRUE)
    })
    
    output$est_profit <- renderInfoBox({
        payments() %>% 
            summarise(fund_sum = sum(funded_amnt),
                      paid_sum = sum(total_pymnt),
                      remain_sum = sum(amount_remain, na.rm =T),
                      est_profit = (paid_sum + remain_sum) - fund_sum ) %>% 
            .$est_profit %>% 
            scales::dollar() %>% 
            infoBox(
                title= "Estimated Potential Profit ($)",
                icon = icon("dollar-sign"),
                color = "olive",
                width = 5,
                fill = TRUE)
    })
    
    
    ## Dygraph: Funded vs. Received
    output$fund_pay = renderDygraph({
        pay_group = payments() %>% 
            group_by(issue_d) %>% 
            summarise(rec_monthly = round(sum(total_pymnt), 2), funded_monthly = round(sum(funded_amnt), 2) )
        
        pay_xts = xts::xts(pay_group[,-1], as.Date(pay_group$issue_d) )
        
        pay_xts %>% 
            dygraph(main = "Funded Loan Amounts vs. Total Received Payments by Month") %>%
            dyAxis(
                "y",
                label = "Dollars ($)",
                valueFormatter = 'function(f){return f.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                axisLabelFormatter = 'function(f){return f.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
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
    
    ## Plot: Frequency of Issued Loans by Grade Over Time
    output$loan_grades = renderGvis({

        grade_time = grades_df() %>% 
            # select(grade, issue_d) %>% 
            # mutate(issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y'),
            #        issue_d = as.Date(issue_d)) %>% 
            group_by(issue_d, grade) %>% 
            summarise(frequency = n()) %>% 
            spread(key = grade, value = frequency)
        
        gvisLineChart(grade_time, xvar="issue_d", yvar=c("A", "B", "C", "D", "E", "F", "G"),
                      options = list(width = 'auto', height = 400,
                                     hAxes="[{title:'Time'}]",
                                     vAxes="[{title:'Frequency'}]",
                                     hAxis= "{'showTextEvery': '1'}",
                                     vAxis= "{'showTextEvery': '1'}",
                                     title="Frequency of Issued Loans by Grade Over Time",
                                     explorer="{actions:['dragToZoom', 'rightClickToReset']}",
                                     bar="{groupWidth:'100%'}"))

    })
    
    ## Plot: Proportion of Grades within Loan Purpose
    output$purp_grade = renderGvis({
        purp_x_grade = grades_df() %>% 
            group_by(purpose, grade) %>% 
            summarise(n = n()) %>% 
            mutate(proportion = round(n / sum(n), 3)) %>%
            select(-n) %>% 
            spread(key = grade, value = proportion)
        
        gvisColumnChart(purp_x_grade, xvar="purpose", yvar=c("A", "B", "C", "D", "E", "F", "G"),
                        options = list(width = 'auto', height = 400,
                                       hAxes="[{title:'Purpose'}]",
                                       vAxes="[{title:'Proportion'}]",
                                       hAxis= "{'showTextEvery': '1'}",
                                       vAxis= "{'showTextEvery': '1'}",
                                       title="Proportion of Grades within Loan Purpose",
                                       explorer="{actions:['dragToZoom', 'rightClickToReset']}"))
    })
    

    
    ## DT: Grade Freq
    output$grade_freq <- renderDataTable({
        grade_table = grades_df() %>% 
            # select(grade) %>%
            group_by(grade) %>% 
            summarise(n = n()) %>% 
            mutate(proportion = round(n / sum(n), 3)) %>% 
            arrange(grade)
        
        datatable(grade_table, rownames=FALSE, options = list(paging = F, searching = F))
    })
    
    ## DT: Grade x Purpose
    output$purp_g_freq <- renderDataTable({
        g_purp_table = grades_df() %>%
            group_by(purpose, grade) %>% 
            summarise(n = n()) %>% 
            mutate(proportion = round(n / sum(n), 3)) %>%
            select(-n) %>% 
            spread(key = grade, value = proportion)
        
        datatable(g_purp_table, rownames=FALSE, options = list(paging = F, searching = F))
    })

    ######################
    ##### Loan Status ####
    ######################   
    
    ##status_prop
    
    output$status_prop = renderGvis ({
        
        status_prop_graph = grades_df() %>%
            group_by(loan_status) %>%
            summarise(n = n()) %>%
            mutate(proportion = n / sum(n)) %>% 
            arrange(desc(proportion))

        
        gvisBarChart(status_prop_graph, xvar = "loan_status", yvar = "proportion",
                        options = list(width = 'auto', height = 400,
                                       hAxes ="[{title:'Proportion'}]",
                                       vAxes ="[{title:'Loan Status'}]",
                                       hAxis = "{'showTextEvery': '2'}",
                                       vAxis = "{'showTextEvery': '1'}",
                                       title ="Total Proportion by Loan Status",
                                       explorer ="{actions:['dragToZoom', 'rightClickToReset']}")) 
    })

    
    ## Plot: Proportion of Grades within Loan Status
    output$grade_status = renderGvis ({
        
        status_g_graph = grades_df() %>%
            group_by(loan_status, grade) %>%
            summarise(n = n()) %>%
            mutate(proportion = n / sum(n)) %>%
            select(-n) %>% 
            spread(key = grade, value = proportion)
        
        gvisColumnChart(status_g_graph, xvar = "loan_status", yvar = c("A", "B", "C", "D", "E", "F", "G"),
                        options = list(width = 'auto', height = 400,
                                       hAxes ="[{title:'Proportion'}]",
                                       vAxes ="[{title:'Loan Status'}]",
                                       hAxis = "{'showTextEvery': '1'}",
                                       vAxis = "{'showTextEvery': '1'}",
                                       title ="Proportion of Grades within Loan Status",
                                       explorer ="{actions:['dragToZoom', 'rightClickToReset']}")) 
    })
        
        ## Plot: Proportion of Purpose within Loan Status
        output$purp_status = renderGvis ({
            
            status_purp_graph = grades_df() %>%
                group_by(loan_status, purpose) %>% 
                summarise(n = n()) %>% 
                mutate(proportion = n / sum(n)) %>%
                select(-n) %>% 
                spread(key = purpose, value = proportion)
            
            gvisColumnChart(status_purp_graph, xvar ="loan_status", yvar = c("debt consolidation", "credit card",        "moving",             "car",               "other",              "home improvement",  
                                                                           "medical",            "small business",     "major purchase",     "vacation",           "wedding",            "house",             
                                                                           "renewable energy",   "educational")  ,
                            options = list(width = 'auto', height = '450',
                                           hAxes ="[{title:'Loan Status'}]",
                                           vAxes ="[{title:'Proportion'}]",
                                           hAxis = "{'showTextEvery': '1'}",
                                           vAxis = "{'showTextEvery': '1'}",
                                           title ="Proportion of Purposes within Loan Status",
                                           explorer ="{actions:['dragToZoom', 'rightClickToReset']}"))
            
        })
    
    
    
})










