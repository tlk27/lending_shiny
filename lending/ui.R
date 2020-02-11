
shinyUI(dashboardPage(
    skin = c('black'),
    
    ##### Dashboard Header #####
    dashboardHeader(title = 'LendingClub Sample'),
    
    ##### Dashboard Sidebar #####
    dashboardSidebar(
        sidebarUserPanel('Tyler Kotnour'),
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Purpose", tabName = "purpose", icon = icon("database")),
            menuItem("Funding & Payments", tabName = "funds", icon = icon("funnel-dollar")),
            menuItem("Loan Grades", tabName = "grades", icon = icon("font")),
            menuItem("Map", tabName = "map", icon = icon("map"))
        )

    ),
    ##### Dashboard Body #####
    dashboardBody(
        tabItems(
            ### Main Page
            tabItem(tabName = "home",
                    fluidRow(
                        
                        ### Start Total Boxes ###
                        
                        infoBoxOutput('tot_req'),
                        infoBoxOutput('tot_funded'),
                        infoBoxOutput('tot_paid')
                        
                        ### End Total Boxes ###
                    ),
                    fluidRow( h2("Introduction"),
                    box(width = 5, tags$div(
                        'This dashboard was created by using a sample of 500,000 records from the LendingClub data on issued loans.', tags$br(),
                        'The original data was quite large and consisted of over 2 million records. Thus, in order to create this Shiny app,', tags$br(),
                        'The data needed to be sampled to meet the size requirements. The data consisted of loans issued from 2007 to 2018.', tags$br(),
                        'First, we examine why people utilize LendingClub for loan services. Next, we examine the amount of loans given out', tags$br(),
                        'and the total payments received. On the grades tab, the types of loan grades by purpose and the frequency of loan', tags$br(), 
                        'grades over time are shown. Before we explore the data, we should discuss what LendingClub is.'
                    ))),
                    fluidRow(h2("What is LendingClub?"),
                     box(width = 5, tags$div(
                         'The LendingClub (2020) website noted the company was founded in 2007. LendingClub is a leading peer-to-peer lending service.', tags$br(),
                         'The company helps individuals acquire loans from other investors. LendingClub screens individuals for loan approval. ', tags$br(),
                         'Then investors can analyze various metrics suited to their investment needs. Once an individual is approved and investors', tags$br(),
                         'choose to fund a loan, LendingClub aids the transaction and services the loans. Investors and LendingClub earn off of the loans', tags$br(),
                         'and barrowers can use the loans as they need.'
                                 ) ) ),
                    fluidRow(h4("Sources:"), 
                     box(width = 5, "LendingClub. (2020). How it works. Retrieved from ", tags$a(href="https://www.lendingclub.com/public/how-peer-lending-works.action", "https://www.lendingclub.com/public/how-peer-lending-works.action"),tags$br(),
                         "Data retrieved from", tags$a(href="https://www.kaggle.com/wendykan/lending-club-loan-data", "here"), '.'
                         
                     ))),
       
            
            ### Purpose ###
            tabItem(tabName = "purpose",
                    
                fluidRow(htmlOutput('purp_bar')  ) ,
                fluidRow(column(1,checkboxGroupInput(inputId = "show_purp", 
                                                     label = "Select purposes to filter:",
                                                     choices = sort(c("debt consolidation" = "debt_consolidation","credit card"="credit_card","moving","car","other","home improvement"="home_improvement","medical","small business"="small_business","major purchase"="major_purchase","vacation","wedding","house","renewable energy"="renewable_energy","educational")), 
                                                     selected = c("debt_consolidation","credit_card", "home_improvement") ) ),
                    column(11, plotOutput('purp_time') ) ),
                fluidRow(
                    tabsetPanel(
                        tabPanel('Income Statistics', 
                                 DT::dataTableOutput('purp_tab'),
                                 width = 12),
                        
                        tabPanel('Home Ownership & Application Type',
                                 box(DT::dataTableOutput('home_tab'),
                                 width = 4),
                                 box(DT::dataTableOutput("app_type"), 
                                 width = 4) ) ) ) ),
            
            tabItem(tabName = "funds",
                    fluidRow(dygraphOutput('fund_pay'))
                    ),
            
            tabItem(tabName = "grades",
                    fluidRow(column(1, checkboxGroupInput(inputId = "show_grades", 
                                                          label = "Select grades to filter:",
                                                          choices = sort(unique(loans_c$grade)), 
                                                          selected = c("A", "B", "C") ) ),
                            column(11, plotOutput('loan_grades')) ),
                             fluidRow(column(1, checkboxGroupInput(inputId = "show_grades2", 
                                                                   label = "Select grades to filter:",
                                                                   choices = sort(unique(loans_c$grade)), 
                                                                   selected = unique(loans_c$grade) ) ),
                                     column(11, plotOutput('purp_grade') ) ),
                    fluidRow(
                        tabsetPanel(
                            tabPanel('Total Grade Frequencies', 
                                     box(DT::dataTableOutput('grade_freq'),
                                     width = 4) ),
                            
                            tabPanel('Grade Frequencies by Purpose',
                                     box(DT::dataTableOutput('purp_g_freq'),
                                         width = 4) ) ) ) ),
            
            
            ### Map ###
            tabItem(tabName = "map",
                    fluidRow(h3("U.S. Map by Input Selection")),
                    fluidRow(box(htmlOutput('map'))),
                    fluidRow(box(
                        selectizeInput(inputId = 'selected', label = 'Select an item to display', choice = choices )
                    )
                )
            )
         )
       )
     )
   )
