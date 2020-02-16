
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
            menuItem("Loan Status", tabName = "loan_stat", icon = icon("calendar-check")),
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
                              box(width = 6, tags$div(
                                  'This dashboard was created by using a sample of 500,000 records from the LendingClub data on issued loans.', 
                                  'The original data was quite large and consisted of over 2 million records. Thus, in order to create this Shiny app,',
                                  'The data needed to be sampled to meet the size requirements. The data consisted of loans issued from 2007 to 2018.', tags$br(), tags$br(),
                                  'First, we examine why people utilize LendingClub for loan services. Next, we examine the amount of loans given out', 
                                  'and the total payments received. On the grades tab, the types of loan grades by purpose and the frequency of loan', 
                                  'grades over time are shown. Before we explore the data, we should discuss what LendingClub is.'
                              ))),
                    fluidRow(h2("What is LendingClub?"),
                             box(width = 6, tags$div(
                                 'The LendingClub (2020) website noted the company was founded in 2007. LendingClub is a leading peer-to-peer lending service.',
                                 'The company helps individuals acquire loans from other investors. LendingClub screens individuals for loan approval. ', 
                                 'Then investors can analyze various metrics suited to their investment needs. Once an individual is approved and investors',
                                 'choose to fund a loan, LendingClub aids the transaction and services the loans. Investors and LendingClub earn off of the loans',
                                 'and barrowers can use the loans as they need.'
                             ) ) ),
                    fluidRow(h4("Sources:"), 
                             box(width = 6, "LendingClub. (2020). How it works. Retrieved from ", tags$a(href="https://www.lendingclub.com/public/how-peer-lending-works.action", "https://www.lendingclub.com/public/how-peer-lending-works.action"),tags$br(),
                                 "Data retrieved from", tags$a(href="https://www.kaggle.com/wendykan/lending-club-loan-data", "here"), '.'
                                 
                             ))),
            
            
            ### Purpose ###
            tabItem(tabName = "purpose",
                    
                    fluidRow(htmlOutput('purp_bar')  ) ,
                    fluidRow(htmlOutput('purp_time') ),
                    fluidRow(
                        tabsetPanel(
                            tabPanel('Income Statistics', 
                                     DT::dataTableOutput('purp_tab'),
                                     width = 12),
                            
                            tabPanel('Home Ownership & Application Type',
                                     box(DT::dataTableOutput('home_tab'),
                                         width = 6),
                                     box(DT::dataTableOutput("app_type"), 
                                         width = 6) ) ) ) ),
            
            tabItem(tabName = "funds",
                    fluidRow( h2("Loan Funding & Repayments")),
                    fluidRow(dygraphOutput('fund_pay'))
            ),
            
            ### Grades ### 
            tabItem(tabName = "grades",
                    fluidRow( h2("Grades"),
                              fluidRow(tags$div(box(
                                  'Below, we examine the frequencies of various loan grades over time.',
                                  'We can see loans graded A, B, or C are the most frequently issued loans.', 
                                  'While the frequency of debt consolidation loans was quite high, we can see that the',
                                  'the proportion of grades within each purpose are relatively close for each purpose.' , tags$br(), tags$br(),
                                  'Note: You can click and hold to zoom in on each graph. Use right-click to zoom out.', tags$br() ) ) ) ),          
                    
                    fluidRow( htmlOutput("loan_grades") ),
                    fluidRow(htmlOutput('purp_grade') ),
                    fluidRow(
                        tabsetPanel(
                            tabPanel('Total Grade Frequencies', 
                                     box(DT::dataTableOutput('grade_freq'),
                                         width = 6) ),
                            
                            tabPanel('Grade Proportions within Purpose',
                                     box(DT::dataTableOutput('purp_g_freq'),
                                         width = 6) ) ) ) ),
            ### Loan Status ###
            tabItem(tabName = "loan_stat",
                    fluidRow( h2("Loan Status") ),
                    fluidRow(tags$div(box(
                        'Below, the status of loans with respect to the issued grade and loan purpose are presented.',
                        'We can see that "A" grade loans, compared to "B" or "C" loans, are less likely to be charged off.',
                        '"B" grade loans had a higher likelihood of default, however, "B" loans also were the most likely',
                        'to be fully paid. "C" grade loans had the highest likelihood of of being late in both categories.', tags$br(), tags$br(),
                        'Debt consolidation has the highest likelihood with respect to the proportion of purposes for each',
                        'loan status type. Thus, while debt consolidation loans were more likely to be fully paid, they were',
                        'also more likely to be charged off or in default. In the sample, only four purposes had defaults.', tags$br(), tags$br(),
                        'Note: You can click and hold to zoom in on each graph. Use right-click to zoom out.', tags$br() ) ) ),
                    fluidRow(htmlOutput("grade_status") ),
                    fluidRow(htmlOutput("purp_status") ) ),
            
            ### Map ###
            tabItem(tabName = "map",
                    fluidRow(h2("U.S. Map by Input Selection")),
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

