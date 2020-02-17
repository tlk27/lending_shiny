
shinyUI(dashboardPage(
    skin = c('black'),
    
    ##### Dashboard Header #####
    dashboardHeader(title = 'LendingClub Sample'),
    
    ##### Dashboard Sidebar #####
    dashboardSidebar(
        sidebarUserPanel('Tyler Kotnour'),
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Funding by State", tabName = "map", icon = icon("map")),
            menuItem("Purpose", tabName = "purpose", icon = icon("database")),
            menuItem("Loan Grades", tabName = "grades", icon = icon("font")),
            menuItem("Loan Status", tabName = "loan_stat", icon = icon("calendar-check")),
            menuItem("Funding and Payments", tabName = "funds", icon = icon("funnel-dollar"))
        )
        
    ),
    ##### Dashboard Body #####
    dashboardBody(
        tabItems(
            ### Main Page
            tabItem(tabName = "home",

                    fluidRow( h2("Introduction"),
                              box(width = 6, tags$div(
                                  'This dashboard was created by using a sample of 500,000 records from the LendingClub data on issued loans.', 
                                  'The original data was quite large and consisted of over 2 million records. Thus, in order to create this Shiny app,',
                                  'the data needed to be sampled to meet the size requirements. The data consisted of loans issued from 2007 to 2018.', tags$br(), tags$br(),
                                  'First, we examine funding by state and why people utilize LendingClub for loan services.', 
                                  'On the grades tab, the types of loan grades by purpose and the frequency of loan grades over time are shown.', 
                                  'Next, we examine the amount of loans given out and the total payments received. Before we explore the data, we should discuss what LendingClub is.'
                              ))),
                    fluidRow(h2("What is LendingClub?"),
                             box(width = 6, tags$div(
                                 'The LendingClub (2020) website noted the company was founded in 2007. LendingClub is a leading peer-to-peer lending service.',
                                 'The company helps individuals acquire loans from other investors. LendingClub screens individuals for loan approval. ', 
                                 'Following screening, investors can analyze various metrics suited to their investment needs and risk tolerance. Once an individual is approved and investors',
                                 'choose to fund a loan, LendingClub aids the transaction and services the loans. Investors and LendingClub earn off of the loans',
                                 'and borrowers can use the loans as they need.'
                             ) ) ),
                    fluidRow(h4("Sources:"), 
                             box(width = 6, "LendingClub. (2020). How it works. Retrieved from ", tags$a(href="https://www.lendingclub.com/public/how-peer-lending-works.action", "https://www.lendingclub.com/public/how-peer-lending-works.action"),tags$br(),
                                 "Data retrieved from", tags$a(href="https://www.kaggle.com/wendykan/lending-club-loan-data", "here"), '.'
                                 
                             ))),
            
            ### Map ###
            tabItem(tabName = "map",
                    fluidRow(h2("U.S. Map by Input Selection")),
                    fluidRow(box(htmlOutput('map'))),
                    fluidRow(box(
                        selectizeInput(inputId = 'selected', label = 'Select an item to display', choice = choices )
                    ) ) ),
            
            ### Purpose ###
            tabItem(tabName = "purpose",
                    
                    fluidRow( h2("Applicant's Loan Purpose") ),
                    
                    fluidRow(box(tags$div(
                        'The most frequented purpose for a borrower to use LendingClub was for debt consolidation followed by credit card and home improvement.',
                        'We can see this trend over time. Futhermore, the growth of debt consolidation purposes outpaced the growth of the other purposes.',
                        'After the plots, applicant summary statistics are provided.', tags$br(), tags$br(),
                        'Note: You can click and hold to zoom in on each graph. Use right-click to zoom out.'
                    ) ) ),
                              
                    fluidRow(htmlOutput('purp_bar')  ) ,
                    fluidRow(htmlOutput('purp_time') ),
                    
                    fluidRow( h2("Applicant Characteristics") ),
            
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
            
            ### Funding & Payments ###
            tabItem(tabName = "funds",
                    fluidRow( h2("Loan Funding & Repayments")),
                    
                    fluidRow(box(tags$div(
                        'The total loans requested, funded, current total payments, and the estimated potential profits are show below.',
                        'The potential estimated profit was estimated by totaling the remaining amount due for each loan. I added the total outstanding',
                        'amount to the total amount paid and subtracted the total amount that was funded. This is the maximum potential profit assuming all loans were paid in full.', tags$br(), tags$br(),
                        'Additionally, the time series shows the total payments received compared to the the total loans funded by month. We can see how the totals received surpasses', 
                        'the total loans funded. The gap narrows near 2018 because loans are issued on a 36 or 60 month time frame.'
                    ) ) ),
                    
                    ### Start Total Boxes ###
                    
                    fluidRow(                         
                        infoBoxOutput('tot_req'),
                        infoBoxOutput('tot_funded') ),
                    fluidRow(
                        infoBoxOutput('tot_paid'),
                        infoBoxOutput('est_profit') ),
                        
                        ### End Total Boxes ###
                    
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
                        'to be fully paid. "C" grade loans had the highest likelihood of being late in both categories.', tags$br(), tags$br(),
                        'Debt consolidation has the highest likelihood of occurring with respect to the proportion of purposes for each',
                        'loan status type. Thus, while debt consolidation loans were more likely to be fully paid, they were',
                        'also more likely to be charged off or in default. In the sample, only four purposes had defaults.', tags$br(), tags$br(),
                        'Note: You can click and hold to zoom in on each graph. Use right-click to zoom out.', tags$br() ) ) ),
                    
                    fluidRow(htmlOutput("status_prop") ),
                    fluidRow(htmlOutput("grade_status") ),
                    fluidRow(htmlOutput("purp_status") ) )
            

        )
    )
)
)
