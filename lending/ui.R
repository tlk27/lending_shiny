
shinyUI(dashboardPage(
    skin = c('black'),
    
    ##### Dashboard Header #####
    dashboardHeader(title = 'Lending Club Sample'),
    
    ##### Dashboard Sidebar #####
    dashboardSidebar(
        sidebarUserPanel('Tyler Kotnour'),
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Purpose", tabName = "purpose", icon = icon("database")),
            menuItem("Funding & Payments", tabName = "funds", icon = icon("funnel-dollar")),
            menuItem("Loan Grades", tabName = "grades", icon = icon("font")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
        #new addition selectizeInput -- choice gives you the choice of the variable choice defined in global.R which contains the column names
        #,selectizeInput(inputId = 'selected', label = 'Select item to display', choice = choices )
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
                    fluidRow('This dashboard was created by using a sample of 500,000 records from the Lending Club data on issued loans. \n
                             The original data was quite large and consisted of over 2 million records. Thus, in order to create this Shiny app, the data needed to be sampled to meet the size requirements. \n
                             The data consisted of loans issued from 2007 to 2018.
                             First, we examine why people utilize Lending Club for loan services. Next, we examine the amount of loans given out and the total payments received. \n
                             On the grades tab, the types of loan grades by purpose and the frequency of loan grades over time are shown.')),
            
            ### Purpose ###
            tabItem(tabName = "purpose",
                    

                fluidRow(htmlOutput('purp_bar') ),
                fluidRow(column(1,checkboxGroupInput(inputId = "show_purp", 
                                                     label = "Select purposes to filter:",
                                                     choices = sort(unique(loans_c$purpose)), 
                                                     selected = unique(loans_c$purpose) ) ),
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
                                                          selected = unique(loans_c$grade) ) ),
                             column(11, 
                                    fluidRow(plotOutput('loan_grades')),
                                    fluidRow(plotOutput('purp_grade'))))),
            
            ### Map ###
            tabItem(tabName = "map",
                
                    fluidRow(box(htmlOutput('map'),
                                 height = 400),
                             box(htmlOutput('hist'),
                                 height = 400)),
                    fluidRow(box(
                        selectizeInput(inputId = 'selected', label = 'Select item to display', choice = choices )
                    )
                )
            ),
            
            tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput('table'),
                                 width = 12))
         )
       )
     )
   )
)
