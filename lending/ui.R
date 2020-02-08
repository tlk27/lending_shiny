
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
                    ) ),
            
            tabItem(tabName = "purpose",
                    
                    fluidRow(infoBoxOutput(""),
                             infoBoxOutput(""),
                             infoBoxOutput("")),
                    fluidRow(box(htmlOutput('purp_bar'),
                                 height = 400),
                             box(htmlOutput('OTHERPLOT_NOT_NAMED'),
                                 height = 400)),
                    fluidRow(box(
                        selectizeInput(inputId = 'selected', label = 'Select item to display', choice = choices )
                    )
                    )
            ),                 
            tabItem(tabName = "map",
                    
                    fluidRow(infoBoxOutput("maxBox"),
                             infoBoxOutput("minBox"),
                             infoBoxOutput("avgBox")),
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
    
    
    
    
    
    
    
    
    
    
    
    
    
))