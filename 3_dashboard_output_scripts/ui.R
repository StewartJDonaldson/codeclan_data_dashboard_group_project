ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = div("CodeClan website navigation analytics",
                style = "color: #1b3445; 
                                    font-size: 20px; 
                                    text-align: left"
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Goal completions by Channel", tabName = "goal_channel", icon = icon("futbol")),
      menuItem("Goal completions by Page", tabName = "goal_page", icon = icon("futbol")),
      menuItem("User Journey", tabName = "user_journey", icon = icon("walking")),
      menuItem("User Aquisitions", tabName = "user_aquisitions", icon = icon("users")),
      
      # Adding blank menuItem to give space for dateRangeInput
      HTML("<br/><br/><br/>")
      
    )
    
  ),
  
  dashboardBody(
    tabItems(
      

# Goal Completions by Channel UI Code -------------------------------------


      tabItem(
        tabName = "goal_channel",
        
        fluidRow(
          tabBox(
            title = "Goal Completions by Channel and Social Network",
            width = 12,
            height = "500px",
            
            
            tabPanel(
              "Channel",
              
              column(6,
                     radioButtons("downsampling_channel",
                                  "Summarise by Month or Day",
                                  c("Month", "Day"),
                                  selected = NULL)
                     
                     
              ),
              
              column(6,
                     
                     # Adding date picker that allows users to select specific date range and the plot automatically updates itself
                     
                     dateRangeInput(
                       
                       # InputId to link up date picker code within the server function
                       inputId = "goal_channel_date_range",
                       
                       # Setting "Select date range" subheader to a h5 header and bold
                       label = tags$b("Select date range"),
                       
                       # Start date in the date picker is by default is set to the first date that is found within the date column of clean_dashboard_data
                       start = first(clean_dashboard_data$date),
                       
                       # End date in the date picker by default is set to the last date found within the date column of clean_dashboard_data
                       end = last(clean_dashboard_data$date),
                       
                       #  min and max values are set to prevent users selecting values  in date picker that are outwith the dates available in clean_dashboard_data
                       min = first(clean_dashboard_data$date),
                       
                       max = last(clean_dashboard_data$date),
                       
                     )
                
              ),
              
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              
              plotlyOutput("channel_conversions_plot", height = "400px")
             
            ),
            
            
            # Goal completions by social media channel
            tabPanel(
              "Social Network",
              
              column(6,
                     radioButtons("downsampling_social", 
                                  "Summarise by Month or Day", 
                                  c("Month", "Day"), 
                                  selected = NULL)
                     
                     
              ),
              
              column(6,
                     # Adding date picker that allows users to select specific date range and the plot automatically updates itself
                     
                     dateRangeInput(
                       
                       # InputId to link up date picker code within the server function
                       inputId = "social_channel_date_range",
                       
                       # Setting "Select date range" subheader to a h5 header and bold
                       label = tags$b("Select date range"),
                       
                       # Start date in the date picker is by default is set to the first date that is found within the date column of clean_dashboard_data
                       start = first(clean_dashboard_data$date),
                       
                       # End date in the date picker by default is set to the last date found within the date column of clean_dashboard_data
                       end = last(clean_dashboard_data$date),
                       
                       #  min and max values are set to prevent users selecting values  in date picker that are outwith the dates available in clean_dashboard_data
                       min = first(clean_dashboard_data$date),
                       
                       max = last(clean_dashboard_data$date),
                       
                     )
                
              ),
           
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              
              plotlyOutput("social_conversions_plot", height = "400px"),
            )
          )
        )
      ),
     

# Goal Completions by Page UI Code --------------------------------------

       
      tabItem(
        tabName = "goal_page",
        fluidRow(
          tabBox(
            title = tagList(shiny::icon("clipboard-check"), "Where Users Click on Events on the CodeClan Website"), 
            width = 12, height = "400px",
            tabPanel("Glasgow",
                     plotOutput("goal3cc_test1_plot", height = "300px"),
                     plotOutput("goal3cc_comparison_plot", height = "300px")),
            tabPanel("Edinburgh",
                     plotOutput("goal5cc_test1_plot", height = "300px"),
                     plotOutput("goal5cc_comparison_plot", height = "300px")))
        )
      ),
      
      
      
# User Journey UI Code ----------------------------------------
      
      tabItem(
        tabName = "user_journey",
        fluidRow(
          tabBox(
            title = "Website Behaviour Flow (last 12 Months)",
            width = 12,
            height = "770px",
            
            
            # Behaviour flow tab
            tabPanel(
              "Behaviour Flow",
              fluidRow(
                column(
                  3,
                  
                  # Selector button that allows user to select which channel to view the data by
                  selectInput(
                    inputId = "channel",
                    label = "Select Channel",
                    choices = unique(entry_page_user_flow$channel),
                    selected = "direct",
                    multiple = FALSE
                  )
                ),
                
                column(
                  3,
                  
                  # Selector button that allows user to select what device to view the data by
                  selectInput(
                    inputId = "device",
                    label = "Select Device",
                    choices = unique(next_page_user_flow$device),
                    selected = "desktop",
                    multiple = FALSE
                  )
                ),
                
                # Checkbox that allows user to include or exclude (not set) values under second_page_path
                column(
                  3,
                  checkboxInput(
                    inputId = "not_set",
                    label = "exclude journeys that exit the CodeClan website",
                    value = FALSE
                  )
                )
              ),
              
              
              # Behaviour flow datatable visualisation
              fluidRow(
                shinydashboard::box(DT::dataTableOutput("user_flow"),
                                    title = "Behaviour Flow",
                                    width = 12
                )
              )
            ),
            
            # Entry page interaction tab
            tabPanel(
              "Entry Page Interaction",
              
              fluidRow(
                column(
                  3,
                  
                  # Selector button that allows user to select which channel to view the data by
                  selectInput(
                    inputId = "channel_entry",
                    label = "Select Channel",
                    choices = unique(entry_page_user_flow$channel),
                    selected = "direct",
                    multiple = FALSE
                  )
                ),
                
                column(
                  3,
                  
                  # Selector button that allows user to select what device to view the data by
                  selectInput(
                    inputId = "device_entry",
                    label = "Select Device",
                    choices = unique(next_page_user_flow$device),
                    selected = "desktop",
                    multiple = FALSE
                  )
                )
              ),
              
              fluidRow(
                
                # Entry page engagement datatable visualisation
                shinydashboard::box(DT::dataTableOutput("entry_page"),
                                    title = "Entry Page Engagement",
                                    width = 12
                )
              )
            ),
            
            tabPanel(
              "Next Page Interaction",
              
              fluidRow(
                column(
                  3,
                  
                  # Selector button that allows user to select which channel to view the data by
                  selectInput(
                    inputId = "channel_next",
                    label = "Select Channel",
                    choices = unique(entry_page_user_flow$channel),
                    selected = "direct",
                    multiple = FALSE
                  )
                ),
                
                column(
                  3,
                  
                  # Selector button that allows user to select what device to view the data by
                  selectInput(
                    inputId = "device_next",
                    label = "Select Device",
                    choices = unique(next_page_user_flow$device),
                    selected = "desktop",
                    multiple = FALSE
                  )
                )
              ),
              
              fluidRow(
                
                # Next page datatable visualisation
                shinydashboard::box(DT::dataTableOutput("next_page"),
                                    title = "Next Page Engagement",
                                    width = 12
                )
              )
            )
          ) 
        )
      ),
      

# User Acquisition UI Code ------------------------------------------------

      
      tabItem(
        tabName = "user_aquisitions",
        # first row
        fluidRow(
          infoBoxOutput("total_number_users_Box"),
          infoBoxOutput("total_number_sessions_Box"),
          infoBoxOutput("mean_bounce_rate_Box"),
          tabBox(
            title = tagList(shiny::icon("walking"), "How users arrive on the CodeClan website"), width = 12, height = "870px",
            
            # User acquisition by channel tab
            tabPanel("by channel",
                     
                     # Adding date picker that allows users to select specific date range and the plot automatically updates itself
                     
                     dateRangeInput(
                       
                       # InputId to link up date picker code within the server function
                       inputId = "date_range",
                       
                       # Setting "Select date range" subheader to a h5 header and bold
                       label = tags$b("Select date range"),
                       
                       # Start date in the date picker is by default is set to the first date that is found within the date column of clean_dashboard_data
                       start = first(clean_dashboard_data$date),
                       
                       # End date in the date picker by default is set to the last date found within the date column of clean_dashboard_data
                       end = last(clean_dashboard_data$date),
                       
                       #  min and max values are set to prevent users selecting values  in date picker that are outwith the dates available in clean_dashboard_data
                       min = first(clean_dashboard_data$date),
                       
                       max = last(clean_dashboard_data$date),
                       
                     ),
                     
                     # plot of number of users by channel by day
                     plotlyOutput("users_by_channel_by_day_plot", height = "370px"),
                     
                     tags$br(),
                     
                     # plot of number of users by channel by month
                     plotlyOutput("users_by_channel_by_month_plot", height = "370px"),
                     
            ),
            
            # User acquisition by device tab
            tabPanel("by device",
                     
                     # Adding date picker that allows users to select specific date range and the plot automatically updates itself
                     
                     dateRangeInput(
                       
                       # InputId to link up date picker code within the server function
                       inputId = "device_date_range",
                       
                       # Setting "Select date range" subheader to a h5 header and bold
                       label = tags$b("Select date range"),
                       
                       # Start date in the date picker is by default is set to the first date that is found within the date column of clean_dashboard_data
                       start = first(clean_dashboard_data$date),
                       
                       # End date in the date picker by default is set to the last date found within the date column of clean_dashboard_data
                       end = last(clean_dashboard_data$date),
                       
                       #  min and max values are set to prevent users selecting values  in date picker that are outwith the dates available in clean_dashboard_data
                       min = first(clean_dashboard_data$date),
                       
                       max = last(clean_dashboard_data$date),
                       
                     ),
                     
                     
                     plotlyOutput("users_by_device_by_day_plot", height = "370px"),
                     tags$br(),
                     plotlyOutput("users_by_device_by_month_plot", height = "370px"),
                     
            )
          )
        )
      )
    )
  )
)
