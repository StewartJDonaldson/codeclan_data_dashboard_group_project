
server <- function(input, output) {


  # Section 1 - Goal Channel Server Code ------------------------------------
  
  # filters data in the channels tab when user selects date in the date picker:
  goal_channel_dashboard_data_filtered <- reactive({
    clean_dashboard_data %>%
      filter(
        date >= input$goal_channel_date_range[1] &
          date <= input$goal_channel_date_range[2]
      )
  })
  
  # channel conversions plot:
  output$channel_conversions_plot <- renderPlotly({

    # down-sample based on radio button input:
    if (input$downsampling_channel == "Month") {

      # combine goal completions, group channel and month, summarise
      channel_full_month <- goal_channel_dashboard_data_filtered() %>%
        mutate(goal_total = glas_info_session_click_completions +
          edin_info_session_click_completions) %>%
        mutate(year_month = substr(date, 1, 7)) %>%
        group_by(channel_grouping, year_month) %>%
        summarise(goal_total_channel = sum(goal_total)) %>%
        ungroup()

      # plot
      month_plot <- ggplot(channel_full_month) +
        geom_line(aes(
          x = year_month,
          y = goal_total_channel,
          group = channel_grouping,
          colour = channel_grouping
        ),
        alpha = 0.7
        ) +
        labs(
          x = "\nMonth",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Channel",
          colour = "Channel"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
      ggplotly(month_plot)
    } else {
      channel_day <- goal_channel_dashboard_data_filtered() %>%

        # combine goal completions, group channel and date, summarise
        mutate(goal_total = glas_info_session_click_completions +
          edin_info_session_click_completions) %>%
        group_by(channel_grouping, date) %>%
        summarise(goal_total_channel = sum(goal_total))

      # plot
      day_plot <- ggplot(
        channel_day,
        aes(
          x = date,
          y = goal_total_channel
        )
      ) +
        geom_point(aes(colour = channel_grouping),
          alpha = 0.7
        ) +
        labs(
          x = "Date",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Channel",
          colour = "Channel"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
      ggplotly(day_plot)
    }
  })

  # social media conversions plot:
  
  # filters data on social network tab when user selects date in the date picker:
  social_channel_dashboard_data_filtered <- reactive({
    clean_dashboard_data %>%
      filter(
        date >= input$social_channel_date_range[1] &
          date <= input$social_channel_date_range[2]
      )
  })
  
  # first filter top-performing networks - too many small ones:
  
  goal_social_top <- reactive({
    social_channel_dashboard_data_filtered() %>%
      filter(social_network != "(not set)") %>%
      mutate(goal_total = glas_info_session_click_completions +
        edin_info_session_click_completions) %>%
      group_by(social_network) %>%
      summarise(goal_total_social = sum(goal_total)) %>%
      arrange(desc(goal_total_social)) %>%
      slice(1:7)
  })


  # plot
  output$social_conversions_plot <- renderPlotly({

    # now filter by top-performers and group by date
    # down_sample based on radio button input
    if (input$downsampling_social == "Month") {
      social_month <- social_channel_dashboard_data_filtered() %>%

        # filter to only top-performing social networks
        filter(social_network %in% goal_social_top()$social_network) %>%

        # combine goal completions, group social network and month, summarise
        mutate(goal_total = glas_info_session_click_completions +
          edin_info_session_click_completions) %>%
        mutate(year_month = substr(date, 1, 7)) %>%
        group_by(social_network, year_month) %>%
        summarise(goal_total_social = sum(goal_total)) %>%
        ungroup()

      # plot
      social_month_plot <- ggplot(
        social_month,
        aes(
          x = year_month,
          y = goal_total_social,
          group = social_network
        )
      ) +
        geom_line(aes(colour = social_network),
          alpha = 0.7
        ) +
        labs(
          x = "Month",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Social Network",
          colour = "Social Network"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
      ggplotly(social_month_plot)
    } else {
      social_day <- social_channel_dashboard_data_filtered() %>%

        # filter to only top-performing social networks
        filter(social_network %in% goal_social_top()$social_network) %>%

        # combine goal completions, group channel and date, summarise
        mutate(goal_total = glas_info_session_click_completions +
          edin_info_session_click_completions) %>%
        group_by(social_network, date) %>%
        summarise(goal_total_social = sum(goal_total))

      # plot
      social_day_plot <- ggplot(
        social_day,
        aes(
          x = date,
          y = goal_total_social
        )
      ) +
        geom_point(aes(colour = social_network),
          alpha = 0.7
        ) +
        labs(
          x = "Date",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Social Network",
          colour = "Social Network"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
      ggplotly(social_day_plot)
    }
  })

  # Section 2 - User Journey Dashboard Server Script ------------------------------------

  # behaviour flow data table server script that allows user to filter by channel and device
  output$user_flow <- DT::renderDataTable({
    filtered_behaviour_flow <- behaviour_flow %>%
      filter(
        channel == input$channel,
        device == input$device,
      )

    # server script that allows user to check "Exclude (not set)" checkbox and removes (not set) values from second_page_path

    if_not_set <- if (input$not_set) {
      filter(filtered_behaviour_flow, `next page` != "**user exited the site**")
    } else {
      filtered_behaviour_flow
    }

    # Column visibility button that allows user to include or exclude certain columns within data table
    DT::datatable(if_not_set,
      extensions = c("Buttons"),
      rownames = FALSE,
      options = list(dom = "Bfrtip", buttons = I("colvis"))
    )
  })

  # Entry page data table server script that allows users to segment by channel and device
  output$entry_page <- DT::renderDataTable({
    entry_page <- entry_page_user_flow %>%
      filter(
        channel == input$channel_entry,
        device == input$device_entry,
      )

    # Column visibility button that allows user to include or exclude certain columns within entry page data table
    DT::datatable(entry_page,
      extensions = c("Buttons"),
      rownames = FALSE,
      options = list(dom = "Bfrtip", buttons = I("colvis"))
    )
  })

  # Second page datatable server script that allows users to segment by channel and device
  output$next_page <- DT::renderDataTable({
    next_page <- next_page_user_flow %>%
      filter(
        channel == input$channel_next,
        device == input$device_next,
      )

    # Column visibility button that allows user to include or exclude certain columns within second page data table
    DT::datatable(next_page,
      extensions = c("Buttons"),
      rownames = FALSE,
      options = list(dom = "Bfrtip", buttons = I("colvis"))
    )
  })


  # Section 3 - Goal Page Server Code ---------------------------------------

  output$goal3cc_test1_plot <- renderPlot({
    goal3cc_test1 %>%
      select(
        month, year, glas_info_session_click_completions,
        category
      ) %>%
      group_by(category) %>%
      summarise(count = sum(glas_info_session_click_completions)) %>%
      ggplot(aes(x = category, y = count, fill = "#4da2cd" )) +
      geom_col() +
      labs(
        x = "Page Category",
        y = "Glasgow Click Completions",
        title = "Glasgow Event Clicks by Page Category"
      ) +
      scale_fill_codeclan() +
      theme(legend.position = "none")
  })

  output$goal5cc_test1_plot <- renderPlot({
    goal5cc_test1 %>%
      select(month, year, edin_info_session_click_completions, category) %>%
      group_by(category) %>%
      summarise(count = sum(edin_info_session_click_completions)) %>%
      ggplot(aes(x = category, y = count, fill = "#4da2cd")) +
      geom_col() +
      labs(
        x = "Page Category",
        y = "Edinburgh Click Completions",
        title = "Edinburgh Event Clicks by Page Category"
      ) +
      scale_fill_codeclan() +
      theme(legend.position = "none")
  })

  output$goal3cc_comparison_plot <- renderPlot({
    goalcc_comparison %>%
      select(
        month, year, glas_info_session_click_completions,
        edin_info_session_click_completions,
        category, previous_step_category
      ) %>%
      group_by(previous_step_category) %>%
      summarise(
        count5 = sum(edin_info_session_click_completions),
        count3 = sum(glas_info_session_click_completions)
      ) %>%
      ggplot(aes(
        x = previous_step_category, y = count3,
        fill = "#4da2cd"
      )) +
      geom_col() +
      labs(
        x = "Previous Step Page Category",
        y = "Glasgow Click Completions",
        title = "Previous Page Category before Glasgow Event Clicks"
      ) +
      scale_fill_codeclan() +
      theme(legend.position = "none")
  })

  output$goal5cc_comparison_plot <- renderPlot({
    goalcc_comparison %>%
      select(
        month, year, glas_info_session_click_completions,
        edin_info_session_click_completions,
        category, previous_step_category
      ) %>%
      group_by(previous_step_category) %>%
      summarise(
        count5 = sum(edin_info_session_click_completions),
        count3 = sum(glas_info_session_click_completions)
      ) %>%
      ggplot(aes(
        x = previous_step_category, y = count5,
        fill = "#4da2cd"
      )) +
      geom_col() +
      labs(
        x = "Previous Step Page Category",
        y = "Edinburgh Click Completions",
        title = "Previous Page Category before Edinburgh Event Clicks"
      ) +
      scale_fill_codeclan() +
      theme(legend.position = "none")
  })


  # Section 4 - User Acquisition server code --------------------------------
  
  # filters data in the channel tab when user selects date in the date picker:
  dashboard_data_filtered <- reactive({
    clean_dashboard_data %>%
      filter(
        date >= input$date_range[1] &
          date <= input$date_range[2]
      )
  })

  # Code for info boxes above plots
  
  # Total number of users info box
  total_users <- reactive({
    dashboard_data_filtered() %>%
      summarise(
        users = sum(users)
      )
  })

  
  output$total_number_users_Box <- renderInfoBox({
    infoBox(
      "Total number of users: ", prettyNum(total_users(), big.mark = ","), tags$em(" (over date range selected)"),
      icon = icon("users"),
      color = "navy"
    )
  })

  # Total number of sessions info box
  
  total_sessions <- reactive({
    dashboard_data_filtered() %>%
      summarise(
        sessions = sum(sessions)
      )
  })

  output$total_number_sessions_Box <- renderInfoBox({
    infoBox(
      "Total number of sessions: ", prettyNum(total_sessions(), big.mark = ","), tags$em(" (over date range selected)"),
      icon = icon("user-friends"),
      color = "blue"
    )
  })

  # mean bounce rate info box
  
  mean_bounce_rate <- reactive({
    dashboard_data_filtered() %>%
      summarise(
        bounce = mean(bounce_rate_percentage)
      )
  })

  output$mean_bounce_rate_Box <- renderInfoBox({
    infoBox(
      "Mean bounce rate: ", paste0(round(mean_bounce_rate()), "%"), tags$em("(over date range selected)"),
      icon = icon("door-open"),
      color = "light-blue"
    )
  })
  
  # Users by device tab server code
  
  # filters data in device tab when user selects date in the date picker:
  device_dashboard_data_filtered <- reactive({
    clean_dashboard_data %>%
      filter(
        date >= input$device_date_range[1] &
          date <= input$device_date_range[2]
      )
  })

  users_by_device_by_day <- reactive({
    device_dashboard_data_filtered() %>%
      group_by(date, device_category) %>%
      summarise(
        users = sum(users)
      )
  })

  output$users_by_device_by_day_plot <- renderPlotly({
    ggplotly(ggplot(users_by_device_by_day()) +
      geom_line(aes(x = date, y = users, col = device_category)) +
      labs(
        x = "\nDate",
        y = "Number of users",
        title = "Number of users by device category by day",
        col = "Device"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(breaks = "1 month", labels = scales::date_format("%Y-%m-%d")) +
      scale_color_codeclan(discrete = TRUE, palette = "main"))
  })

  users_by_device_by_month <- reactive({
    device_dashboard_data_filtered() %>%
      group_by(year, month, device_category) %>%
      summarise(
        users = sum(users)
      ) %>%
      mutate(year_month = make_date(year, month))
  })

  output$users_by_device_by_month_plot <- renderPlotly({
    ggplotly(ggplot(users_by_device_by_month()) +
      geom_col(aes(x = year_month, y = users, fill = device_category)) +
      labs(
        x = "\nMonth",
        y = "Number of users",
        title = "Number of users by channel grouping by month",
        fill = "Device"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(breaks = "1 month", labels = scales::date_format("%Y-%m")) +
      scale_fill_codeclan(discrete = TRUE, palette = "main"))
  })

  # Users by Channel server code
  users_by_channel_by_day <- reactive({
    dashboard_data_filtered() %>%
      group_by(date, channel_grouping) %>%
      summarise(
        users = sum(users)
      )
  })

  output$users_by_channel_by_day_plot <- renderPlotly({
    ggplotly(ggplot(users_by_channel_by_day()) +
      geom_line(aes(x = date, y = users, col = channel_grouping)) +
      labs(
        x = "\nDate",
        y = "Number of users",
        title = "Number of users by channel grouping by day",
        col = "Channel"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(breaks = "1 month", labels = scales::date_format("%Y-%m-%d")) +
      scale_color_codeclan(discrete = TRUE, palette = "mixed"))
  })

  users_by_channel_by_month <- reactive({
    dashboard_data_filtered() %>%
      group_by(year, month, channel_grouping) %>%
      summarise(
        users = sum(users)
      ) %>%
      mutate(year_month = make_date(year, month))
  })

  output$users_by_channel_by_month_plot <- renderPlotly({
    ggplotly(ggplot(users_by_channel_by_month()) +
      geom_col(aes(x = year_month, y = users, fill = channel_grouping)) +
      labs(
        x = "\nMonth",
        y = "Number of users",
        title = "Number of users by channel grouping by month",
        fill = "Channel"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(breaks = "1 month", labels = scales::date_format("%Y-%m")) +
      scale_fill_codeclan(discrete = TRUE, palette = "mixed"))
  })
}
