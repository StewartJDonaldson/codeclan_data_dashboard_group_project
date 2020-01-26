
# Section 1 - Loading Libraries-------------------------------------

library(googleAnalyticsR)
library(keyring)
library(janitor)
library(ggplot2)
library(formattable)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)

# Section 2 - API Call---------------------------------------------------
# Example of the original GA API call code. However, due to the sensitivity of the data the dashboard is using synthesized data instead
# 
#
# # reloading package
# devtools::reload(pkg = devtools::inst("googleAnalyticsR")) 
# 
# #Authenticating account
# 
# ga_auth()
# 
# keyring_lock(keyring = "googleanalytics") 
# 
# #Select full CodeClan website data
# my_ga_id <- 102407343
# 
# #Set today's date and year previous for flexible API call
# today <- today()
# year_previous <- today() - days(365)
# 
# 
# #Call the API to access the data required for various dashboards
# dashboard_data <- google_analytics(my_ga_id,
#                  date_range = c(year_previous, today),
#                  metrics = c(
#                    "users",
#                    "sessions",
#                    "bounces",
#                    "bounceRate",
#                    "exits",
#                    "exitRate",
#                    "pageviews",
#                    "avgTimeOnPage",
#                    "goal3Completions",
#                    "goal5Completions"),
#                  dimensions = c(
#                    "date",
#                    "channelGrouping",
#                    "source",
#                    "deviceCategory",
#                    "landingPagePath",
#                    "secondPagePath",
#                    "exitPagePath",
#                    "socialNetwork"
#                  ),
#                  max = -1,
#                  anti_sample = TRUE
# )
# 
# # Had to set up a seperate API call for goal path dimensions because API doesn't work if goal path dimensions are called with channel and traffic source dimensions/metrics
# 
# goal_path_data <- google_analytics(my_ga_id,
#                                    date_range = c(year_previous, today),
#                                    metrics = c(
#                                      "goal3Completions",
#                                      "goal5Completions"
#                                      ),
#                                    dimensions = c(
#                                      "date",
#                                      "goalCompletionLocation",
#                                      "goalPreviousStep1",
#                                      "goalPreviousStep2"
#                                    ),
#                                    max = -1,
#                                    anti_sample = TRUE
# )
# 
# Section 3 - Synthesized Data --------------------------------------------

#import data from synthesized csv's with same filenames as before

dashboard_data <- read_csv(here::here("/2_clean_data/dashboard_data_syn.csv"))
goal_path_data <- read_csv(here::here("/2_clean_data/goal_path_syn.csv"))

# Section 4 - Data Cleaning Script ----------------------------------------------------

# Cleaning dashboard data

dashboard_data <- clean_names(dashboard_data)

clean_dashboard_data <- dashboard_data %>%
  
  # adding year, month, day columns based on date column
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
    ) %>%
  
  # converting avg_time_on_page from seconds into user friendly format of hours, minutes and seconds
  mutate(
    avg_time_on_page_period = as.period(avg_time_on_page)
    ) %>%
  
  # reordering data so year, month, day columns are beside the date column
  select(
    date,
    year,
    month,
    day,
    channel_grouping:avg_time_on_page,
    avg_time_on_page_period,
    goal3completions,
    goal5completions
         ) %>%
  
  # converting character vectors to lower case
  mutate(
    channel_grouping = str_to_lower(channel_grouping),
    source           = str_to_lower(source),
    bounce_rate      = round(bounce_rate, 0),
    exit_rate        = round(exit_rate, 0)
    ) %>%
  
  # renaming columns to help user better understand the data
  rename (
    edin_info_session_click_completions = goal5completions,
    glas_info_session_click_completions = goal3completions,
    bounce_rate_percentage = bounce_rate,
    exit_rate_percentage = exit_rate
  )


# Cleaning goal path data

goal_path_data <- clean_names(goal_path_data)

clean_goal_path_data <- goal_path_data %>%
  
  # adding year, month, day columns based on date column
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) %>%
  
  # reordering data so year, month, day columns are beside the date column
  select(
    date,
    year,
    month,
    day,
    goal_completion_location:goal5completions
    ) %>%
  rename (
    edin_info_session_click_completions = goal5completions,
    glas_info_session_click_completions = goal3completions
  )

# Section 5 - Colour Pallete---------------------------------------------------
# codeclan corporate colour pallete
codeclan_colours <- c(
  `codeclan dark blue` = "#1b3445",
  `codeclan light blue` = "#4da2cd",
  `codeclan other blue` = "#4ca1cd",
  `codeclan gold`     = "#e1bf76",
  `codeclan pink` = "#e74b7d",
  `codeclan red` = "red",
  `codeclan light grey` = "#cccccc",
  `codeclan dark grey`  = "#8c8c8c")

# Any changes to these colours, or addition of new colours, are done in the above vector.

# Function that extracts the hex codes from this vector by name.

codeclan_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (codeclan_colours)

  codeclan_colours[cols]
}

# This allows us to get hex colors in a robust and flexible way. For example, you can have all colours returned as they are, specify certain colours, in a particular order, add additional function arguments and checks, and so on:
# codeclan_cols()
# codeclan_cols("codeclan gold", "codeclan light blue")


codeclan_palettes <- list(
  `main`  = codeclan_cols("codeclan light blue", "codeclan dark blue", "codeclan gold"),
  `cool`  = codeclan_cols("codeclan dark blue", "codeclan light blue", "codeclan other blue"),
  `hot`   = codeclan_cols("codeclan gold", "codeclan red", "codeclan pink"),
  `mixed` = codeclan_cols("codeclan gold","codeclan light blue", "codeclan other blue","codeclan dark blue", "codeclan dark grey", "codeclan light grey", "codeclan pink"),
  `grey`  = codeclan_cols("codeclan light grey", "codeclan dark grey"),
  `one_tone` = codeclan_cols("codeclan light blue")
)

codeclan_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- codeclan_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

# This function gets a pallete by name from the list ("main" by default), has a boolean condition determining whether to reverse the order or not, and additional arguments to pass on to colorRampPallete() (such as an alpha value). This returns another function:

# Scales for ggplot2

scale_color_codeclan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- codeclan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("example_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Fill scale constructor for codeclan colours

scale_fill_codeclan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- codeclan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("example_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



# Section 6 - Goal Completion by Page Data Cleaning---------------------------------------------------
## Wrangling

# Selecting variables for Goal 3 (glas_info_session_click_completions)

# completions by page
goal3cc <- clean_goal_path_data  %>%
  select(date, year, month, day, glas_info_session_click_completions, 
         goal_completion_location) %>%
  filter(glas_info_session_click_completions >= 1)

# shortening the name of the websites and categorising them
goal3cc_test1 <- goal3cc %>%
  mutate(category = case_when(
    str_detect(goal_completion_location, "/events/")   ~ "events",
    str_detect(goal_completion_location, "(entrance)") ~ "entrance",
    str_detect(goal_completion_location, "/blog/")     ~ "blog", 
    str_detect(goal_completion_location, "/enquire/")  ~ "enqiry",
    str_detect(goal_completion_location, "/courses/")  ~ "courses",
    str_detect(goal_completion_location,
               "/data-analysis-announcement/")  ~ "data",
    TRUE  ~ "other"
  ))

# Selecting variables for Goal 5 (edin_info_session_click_completions)

# completions by page

goal5cc <- clean_goal_path_data  %>%
  select(date, year, month, day, edin_info_session_click_completions, 
         goal_completion_location) %>%
  filter(edin_info_session_click_completions >= 1)


# shortening the name of the websites and categorising them
goal5cc_test1 <- goal5cc %>%
  mutate(category = case_when(
    str_detect(goal_completion_location, "/events/")   ~ "events",
    str_detect(goal_completion_location, "(entrance)") ~ "entrance",
    str_detect(goal_completion_location, "/blog/")     ~ "blog", 
    str_detect(goal_completion_location, "/enquire/")  ~ "enqiry",
    str_detect(goal_completion_location, "/courses/")  ~ "courses",
    str_detect(goal_completion_location,
               "/data-analysis-announcement/")  ~ "data",
    TRUE  ~ "other"
  ))


# Selecting variables for Goal Completions after Previous Step

goalcc_previous <- clean_goal_path_data  %>%
  select(date, year, month, day, glas_info_session_click_completions, 
         edin_info_session_click_completions, 
         goal_completion_location, goal_previous_step1)


# Shortening the names of the websites and categorising them
# for the Previous Step 1

goalcc_previous_clean <-goalcc_previous %>%
  mutate(previous_step_category = case_when(
    str_detect(goal_previous_step1, "/events/") ~ "events",
    str_detect(goal_previous_step1, "(entrance)") ~ "entrance",
    str_detect(goal_previous_step1, "/blog/") ~ "blog",
    str_detect(goal_previous_step1, "/enquire/") ~ "enqiry",
    str_detect(goal_previous_step1, "/courses/") ~ "courses",
    str_detect(goal_previous_step1, "/about-us/") ~ "about_us",
    str_detect(goal_previous_step1, "/codeclan-store/") ~
      "codeclan_store",
    str_detect(goal_previous_step1, "/for-employers/") ~
      "for_employers",
    str_detect(goal_previous_step1, "/highlands-academy/") ~
      "highlands_academy",
    str_detect(goal_previous_step1, "/pre-course-work") ~
      "pre_course_work",
    str_detect(goal_previous_step1, "/ourbrand/") ~ "our_brand",
    str_detect(goal_previous_step1, "/the-codeclan-experience") ~
      "codeclan_experience",
    str_detect(goal_previous_step1, "/our-board/") ~ "our_board",
    str_detect(goal_previous_step1, "/admissions-track") ~
      "admissions",
    str_detect(goal_previous_step1, "/tech-boom/") ~ "tech_boom",
    TRUE ~ "other"
  ))


# For the goal completion location

goalcc_previous_clean2 <- goalcc_previous_clean %>%
  mutate(category = case_when(
    str_detect(goal_completion_location, "/events/")   ~ "events",
    str_detect(goal_completion_location, "(entrance)") ~ "entrance",
    str_detect(goal_completion_location, "/blog/")     ~ "blog", 
    str_detect(goal_completion_location, "/enquire/")  ~ "enqiry",
    str_detect(goal_completion_location, "/courses/")  ~ "courses",
    str_detect(goal_completion_location,
               "/data-analysis-announcement/")  ~ "data",
    TRUE  ~ "other"
  ))

# Selecting and Filtering for goal completions

goalcc_comparison <- goalcc_previous_clean2 %>%
  select(year, month, edin_info_session_click_completions, 
         glas_info_session_click_completions, 
         category, previous_step_category) %>%
  filter(edin_info_session_click_completions >= 1, 
         glas_info_session_click_completions >= 1) 


# Section 7 - User Journey Dashboard Data Cleaning --------------------------------

# data cleaning for behaviour flow

# Selecting the relevant columns for the dashboard

behaviour_flow <- clean_dashboard_data %>%
  select(
    date,
    channel_grouping,
    device_category,
    sessions,
    landing_page_path,
    bounces,
    second_page_path,
    exits,
    edin_info_session_click_completions,
    glas_info_session_click_completions
  ) %>%
  mutate(
    landing_page_path = if_else(landing_page_path == "/", "homepage", landing_page_path),
    second_page_path = if_else(second_page_path == "(not set)", "**user exited the site**", second_page_path)
  ) %>%
  
  # Grouping and summarising the data so that it looks better visually in the dashboard and not as granular
  
  group_by(
    "channel" = channel_grouping,
    "device" = device_category,
    landing_page_path,
    second_page_path
    ) %>%
  summarise(
    sessions = sum(sessions),
    bounces = sum(bounces),
    exits = sum(exits),
    edin_info_session_click_completions = sum(edin_info_session_click_completions),
    glas_info_session_click_completions = sum(glas_info_session_click_completions)
    ) %>%
  
  # Setting filters to refine the data that is coming into the dashboard so it is more insightful for the users:
  # Excluding blog pages and internal business pages that are not for public use
  # pages that have 10 sessions or more
  
  filter(
    str_detect(landing_page_path, "/blog/", negate = TRUE),
    sessions >= 10,
    landing_page_path != "/pre-course-work/",
    landing_page_path != "/admissions-track/",
    second_page_path != "/admissions-track/") %>%
  arrange(desc(sessions)) %>%
  
  # Renaming columns so they are more user friendly in the dashboard
  rename(
    "entry page"  = landing_page_path,
    "next page" = second_page_path,
    "entry page exit"         = bounces,
    "next page exit"        = exits,
    "goal conv. (edin)"       = edin_info_session_click_completions,
    "goal conv. (glas)"       = glas_info_session_click_completions
  ) %>%
  select(
    sessions,
    "entry page",
    "entry page exit",
    "next page",
    "next page exit",
    "goal conv. (edin)",
    "goal conv. (glas)"
  )

# data cleaning for entry page engagement table
entry_page_user_flow <- clean_dashboard_data %>%
  select(
    date,
    channel_grouping,
    device_category,
    sessions,
    landing_page_path,
    bounces
  ) %>%
  mutate(
  landing_page_path = if_else(landing_page_path == "/", "homepage", landing_page_path)) %>%
  group_by(
    "channel" = channel_grouping,
    "device" = device_category,
    landing_page_path
  ) %>%
  summarise(
    sessions = sum(sessions),
    bounces = sum(bounces),
  ) %>%
  mutate(
    "drop off rate (%)" = round((bounces/sessions) * 100, 0)
  ) %>%
  select(
    channel,
    device,
    landing_page_path,
    sessions,
    bounces,
    "drop off rate (%)"
  ) %>%
  filter(
    landing_page_path != "/pre-course-work/",
    landing_page_path != "/admissions-track/",
    str_detect(landing_page_path, "/blog/", negate = TRUE),
    sessions >= 10,
    bounces >= 1
  ) %>%
  arrange(desc(bounces), desc(sessions)) %>%
  rename(
    "entry page" = landing_page_path,
    "drop offs" = bounces
  )


# data cleaning for next page engagement table

next_page_user_flow <- clean_dashboard_data %>%
  select(
    date,
    channel_grouping,
    device_category,
    sessions,
    second_page_path,
    exits,
    pageviews,
    edin_info_session_click_completions,
    glas_info_session_click_completions
  ) %>%
  mutate(
    second_page_path = if_else(second_page_path == "/", "homepage", second_page_path)
  ) %>%
  group_by(
    "channel" = channel_grouping,
    "device" = device_category,
     second_page_path
    ) %>%
  summarise(
    sessions = sum(sessions),
    exits = sum(exits),
    pageviews = sum(pageviews),
    "info session click (ed)" = sum(edin_info_session_click_completions),
    "info session click (gla)" = sum(glas_info_session_click_completions)
  ) %>%
  mutate(
    "drop off rate (%)" = round((exits / pageviews) * 100, 0)
  ) %>%
  select(
    channel,
    device,
    second_page_path,
    sessions,
    exits,
    "drop off rate (%)",
  ) %>%
  filter(
    second_page_path != "(not set)",
    second_page_path != "/admissions-track/",
    str_detect(second_page_path, "/blog/", negate = TRUE),
    sessions >= 10,
    exits >= 1
  ) %>%
  arrange(desc(sessions), desc(exits)) %>%
  rename(
    "next page" = second_page_path,
    "drop offs" = exits
  )
  

