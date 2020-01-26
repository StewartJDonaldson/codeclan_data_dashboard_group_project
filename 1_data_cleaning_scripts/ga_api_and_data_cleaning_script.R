# Section 1 - Loading libraries-------------------------------------

library(googleAnalyticsR)
library(keyring)
library(janitor)
library(ggplot2)
library(formattable)
library(tidyverse)
library(ggthemes)
library(lubridate)

# Section 2 - API---------------------------------------------------


keyring_unlock(keyring = "googleanalytics")
# Setting options(googleAuthR.client_id) and options(googleAuthR.client_secret) to activate API call
options(googleAuthR.client_id = keyring::key_get(service = "ga_client_id", keyring = "googleanalytics"))
options(googleAuthR.client_secret = keyring::key_get(service = "ga_client_secret", keyring = "googleanalytics"))

# reloading package
devtools::reload(pkg = devtools::inst("googleAnalyticsR"))

#Authenticating account

ga_auth()

keyring_lock(keyring = "googleanalytics")

#Get a list of accounts you have access to
account_list <- ga_account_list()

account_list

#ViewID is the way to access the account you want
account_list$viewId

#Select the one you want to work with
my_ga_id <- 102407343

#Call the API to access the data required for various dashboards
dashboard_data <- google_analytics(my_ga_id,
                                   date_range = c("2018-12-01", "2019-12-01"),
                                   metrics = c(
                                     "sessions",
                                     "bounces",
                                     "bounceRate",
                                     "exitRate",
                                     "avgTimeOnPage",
                                     "goal3Completions",
                                     "goal5Completions"),
                                   dimensions = c(
                                     "date",
                                     "channelGrouping",
                                     "source",
                                     "deviceCategory",
                                     "landingPagePath",
                                     "secondPagePath",
                                     "exitPagePath"
                                   ),
                                   max = -1,
                                   anti_sample = TRUE
)

# Had to set up a seperate API call for goal path dimensions because API doesn't work if goal path dimensions are called with channel and traffic source dimensions/metrics

goal_path_data <- google_analytics(my_ga_id,
                                   date_range = c("2018-12-01", "2019-12-01"),
                                   metrics = c(
                                     "goal3Completions",
                                     "goal5Completions"
                                   ),
                                   dimensions = c(
                                     "date",
                                     "goalCompletionLocation",
                                     "goalPreviousStep1",
                                     "goalPreviousStep2"
                                   ),
                                   max = -1,
                                   anti_sample = TRUE
)



# data cleaning script ----------------------------------------------------

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
    bounce_rate      = round(bounce_rate, 2),
    exit_rate        = round(exit_rate, 2)
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
