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
library(synthpop)
library(here)

# Section 2 - API---------------------------------------------------



# reloading package
devtools::reload(pkg = devtools::inst("googleAnalyticsR"))

#Authenticating account

ga_auth()

keyring_lock(keyring = "googleanalytics")

# ----------------------------------------
# only need this section when sorting website initially - not necessary in production use
#Get a list of accounts you have access to
# account_list <- ga_account_list()
# 
# account_list
# 
# #ViewID is the way to access the account you want
# account_list$viewId
#------------------------------------------

#Select full CodeClan website data
my_ga_id <- 102407343

#Set today's date and year previous for flexible API call
today <- today()
year_previous <- today() - days(365)


#Call the API to access the data required for various dashboards
dashboard_data <- google_analytics(my_ga_id,
                                   date_range = c(year_previous, today),
                                   metrics = c(
                                     "users",
                                     "sessions",
                                     "bounces",
                                     "bounceRate",
                                     "exits",
                                     "exitRate",
                                     "pageviews",
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
                                     "exitPagePath",
                                     "socialNetwork"
                                   ),
                                   max = -1,
                                   anti_sample = TRUE
)

# Had to set up a seperate API call for goal path dimensions because API doesn't work if goal path dimensions are called with channel and traffic source dimensions/metrics

goal_path_data <- google_analytics(my_ga_id,
                                   date_range = c(year_previous, today),
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

#synthesize dashboard numerical data only

dashboard_data_syn <- syn(dashboard_data,
                          visit.sequence = c("users", 
                                             "sessions", 
                                             "bounces",
                                             "bounceRate",
                                             "exits",
                                             "exitRate",
                                             "pageviews",
                                             "avgTimeOnPage",
                                             "goal3Completions",
                                             "goal5Completions"
                                             ))

dashboard_data_syn_data <- dashboard_data_syn$syn

#synthesize goal path numerical data only

goal_path_syn <- syn(goal_path_data,
                     visit.sequence = c("goal3Completions",
                                        "goal5Completions"))

goal_path_syn_data <- goal_path_syn$syn

#write both files to csv for use in dashboard

write_csv(dashboard_data_syn_data, here::here("data_synthesis/dashboard_data_syn.csv"))
write_csv(goal_path_syn_data, here::here("data_synthesis/goal_path_syn.csv"))
