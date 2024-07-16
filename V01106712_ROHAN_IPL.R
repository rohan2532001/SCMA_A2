install.packages(c("dplyr", "readxl", "fuzzyjoin", "caret", "stats"))

library(dplyr)
library(readxl)
library(fuzzyjoin)
library(caret)
library(stats)


setwd('C:\\Users\\HP\\Desktop')
getwd()

df_ipl <- read.csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_excel("IPL SALARIES 2024.xlsx")

#Data Aggregation
#Aggregate runs scored and wickets:
grouped_data <- df_ipl %>%
  group_by(Season, Innings.No, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored), 
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Total Runs and Wickets Each Year
total_runs_each_year <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored)) %>%
  ungroup()

total_wicket_each_year <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation)) %>%
  ungroup()

library(fuzzyjoin)

# Fuzzy matching function
match_names <- function(name, names_list) {
  match <- stringdist_left_join(data.frame(name=name), data.frame(names_list=names_list), 
                                by = c("name" = "names_list"), max_dist = 2, distance_col = "distance")
  if (nrow(match) > 0) {
    return(match$names_list[1])
  } else {
    return(NA)
  }
}

# Apply fuzzy matching for runs data
salary$Matched_Player <- sapply(salary$Player, function(x) match_names(x, total_runs_each_year$Striker))

# Merge DataFrames on matched names
df_merged <- left_join(salary, total_runs_each_year, by = c("Matched_Player" = "Striker"))

# Subset data for last three years
df_merged <- df_merged %>% filter(Season %in% c('2021', '2022', '2023'))

#Linear Regression Model (Runs Scored vs Salary)
# Linear regression model
model <- lm(Rs ~ runs_scored, data = df_merged)
summary(model)


# Apply fuzzy matching for wickets data
salary$Matched_Player <- sapply(salary$Player, function(x) match_names(x, total_wicket_each_year$Bowler))

# Merge DataFrames on matched names
df_merged_wickets <- left_join(salary, total_wicket_each_year, by = c("Matched_Player" = "Bowler"))

# Filter data for the year 2022
df_merged_wickets <- df_merged_wickets %>% filter(Season == '2022')

# Linear regression model
model_wickets <- lm(Rs ~ wicket_confirmation, data = df_merged_wickets)
summary(model_wickets)