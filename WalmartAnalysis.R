library(vroom)
library(dplyr)
library(DataExplorer)




# Upload Data ------------------------------------------------------------------

train_data <- vroom("train.csv.zip")
test_data <- vroom("test.csv.zip")
stores_data <- vroom("stores.csv")
features_data <- vroom("features.csv.zip")

# Join Datasets
train_data <- left_join(train_data, stores_data, by = "Store")

train_data$Store_Date <- paste0(as.character(train_data$Store), "_", as.character(train_data$Date))
features_data$Store_Date <- paste0(as.character(features_data$Store), "_", as.character(features_data$Date))
features_data$Store <- NULL
features_data$Date <- NULL
features_data$IsHoliday <- NULL
train_data <- left_join(train_data, features_data, by = "Store_Date")

# Get rid of NA's
train_data$MarkDown1[is.na(train_data$MarkDown1)] <- 0
train_data$MarkDown2[is.na(train_data$MarkDown2)] <- 0
train_data$MarkDown3[is.na(train_data$MarkDown3)] <- 0
train_data$MarkDown4[is.na(train_data$MarkDown4)] <- 0
train_data$MarkDown5[is.na(train_data$MarkDown5)] <- 0

# More MarkDown Wrangling
train_data$TotalMarkDown <- train_data$MarkDown1 + train_data$MarkDown2 + train_data$MarkDown3 + train_data$MarkDown4 + train_data$MarkDown5
train_data$MarkDownFlag <- ifelse(train_data$TotalMarkDown != 0, TRUE, FALSE)
train_data$MarkDown1 <- NULL
train_data$MarkDown2 <- NULL
train_data$MarkDown3 <- NULL
train_data$MarkDown4 <- NULL
train_data$MarkDown5 <- NULL



# EDA --------------------------------------------------------------------------

DataExplorer::plot_intro(train_data)
DataExplorer::plot_correlation(train_data)
DataExplorer::plot_bar(train_data)
DataExplorer::plot_missing(train_data)


# Recipe -----------------------------------------------------------------------

# recipe
my_recipe <- recipe(count~., data=train_data) %>%
  step_mutate(weather = ifelse(weather == 4,3,weather)) %>%
  step_mutate(weather=factor(weather)) %>%
  step_time(datetime, features=c("hour")) %>%
  step_date(datetime, features=c("dow", "month", "year")) %>%
  step_mutate(datetime_dow = as.numeric(datetime_dow)) %>%
  step_harmonic(datetime_hour, cycle_size = 24, frequency = 1) %>%
  step_harmonic(datetime_dow, cycle_size = 7, frequency = 1) %>%
  step_rm(datetime) %>%
  step_mutate(season=factor(season)) %>%
  step_mutate(workingday=factor(workingday)) %>%
  step_mutate(holiday=factor(holiday)) %>%
  # step_interact(~ datetime_hour:workingday) %>%
  # step_interact(~ holiday:workingday) %>%
  # step_interact(~ weather:workingday) %>%
  # step_interact(~ season:workingday) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())






# # apply the recipe to your data1
# prep <- prep(my_recipe)
# baked <- bake(prep, new_data = train_data)





