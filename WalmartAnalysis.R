library(vroom)
library(dplyr)
#library(DataExplorer)
library(tidyverse)
library(tidymodels)
library(glmnet)
library(rpart)
library(ranger)
library(bonsai)
library(lightgbm)
library(agua)
library(prophet)



#
# Upload Data ------------------------------------------------------------------

train_data <- vroom("train.csv.zip")
test_data <- vroom("test.csv.zip")
stores_data <- vroom("stores.csv")
features_data <- vroom("features.csv.zip")

# Join Datasets
train_data <- left_join(train_data, features_data, by = c("Store", "Date", "IsHoliday"))
train_data <- left_join(train_data, stores_data, by = "Store")

test_data <- left_join(test_data, features_data, by = c("Store", "Date", "IsHoliday"))
test_data <- left_join(test_data, stores_data, by = "Store")

# Get rid of MarkDown NA's
train_data$MarkDown1[is.na(train_data$MarkDown1)] <- 0
train_data$MarkDown2[is.na(train_data$MarkDown2)] <- 0
train_data$MarkDown3[is.na(train_data$MarkDown3)] <- 0
train_data$MarkDown4[is.na(train_data$MarkDown4)] <- 0
train_data$MarkDown5[is.na(train_data$MarkDown5)] <- 0

test_data$MarkDown1[is.na(test_data$MarkDown1)] <- 0
test_data$MarkDown2[is.na(test_data$MarkDown2)] <- 0
test_data$MarkDown3[is.na(test_data$MarkDown3)] <- 0
test_data$MarkDown4[is.na(test_data$MarkDown4)] <- 0
test_data$MarkDown5[is.na(test_data$MarkDown5)] <- 0

# More MarkDown Wrangling
train_data$TotalMarkDown <- train_data$MarkDown1 + train_data$MarkDown2 + train_data$MarkDown3 + train_data$MarkDown4 + train_data$MarkDown5
train_data$MarkDownFlag <- ifelse(train_data$TotalMarkDown != 0, TRUE, FALSE)
train_data$MarkDown1 <- NULL
train_data$MarkDown2 <- NULL
train_data$MarkDown3 <- NULL
train_data$MarkDown4 <- NULL
train_data$MarkDown5 <- NULL

test_data$TotalMarkDown <- test_data$MarkDown1 + test_data$MarkDown2 + test_data$MarkDown3 + test_data$MarkDown4 + test_data$MarkDown5
test_data$MarkDownFlag <- ifelse(test_data$TotalMarkDown != 0, TRUE, FALSE)
test_data$MarkDown1 <- NULL
test_data$MarkDown2 <- NULL
test_data$MarkDown3 <- NULL
test_data$MarkDown4 <- NULL
test_data$MarkDown5 <- NULL

# Get rid of other NA's
feature_recipe <- recipe(~., data=train_data) %>%
  step_mutate(DecDate=decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with=imp_vars(DecDate, Store))
train_data <- juice(prep(feature_recipe))

feature_recipe <- recipe(~., data=test_data) %>%
  step_mutate(DecDate=decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with=imp_vars(DecDate, Store))
test_data <- juice(prep(feature_recipe))


# EDA --------------------------------------------------------------------------

# DataExplorer::plot_intro(train_data)
# DataExplorer::plot_correlation(train_data)
# DataExplorer::plot_bar(train_data)
# DataExplorer::plot_missing(train_data)

# Recipe -----------------------------------------------------------------------

# Filter by Store and Department
train_data <- train_data[train_data$Store == 17,]  # 25
train_data <- train_data[train_data$Dept == 17,] # 11

my_recipe <- recipe(Weekly_Sales~., data=train_data) %>%
  step_date(Date, features=c("doy", "month", "year")) %>%
  step_range(Date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(Date_doy), cosDOY=cos(Date_doy)) %>%
  step_rm(Date) %>%
  step_mutate(IsHoliday = factor(IsHoliday), MarkDownFlag = factor(MarkDownFlag)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())



# Penalized Regression ---------------------------------------------------------

# preg_model <- linear_reg(penalty=tune(), mixture=tune()) %>%
#   set_engine("glmnet")
# 
# preg_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(preg_model)
# 
# tuning_grid <- grid_regular(penalty(), mixture(), levels=3)
# folds <- vfold_cv(train_data, v=3, repeats=1)
# 
# cv_results <- preg_wf %>%
#   tune_grid(resamples=folds, grid=tuning_grid, metrics=metric_set(mae))
# best_tune <- cv_results %>%
#   collect_metrics()
# 
# mean(best_tune$std_err)


# Random Forests ---------------------------------------------------------------

# forest_mod <- rand_forest(mtry=tune(), min_n=tune(), trees=tune()) %>%
#   set_engine("ranger") %>%
#   set_mode("regression")
# 
# forest_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(forest_mod)
# 
# tuning_grid <- grid_regular(mtry(range=c(1,10)), min_n(), levels=3)
# folds <- vfold_cv(train_data, v=3, repeats=1)
# 
# cv_results <- forest_wf %>%
#   tune_grid(resamples=folds, grid=tuning_grid, metrics=metric_set(mae))
# best_tune <- cv_results %>%
#   collect_metrics()
# 
# mean(best_tune$std_err)


# Boost ------------------------------------------------------------------------

# boost_mod <- boost_tree(tree_depth=tune(), trees=tune(), learn_rate=tune()) %>%
#   set_engine("lightgbm") %>%
#   set_mode("regression")
# 
# boost_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(boost_mod)
# 
# tuning_grid <- grid_regular(tree_depth(), trees(), learn_rate(), levels=3)
# folds <- vfold_cv(train_data, v=3, repeats=1)
# 
# cv_results <- boost_wf %>%
#   tune_grid(resamples=folds, grid=tuning_grid, metrics=metric_set(mae))
# best_tune <- cv_results %>%
#   collect_metrics()
# 
# mean(best_tune$std_err)


# Facebook Prophet -------------------------------------------------------------

# Need to have the column names right for prophet
prophet_data <- train_data %>%
  rename(y=Weekly_Sales, ds=Date)

# Filter and Rename to match prophet syntax
sd_train <- train_data %>%
  #filter(Store==store, Dept==dept) %>%
  rename(y=Weekly_Sales, ds=Date)
sd_test <- test_data %>%
  #filter(Store==store, Dept==dept) %>%
  rename(ds=Date)

## Fit a prophet model
prophet_model <- prophet() %>%
  add_regressor("CPI") %>%
  fit.prophet(df=sd_train)

# Predict Using Fitted prophet Model
fitted_vals <- predict(prophet_model, df=sd_train)   # For Plotting Fitted Values
test_preds <- predict(prophet_model, df=sd_test)     # Predictions are called "yhat"

## Plot Fitted and Forecast on Same Plot
plot2 <- ggplot() +
  geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="")






