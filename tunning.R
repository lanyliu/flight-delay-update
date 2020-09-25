setwd("~/Documents/flight-delay-update")
library(dplyr)
library(caret)
library(xgboost)
library(Metrics)


# load data set
load(file = "data_set_model.rda")

# prep training set to DMatrix
train_df <- ex_all_df %>% 
  filter(flight_date <= '2016-04-30' & flight_date >= '2015-05-01') %>%
  select(-flight_date) %>%
  sample_n(2000) #1000

train_y <- train_df$delay_time_num 
train <- as.matrix(train_df[, -c("delay_time_num")])
train_data <- xgb.DMatrix(train)

# prep testing set to DMatrix
test_df <- ex_all_df %>% 
  filter(flight_date >= '2016-05-01') %>%
  select(-flight_date) 

test_y <- test_df$delay_time_num 
test <- as.matrix(test_df[, -c("delay_time_num")])
test_data <- xgb.DMatrix(test)


# tuning
## XGBT
cv.ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3, #3
  number = 10) #folds 10

xgb.grid <- expand.grid(
  nrounds = 50,#4, #5
  max_depth = 1,#5, 
  eta = 0.3, #0.1944444 
  gamma = 0, #0.9, 
  colsample_bytree = 0.8, #0.84, #0.9, 
  min_child_weight = 1, #6, #7, 
  subsample = 1 #0.88 #1
)

set.seed(23)
system.time(
  xgbt_tune <- caret::train(
    train_data, train_y,
    method = "xgbTree",
    metric = "RMSE",
    trControl = cv.ctrl,
    tuneGrid = xgb.grid)
)

# plot var imp
caret_imp <- varImp(xgbt_tune, scale = TRUE)
plot(caret_imp)
ggplot(caret_imp) +
  theme_minimal()

xgb_imp <- xgb.importance(
  feature_names = xgbt_tune$finalModel$xNames,
  model = xgbt_tune$finalModel
)
xgb.plot.importance(xgb_imp)

# save model
save(xgbt_tune, file = 'xgbt tune.rda')



## SVM
svm.grid <- expand.grid(
  C = seq(0, 1, length = 3)) # seq(0, 2, length = 10)

set.seed(23)
system.time(
  svm_tune <- caret::train(
    delay_time_num ~ .,
    train_df,
    method = "svmLinear",
    metric = "RMSE",
    trControl = cv.ctrl,
    tuneGrid = svm.grid)
)

# var imp
caret_imp <- varImp(svm_tune, scale = TRUE)
caret_imp
plot(caret_imp)
ggplot(caret_imp) +
  theme_minimal()

# save model
save(svm_tune, file = 'svm tune.rda')



# prediction result
## XGBT
xgbt_test_result <- predict(xgbt_tune, newdata = test_data)

# validate delay_time_num
actual <- test_df$delay_time_num
predicted <- xgbt_test_result
mse(actual, predicted)^0.5
mae(actual, predicted)

# validate is_claim
actual <- if_else(na.omit(test_df)$delay_time_num > 3, 800, 0)
predicted <- if_else(xgbt_test_result > 3, 800, 0)
mse(actual, predicted)^0.5
mae(actual, predicted)

met <- data.frame(act = actual, pred = predicted)
table(met)


## SVM
svm_test_result <- predict(svm_tune, newdata = test_df) 

# validate delay_time_num
actual <- test_df$delay_time_num
predicted <- svm_test_result
mse(actual, predicted)^0.5
mae(actual, predicted)

# validate is_claim
actual <- if_else(na.omit(test_df)$delay_time_num > 3, 800, 0)
predicted <- if_else(svm_test_result > 3, 800, 0)
mse(actual, predicted)^0.5
mae(actual, predicted)

met <- data.frame(act = actual, pred = predicted)
table(met)