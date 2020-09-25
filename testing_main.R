setwd("~/Documents/flight-delay-update")
library(Metrics)


# load data set
load(file = "data_set_model.rda")

# load model
load(file = 'ori feaure model.rda')
load(file = 'time feaure model.rda')
load(file = 'flight feaure model.rda')
load(file = 'all feaure model.rda')

# model results
model_compare <- data.frame(
  LM = c(min(train_result[[1]]$results$RMSE), min(train_result[[1]]$results$MAE)),
  RF = c(min(train_result[[2]]$results$RMSE), min(train_result[[2]]$results$MAE)),
  XGBT = c(min(train_result[[3]]$results$RMSE), min(train_result[[3]]$results$MAE)),
  SVM = c(min(train_result[[4]]$results$RMSE), min(train_result[[4]]$results$MAE))
)
rownames(model_compare) <- c('RMSE', 'MAE')
model_compare
md = train_result[[4]]


# make prediction
# testing df
test_df <- ex_all_df %>% 
  filter(flight_date >= '2016-05-01') %>%
  arrange(flight_date) %>%
  select(-flight_date) 

# predict
test_result <- predict(md, newdata=na.omit(test_df))

# validation delay_time_num
actual <- na.omit(test_df)$delay_time_num
predicted <- test_result
mse(actual, predicted)^0.5
mae(actual, predicted)

# validation is_claim
actual <- if_else(na.omit(test_df)$delay_time_num > 3, 800, 0)
predicted <- if_else(test_result > 3, 800, 0)
mse(actual, predicted)^0.5
mae(actual, predicted)

met <- data.frame(act = actual, pred = predicted)
table(met)


