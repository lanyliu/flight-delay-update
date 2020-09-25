setwd("~/Documents/flight-delay-update")
library(dplyr)
library(caret)
library(xgboost)


# load data set
load(file = "data_set_model.rda")

# data set splitting
# non: 2013-09-01 ~ 2015-04-30
# training: 2015-05-01 ~ 2016-04-30
# testing:	2016-05-01 ~ 2016-07-31
train_df <- ex_all_df %>%
  filter(flight_date <= '2016-04-30' & flight_date >= '2015-05-01') %>%
  select(-flight_date) %>%
  sample_n(2000)  # 1000 / 2000 4.5hr

train_y <- train_df$delay_time_num 
train <- as.matrix(train_df[, -c("delay_time_num")])
train <- as(train, "sparseMatrix")

train_data <- xgb.DMatrix(
  data = train, 
  label = train_y
) 


# training
# method list: 
#  https://rdrr.io/cran/caret/man/models.html
#  http://topepo.github.io/caret/available-models.html
ctrl <- trainControl(
  method = "cv", 
  number = 10,
  savePredictions = 'final',
  allowParallel = TRUE,
  classProbs =  TRUE)

start_time <- Sys.time()
set.seed(23)
train_result <- lapply(
  c('lm', 'rf', 'xgbTree', 'svmLinear'),  
  function (m) {
    train(
      delay_time_num ~ ., 
      method = m, 
      data = train_df,
      metric = 'RMSE',
      trControl = ctrl,
      na.action = na.omit,
      verbose= FALSE,
      importance = TRUE)
  })
end_time <- Sys.time()
cat(end_time - start_time)


# model results
model_compare <- data.frame(
  LM = c(min(train_result[[1]]$results$RMSE), min(train_result[[1]]$results$MAE)),
  RF = c(min(train_result[[2]]$results$RMSE), min(train_result[[2]]$results$MAE)),
  XGBT = c(min(train_result[[3]]$results$RMSE), min(train_result[[3]]$results$MAE)),
  SVM = c(min(train_result[[4]]$results$RMSE), min(train_result[[4]]$results$MAE))
)
rownames(model_compare) <- c('RMSE', 'MAE')
model_compare


# plot var imp
m = train_result[[4]]
varImp(m, scale = TRUE)
ggplot(
  varImp(m, scale = TRUE),
  top = 20
)


# save model
md = m
save(md, train_result, file = 'ori feaure model.rda')
save(md, train_result, file = 'time feaure model.rda')
save(md, train_result, file = 'flight feaure model.rda')
save(md, train_result, file = 'all feaure model.rda')


