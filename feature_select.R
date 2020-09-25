setwd("~/Documents/flight-delay-update")
source('featuring.R')
source('utils.R')
library(dplyr)
library(Hmisc) # correlation
library(glmnet)
library(visNetwork)


# load data set
load(file = "data_set.rda")
# data set for correlation
cor_df <- ex_all_df %>% 
  filter(flight_date >= '2015-05-01') %>%
  select(-flight_date) %>%
  sample_n(2000) #1000


## correlation
cor <- rcorr(as.matrix(cor_df))
cor_df <- flattenCorrMatrix(cor$r, cor$P)
head(cor_df)

# high corr between x-x
high_cor <- cor_df %>% 
  filter(abs(cor) > 0.7) %>% 
  arrange(desc(cor))
high_cor

# plotting nodes and edges
node_list <- unique(c(as.vector(high_cor$row), as.vector(high_cor$column)))
nodes <- data.frame(
  id = seq(1:length(node_list)),
  label = node_list
)

edges <-  high_cor %>%
  left_join(nodes, by = c('row' = 'label')) %>%
  left_join(nodes, by = c('column' = 'label')) %>%
  select(from = id.x, to = id.y)

visNetwork(nodes, edges) %>% visLegend()

# exclude co-linear
excl <- c('Week', 'quarter', 'month',
          'day_of_wk__count_delay', 'day_of_wk__delay_avg', 
          'day_of_wk__delay_sd', 'day_of_wk__count_flights',
          'day_of_mo__count_delay', 'day_of_mo__delay_sd',
          'day_of_yr__delay_sd', 'day_of_yr__delay_avg',
          'day_of_yr__count_delay', 'prev_hr__delay_sd',
          
          'iso_country__delay_sd', 'iso_country__delay_q2',
          'iso_country__count_flights', 'iso_country__delay_avg',
          'iso_country__count_delay',
          'type__count_flights', 'type__count_delay',
          'type__delay_sd', 'type__delay_min', 'type__delay_avg',
          'Arrival__delay_sd', 'Arrival__delay_avg', 'Arrival__count_delay',
          'Airline__count_flights', 'Airline__delay_avg', 
          'Airline__delay_sd', 'Airline__delay_min',
          'prev_hr__delay_avg', 'prev_hr__delay_min')

train_df <- ex_all_df %>% 
  drop_features(excl) %>%
  filter(flight_date <= '2016-04-30' & flight_date >= '2015-05-01') %>%
  select(-flight_date) %>%
  sample_n(5000) #1000

train_y <- train_df$delay_time_num 
train <- as.matrix(train_df[, -c("delay_time_num")])


## lasso
set.seed(23)
cv_fit <- cv.glmnet(train,  train_y, alpha = 1, type.measure = "mse", nfolds = 10)
round(log(c(cv_fit$lambda.min, cv_fit$lambda.1se)), 2) # -2.87 -1.38
plot(cv_fit)

best_lambda_lasso <- cv_fit$lambda.min #lambda.1se
lasso_coef <- cv_fit$glmnet.fit$beta[,  cv_fit$glmnet.fit$lambda == best_lambda_lasso]

# fit with min mse lambda
fit <- glmnet(train, train_y, alpha = 1, type.measure = 'mse') #lasso
plot(fit, xvar="lambda", label=TRUE)
abline(v = log(best_lambda_lasso), col = "red", lty = "dashed")

# find coefficient and plot
coef_l <- data.table(lasso = lasso_coef)
coef_l[, feature := names(lasso_coef)]
to_plot_l <- melt(
  coef_l, 
  id.vars='feature',
  variable.name = 'model',
  value.name = 'coefficient')

ggplot(
  data = to_plot_l[coefficient != 0],
  aes(
    x = feature, 
    y = coefficient, 
    fill = model
  )) +
coord_flip() +
geom_bar(
  stat = 'identity', 
  fill = 'brown4'
  ) +
facet_wrap(~ model) + 
guides(
  fill=FALSE
  ) 


# featuring 0
# origin
ex_ori_df <- original_df %>% drop_features(excl)


# featuring 1
# extend time
# for model
ex_time_df <- ex_time_df %>% drop_features(excl)


# featuring 2
# extend airline
# for model
ex_flight_df <- ex_flight_df %>% drop_features(excl)


# featuring 3
# extend time & airline
# for model
ex_all_df <- ex_all_df %>% drop_features(excl)


# save data sets for modeling
save(original_df, ex_flight_df, ex_time_df, ex_all_df, ex_ori_df, file = "data_set_model.rda")
