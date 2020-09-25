setwd("~/Documents/flight-delay-update")
source('utils.R')
source('featuring.R')
library(data.table)
library(dplyr)


# read data
raw_df <- fread("./data/flight_delays_data.csv")
str(raw_df)
table(raw_df$is_claim)/length(raw_df$is_claim)

# data type & cleaning
delay <- raw_df %>%
  select(delay_time) %>%
  filter(delay_time != 'Cancelled') %>%
  mutate(delay_time = as.num(delay_time)) %>%
  filter(delay_time > 3) 
summary(delay)

original_df <- raw_df %>%
  mutate(
    # date type
    flight_date = as.Date(flight_date),
    
    # y
    delay_time_num = case_when(
      delay_time == 'Cancelled' ~ delay[sample(1:nrow(delay), 1), ]$delay_time, #99.0,
      TRUE ~ as.num(delay_time, na.strings = 'Cancelled'))
  ) %>%
  filter(Airline != 'NULL') %>%
  select(-Departure, -delay_time)


# featuring 0
# origin
ex_ori_df <- original_df 


# featuring 1
# extend time
ex_time_df <- add_time_feauture(original_df)

# plot time features
plot_df = ex_time_df %>% sample_n(500)
feature_plot(plot_df, plot_df$quarter)
feature_plot(plot_df, plot_df$month)
feature_plot(plot_df, plot_df$Week)
feature_plot(plot_df, plot_df$day_of_mo)
feature_plot(plot_df, plot_df$day_of_wk)
feature_plot(plot_df, plot_df$day_of_yr)


# featuring 2
# extend airline
ex_flight_df <- add_flight_feauture(original_df) 

# plot airline features
plot_df = ex_flight_df %>% sample_n(1000)
feature_plot(plot_df, plot_df$Airline)
feature_plot(plot_df, plot_df$Arrival)
feature_plot(plot_df, plot_df$type)
feature_plot(plot_df, plot_df$iso_country)


# featuring 3
# extend time & airline
ex_all_df <- original_df %>%
  add_time_feauture() %>%
  add_flight_feauture()


# save/ load featuring result
# different combination of data sets for modeling
save(original_df, ex_flight_df, ex_time_df, ex_all_df, ex_ori_df, file = "data_set.rda")
