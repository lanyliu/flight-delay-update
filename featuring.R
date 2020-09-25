setwd("~/Documents/flight-delay-update")
library(dplyr)
library(geosphere)
library(RQuantLib)


feature_state <- function(df, col) {
  out <- df %>% 
    group_by_at(vars(all_of(col))) %>% 
    summarise(
      count_flights = n(),
      count_delay = length(delay_time_num[is_claim > 0]),
      perc_flights_delay = count_delay/count_flights,
      # delay_q1 = unname(quantile(delay_time_num, 0.25)),
      delay_q2 = unname(quantile(delay_time_num, 0.5)),
      # delay_q3 = unname(quantile(delay_time_num, 0.75)),
      delay_avg = mean(delay_time_num),
      delay_sd = sd(delay_time_num),
      delay_min = min(delay_time_num)
      # delay_max = max(delay_time_num)
    ) %>%
    arrange(desc(delay_q2)) %>%
    # Standardize
    mutate_at(c("count_flights", "count_delay"), ~(scale(.) %>% as.vector))
  
  colnames(out)[-1] = paste(col, colnames(out)[-1], sep='__')
  print(out)
  
  out
}


extend_time <- function(df) {
  # extend time related features
  out <- df %>%
    mutate(
      quarter = quarter(flight_date),
      month = month(flight_date),
      day_of_mo = mday(flight_date),
      day_of_wk = wday(flight_date),
      day_of_yr = yday(flight_date),
      date_time = paste(flight_date, sprintf("%02d", std_hour), sep=' ')
    )
  
  out
}


extend_calender <- function(df) {
  # top countries: CN, TW, JP, TH, AU, KR, SG, US
  # extend calendar related features
  out <- df %>%
    mutate(
      is_hk_holiday = as.integer(isHoliday("HongKong", flight_date)),
      is_cn_holiday = as.integer(isHoliday("China", flight_date)),
      is_tw_holiday = as.integer(isHoliday("Taiwan", flight_date)),
      is_jp_holiday = as.integer(isHoliday("Japan", flight_date)),
      # is_th_holiday = as.integer(isHoliday("", flight_date)),
      is_au_holiday = as.integer(isHoliday("Australia", flight_date)),
      is_kr_holiday = as.integer(isHoliday("SouthKorea", flight_date)),
      is_sg_holiday = as.integer(isHoliday("Singapore", flight_date)),
      is_us_holiday = as.integer(isHoliday("UnitedStates", flight_date))
    )
  
  out
}


get_time_state <- function(df) {
  # list of data frames to return
  out <- list()
  
  # statistics for each features
  # out$f_quarter <- feature_state(df, 'quarter') 
  # out$f_month <- feature_state(df, 'month')
  out$f_day_of_mo <- feature_state(df, 'day_of_mo')
  out$f_day_of_wk <- feature_state(df, 'day_of_wk')
  out$f_day_of_yr <- feature_state(df, 'day_of_yr')
  
  # states at previous hr
  out$f_date_time <- feature_state(df, 'date_time') %>%
    arrange(date_time) %>%
    mutate(
      prev_hr__count_flights = lag(date_time__count_flights),
      prev_hr__count_delay = lag(date_time__count_delay),
      prev_hr__perc_flights_delay = lag(date_time__perc_flights_delay),
      # prev_hr__delay_q1 = lag(date_time__delay_q1),
      prev_hr__delay_q2 = lag(date_time__delay_q2),
      # prev_hr__delay_q3 = lag(date_time__delay_q3),
      prev_hr__delay_avg = lag(date_time__delay_avg),
      prev_hr__delay_sd = lag(date_time__delay_sd),
      prev_hr__delay_min = lag(date_time__delay_min)
      # prev_hr__delay_max = lag(date_time__delay_max)
    ) %>%
    mutate_at(
      vars(starts_with("prev_hr__")),
      funs(if_else( is.na(.), 0, .))
    ) %>%
    select(-starts_with("date_time__"))
  
  out
}


add_time_feauture <- function(df) {
  ex_calender_df <- extend_calender(df) #new
  ex_time_df <- extend_time(ex_calender_df)
  time_state <- get_time_state(ex_time_df)
  ex_time_df <- ex_time_df %>%
    # left_join(time_state$f_quarter, by = c("quarter")) %>%
    # left_join(time_state$f_month, by = c("month")) %>%
    left_join(time_state$f_day_of_mo, by = c("day_of_mo")) %>%
    left_join(time_state$f_day_of_wk, by = c("day_of_wk")) %>%
    left_join(time_state$f_day_of_yr, by = c("day_of_yr")) %>%
    left_join(time_state$f_date_time, by = c("date_time")) 
  ex_time_df
}


extend_flight <- function(df) {
  # extend flight related features
  # read external data
  # airports: https://github.com/epranka/airports-db
  airports_raw <- fread("./data/airports-db/raw/airports.csv") 
  hk_longitude <- 113.915
  hk_latitude <- 22.3089
  airports <- airports_raw %>%
    filter(!iata_code %in% c('','-','0') & type != 'closed') %>%
    rowwise() %>%
    mutate(
      distance_to_hk = distm(c(hk_longitude, hk_latitude), c(longitude_deg, latitude_deg), fun = distHaversine)[,1]
    ) %>%
    select(iata_code, type, latitude_deg, longitude_deg, iso_country, distance_to_hk) %>%
    # dedup
    group_by(iata_code) %>% 
    dplyr::slice(which.max(distance_to_hk))
  
  out <- df %>%
    left_join(airports, by = c("Arrival" = "iata_code")) %>%
    
    # Standardize
    mutate_at(c("distance_to_hk"), ~(scale(.) %>% as.vector)) %>%
    select(-latitude_deg, -longitude_deg)
  
  out
}


get_flight_state <- function(df) {
  # list of data frames to return
  out <- list()
  
  # statistics for each features
  out$f_airline <- feature_state(df, 'Airline') 
  out$f_arrival <- feature_state(df, 'Arrival') 
  out$f_type <- feature_state(df, 'type') 
  out$f_iso_country <- feature_state(df, 'iso_country') 
  
  out
}


add_flight_feauture <- function(df) {
  ex_flight_df <- extend_flight(df)
  flight_state <- get_flight_state(ex_flight_df)
  ex_flight_df <- ex_flight_df %>%
    left_join(flight_state$f_airline, by = c("Airline")) %>%
    left_join(flight_state$f_arrival, by = c("Arrival")) %>%
    left_join(flight_state$f_type, by = c("type")) %>%
    left_join(flight_state$f_iso_country, by = c("iso_country")) %>%
    mutate(
      is_large_airport = ifelse(type == 'large_airport', 1, 0)
    )
  
  # one-hot
  cat('one-hot encoding...')
  arrival <- model.matrix(~Arrival-1, ex_flight_df)
  airline <- model.matrix(~Airline-1, ex_flight_df)
  country <- model.matrix(~iso_country-1, ex_flight_df)
  
  ex_flight_df <- cbind(ex_flight_df, arrival, airline, country)
  ex_flight_df
}


drop_features <- function(df, ext = c()) {
  excl <- c('flight_id', 'flight_no', 'is_claim', 'date_time',
            'iso_country', 'type', 'Airline', 'Arrival', 
            ext)
  out <- df %>% select(-one_of(excl))
  out
}
