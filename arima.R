library(tseries)
library(ggplot2)
library(forecast)
library(readxl)
library(dplyr)
library(magrittr)

df <- read_excel("C:/Users/rxu/Desktop/time series.xlsx")

daily_data_dyn = df %>% filter(StoreCountryCode == 'US' & BannerName == 'Dy') %>% select(New_Cust_Count, FirstTransactionDatetime)

df_grg = df %>% filter(StoreCountryCode == 'US' & BannerName == 'Ga') %>% select(New_Cust_Count, FirstTransactionDatetime)


##################################################################################################

daily_data_dyn$Date = as.Date(daily_data_dyn$FirstTransactionDatetime)

ggplot(daily_data_dyn, aes(Date, New_Cust_Count)) + geom_line() + scale_x_date('month')  + ylab("Daily") +
  xlab("")

 
count_ts = ts(daily_data_dyn[, c('New_Cust_Count')])

daily_data_dyn$clean_cnt = tsclean(count_ts)

ggplot() + 
  geom_line(data = daily_data_dyn, aes(x = FirstTransactionDatetime, y = clean_cnt)) + ylab('Cleaned Bicycle Count') 

############
daily_data_dyn$cnt_ma = ma(daily_data_dyn$clean_cnt, order=7) # using the clean count with no outliers
daily_data_dyn$cnt_ma30 = ma(daily_data_dyn$clean_cnt, order=30)


ggplot() + 
  geom_line(data = daily_data_dyn, aes(x = FirstTransactionDatetime, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data_dyn, aes(x = FirstTransactionDatetime, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data_dyn, aes(x = FirstTransactionDatetime, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count') 

#
a$cnt_ma = ma(a$clean_cnt, order=1) # using the clean count with no outliers

count_ma = ts(na.omit(a$cnt_ma), frequency=1)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)



###########################

ts_df <- function(country, banner, df){
  a = df %>% filter(StoreCountryCode == country & BannerName == banner)%>% select(New_Cust_Count, FirstTransactionDatetime)
  
  a$Date = as.Date(a$FirstTransactionDatetime)
  
  count_ts = ts(a[, c('New_Cust_Count')])
  
  a$clean_cnt = tsclean(count_ts)
  
  a$cnt_ma = ma(a$clean_cnt, order=1) # using the clean count with no outliers
  #
  count_ma = ts(na.omit(a$cnt_ma), frequency=4)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  #
  count_d1 = diff(deseasonal_cnt, differences = 1)
  #
  auto.arima(deseasonal_cnt, seasonal=FALSE)
  #
  fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
  #forcast
  fcast <- forecast(fit, h=30)
  #plot(fcast)

  return(fcast)
}

ts_df('US','Dy', df)
