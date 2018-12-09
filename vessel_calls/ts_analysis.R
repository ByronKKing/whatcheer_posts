library("readxl")
#library("plotly")
library("ggplot2")
library("TTR")
library("forecast")

##see here: http://r-statistics.co/Time-Series-Analysis-With-R.html
##see here: https://github.com/ByronKKing/Time-Series-R/blob/master/Project1/project.r1.R

setwd("~/Desktop/vessel_calls/vessel_calls_data/")
#setwd("~/Downloads/vessel_calls/vessel_calls_data/")

# Process data

append_df = NULL
year = 2012
for(i in 2:12){
  
  df = read_excel("dsusportcalls2002-2012.xls", i)
  colnames(df) = df[3,]
  df = df[4:nrow(df),]
  df = df[,1:4]
  df$year = year
  
  append_df = rbind(append_df,df)
  year = year-1
  
}

append_df2 = NULL
year = 2013

for(i in 1:3){
  
  df = read_excel(paste("dsvesselcalls",as.character(year),".xlsx",sep = ""),1)
  colnames(df) = df[5,]
  df = df[6:nrow(df),]
  df = df[,1:4]
  df$year = year
  df$state = trimws(sapply(strsplit(df$Port,",",fixed = FALSE), "[", 2)) 
  df$Port = sapply(strsplit(df$Port,",",fixed = FALSE), "[", 1)
  
  append_df2 = rbind(append_df2,df)
  year = year+1
  
}

colnames(append_df) = c("port","state","calls","capacity_dwt","year")
append_df$capacity_gt = NA

colnames(append_df2) = c("port","calls","capacity_gt","capacity_dwt","year","state")
append_df2 = append_df2[,c("port","state","calls","capacity_dwt","year","capacity_gt")]

finaldf = rbind(append_df,append_df2)


# Process series for overall dwt/calls

df = finaldf[finaldf$port=="Grand Total",c("year","calls","capacity_dwt")]
df = df[!(rowSums(is.na(df))==ncol(df)),]

for(i in 2012:2002){
  df = rbind(df,
             c(i,
               sum(as.numeric(append_df$calls[append_df$year==as.character(i)]),na.rm = TRUE),
               sum(as.numeric(append_df$capacity_dwt[append_df$year==as.character(i)]),na.rm = TRUE)))
}

df = rbind(df,
           c(2012,
             sum(as.numeric(append_df$calls[append_df$year=="2012"])),
             sum(as.numeric(append_df$capacity_dwt[append_df$year=="2012"]))))

df$calls = as.numeric(df$calls)
df$capacity_dwt = as.numeric(df$capacity_dwt)
df$year = as.numeric(df$year)
df = df[order(df$year),]
df$capacity_call = df$capacity_dwt/df$calls


# Time series plots and stats

##basic plots

###use this: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
p = plot_ly(df, x = ~year, y = ~calls,
            type = 'scatter', mode = 'lines+markers') 
p

p = plot_ly(df, x = ~year, y = ~capacity_dwt,
            type = 'scatter', mode = 'lines+markers') 
p

p = plot_ly(df, x = ~year, y = ~capacity_call,
            type = 'scatter', mode = 'lines+markers') 
p

tsData = ts(df[,c("calls","capacity_dwt","capacity_call")], start=c(2002), end=c(2015), frequency=1)

plot.ts(tsData[,2])

## acf and pcf for different variables

acf(tsData[,1])
pacf(tsData[,1])
acf(tsData[,2])
pacf(tsData[,2])
acf(tsData[,3])
pacf(tsData[,3])

## iterate over SMA for different n (order) and calculate MSE, this removes trend
###see this:https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

mean(((tsData[,2]-ts_sma)^2),na.rm = TRUE)

mse_df = NULL
for(n in 2:6){
  ts_sma = SMA(tsData[,2],n=n)
  mse = mean(((tsData[,2]-ts_sma)^2),na.rm = TRUE)
  mse_df = rbind(mse_df,data.frame(n,mse))
}

ts_sma = SMA(tsData[,2],n=mse_df$n[mse_df$mse == min(mse_df$mse)])
plot.ts(ts_sma)
plot.ts(tsData[,2])
plot.ts(diff(tsData[,2],3))

##simple exponential smoothing (holt winters)
ts_hw = HoltWinters(tsData[,2],beta = FALSE,gamma = FALSE,l.start = tsData[1,2])
ts_hw
ts_hw$fitted
plot(ts_hw)

forecast_hw = predict(ts_hw, n.ahead = 3, prediction.interval = T, level = 0.95)
plot(forecast_hw, forecast)

forecast_hw = forecast:::forecast.HoltWinters(ts_hw, h=3)
forecast:::plot.forecast(forecast_hw)

acf(forecast_hw$residuals[!is.na(forecast_hw$residuals)], lag.max=10)
plot.ts(forecast_hw$residuals[!is.na(forecast_hw$residuals)])
hist(forecast_hw$residuals[!is.na(forecast_hw$residuals)])

##ARIMA model (loop over and find best parameters)

##NNETAR model
###see this: https://stats.stackexchange.com/questions/313927/time-series-prediction-neural-network-nnetar-vs-exponential-smoothing-ets/313934

###compare these predictions by MSE!




