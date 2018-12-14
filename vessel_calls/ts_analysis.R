library("readxl")
library("grid")
library("ggplot2")
library("TTR")
library("forecast")
library("lattice")

##see here: http://r-statistics.co/Time-Series-Analysis-With-R.html
##see here: https://github.com/ByronKKing/Time-Series-R/blob/master/Project1/project.r1.R

setwd("~/Desktop/vessel_calls/vessel_calls_data/")
#setwd("~/Downloads/vessel_calls/vessel_calls_data/")

multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  plots = c(list(...), plotlist)
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout = matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



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

tsData = ts(df[,c("calls","capacity_dwt","capacity_call")], start=c(2002), end=c(2015), frequency=1)

autoplot(tsData[,2]) +
  xlab("Year") +
  ylab("Capacity") +
  ggtitle("Vessel Capacity by Year") +
  theme(plot.title = element_text(hjust = 0.5))

p1 = autoplot(acf(tsData[,1],plot = FALSE)) +
  ggtitle("Series ACF") +
  theme(plot.title = element_text(hjust = 0.5))

p2 = autoplot(pacf(tsData[,1],plot = FALSE)) +
  ggtitle("Series PACF") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,cols=1)

p1 = autoplot(acf(tsData[,2],plot = FALSE)) +
  ggtitle("Series ACF") +
  theme(plot.title = element_text(hjust = 0.5))
p2 = autoplot(pacf(tsData[,2],plot = FALSE)) +
  ggtitle("Series PACF") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,cols=1)

p1 = autoplot(acf(tsData[,3],plot = FALSE)) +
  ggtitle("Series ACF") +
  theme(plot.title = element_text(hjust = 0.5))
p2 = autoplot(pacf(tsData[,3],plot = FALSE)) +
  ggtitle("Series PACF") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,cols=1)


## iterate over SMA for different n (order) and calculate MSE, this removes trend
###see this:https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

mse_df = NULL
for(n in 2:6){
  ts_sma = SMA(tsData[,2],n=n)
  mse = mean(((tsData[,2]-ts_sma)^2),na.rm = TRUE)
  mse_df = rbind(mse_df,data.frame(n,mse))
}

mse_df$n[mse_df$mse == min(mse_df$mse)]
ts_sma = SMA(tsData[,2],n=mse_df$n[mse_df$mse == min(mse_df$mse)])

p1 = autoplot(tsData[,2]) +
  xlab("Year") +
  ylab("Capacity") +
  ggtitle("Vessel Capacity by Year") +
  theme(plot.title = element_text(hjust = 0.5))
p2 = autoplot(ts_sma) +
  xlab("Year") +
  ylab("Capacity") +
  ggtitle("Simple Moving Average") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,cols=1)


##differencing series does not make it stationary in mean or variance

p1 = autoplot(diff(tsData[,2],1)) +
  xlab("Year") +
  ylab("Capacity") +
  ggtitle("Series Differenced Once") +
  theme(plot.title = element_text(hjust = 0.5))

p2 = autoplot(diff(tsData[,2],3)) +
  xlab("Year") +
  ylab("Capacity") +
  ggtitle("Series Differenced Three Times") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,cols=1)

##simple exponential smoothing (holt winters)
ts_hw = HoltWinters(tsData[,2],beta = FALSE,gamma = FALSE,l.start = tsData[1,2])
ts_hw
ts_hw$fitted

forecast_hw = forecast:::forecast.HoltWinters(ts_hw, h=3)
autoplot(forecast_hw) +
  xlab("Year") +
  ylab("Capacity") +
  ggtitle("Holt Winters Forecasts") +
  theme(plot.title = element_text(hjust = 0.5))

###this is weird
forecast_hw = predict(ts_hw, n.ahead = 3, prediction.interval = TRUE, level = 0.95)
plot(forecast_hw, forecast)

###conclusion: HW is bad!?
p1 = autoplot(acf(forecast_hw$residuals[!is.na(forecast_hw$residuals)], lag.max=10),
              plot = TRUE) +
  ggtitle("Holt Winters ACF Residuals") +
  theme(plot.title = element_text(hjust = 0.5))
p2 = autoplot(ts(forecast_hw$residuals[!is.na(forecast_hw$residuals)])) +
  xlab("Year") +
  ylab("Residuals") +
  ggtitle("Holt Winters Forecast Residuals") +
  theme(plot.title = element_text(hjust = 0.5))
plotdf = as.data.frame(forecast_hw$residuals[!is.na(forecast_hw$residuals)])
colnames(plotdf)[1] = "residuals"
p3 = ggplot(data = plotdf,aes(residuals)) +
  geom_histogram(bins = 7) +
  xlab("Residual Bucket") +
  ylab("Frequency") +
  ggtitle("Holt Winters Forecast Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2,p3,cols = 3)


##ARIMA model (loop over and find best parameters)
###see this: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

###create matrix to store AIC of ARIMA models
max.order = 10
AIC.matrix = list()

###set the maximum 'd' differences of the time series x to be 3
max.d = 3

###create a loop that fits all 363 models and returns a new matrix for each integrated component 'd'
###this loop takes time series x, extracts the AIC value, and stores the value in a matrix (returns 4 in total)
for(d in 0:max.d){
  AIC.temp.matrix = matrix(0,nrow = max.order+1,ncol= max.order+1)
  for(i in 1:(max.order+1)){
    for(j in 1:(max.order+1)){
      AIC.temp.matrix[i,j] = tryCatch(
        {
          currentArima = arima(tsData[,1],order=c(i-1,d,j-1))
          AIC(currentArima)
        },
        error=function(cond){
          errMessage = paste(i-1,d,j-1,sep=",")
          errMessage = paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        },
        warning=function(cond){
          errMessage = paste(i-1,d,j-1,sep=",")
          errMessage = paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        })
    }
  }
  AIC.matrix[[d+1]] = AIC.temp.matrix
}

findMin = function(matrix){
  matrixIndex = which(matrix == min(matrix), arr.ind = TRUE)
  minAIC = min(matrix)
  print(matrixIndex)
  print(minAIC)
}

findMin(AIC.matrix[[1]])
findMin(AIC.matrix[[2]])
findMin(AIC.matrix[[3]])
findMin(AIC.matrix[[4]])

plot.new()
par(mfrow=c(2,2), oma=c(2,0,2,0))
print(levelplot(AIC.matrix[[1]],main="0 Differenced"), split=c(1, 1, 2, 2)) 
print(levelplot(AIC.matrix[[2]],main="1 Differenced"), split=c(1, 2, 2, 2), newpage=FALSE)
print(levelplot(AIC.matrix[[3]],main="2 Differenced"), split=c(2, 1, 2, 2), newpage=FALSE)
print(levelplot(AIC.matrix[[4]],main="3 Differenced"), split=c(2, 2, 2, 2), newpage=FALSE)
title("Centered Overall Title", outer=TRUE)
mtext(side=1, "Centered Subtitle", outer=TRUE)

###print acf and pacf of the best model with the lowest AIC --this is where I left off
best.model = arima(tsData[,1],order=c(2,4,2))
par(mfrow=c(1, 2))
acf(best.model$resid)
pacf(best.model$resid)

tsdisplay(residuals(best.model), lag.max=5, main='(1,1,1) Model Residuals')

fcast = forecast(best.model, h=10)
plot(fcast)


##NNETAR model
###see this: https://stats.stackexchange.com/questions/313927/time-series-prediction-neural-network-nnetar-vs-exponential-smoothing-ets/313934

model.nn = nnetar(tsData[,1],p = 1)

fcast = forecast(model.nn,h=5)
plot(fcast)

###compare these predictions by MSE!
mean(((best.model$residuals)^2),na.rm = TRUE)
mean(((model.nn$residuals)^2),na.rm = TRUE)

