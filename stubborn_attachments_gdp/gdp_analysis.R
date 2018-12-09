library("reshape2")
library("plotly")
library("xlsx")
library("htmlwidgets")
library("widgetframe")

saveHtmlWidget = function(plotObj,name){
  frameWidget(plotObj, height = '300')
  htmlwidgets::saveWidget(plotObj, 
                          file = paste("~/Desktop/gdp/plots/",name,".html",sep = ""), 
                          selfcontained = TRUE)
}

# Process gdp data

df = read.csv("~/Desktop/gdp/gdp_current_us_dollars.csv",stringsAsFactors = FALSE)

colnames(df) = df[4,]
df = df[-c(1:4),]

df = df[,c(1,5:length(df))]

df = melt(df,id = "Country Name")
df = df[complete.cases(df),]
colnames(df) = c("country","year","gdp")

df$diff = ave(df$gdp, factor(df$country), FUN=function(x) c(NA,diff(x)))

df = df[order(df$country,df$year),]
df$lag_value = c(NA, df$gdp[-nrow(df)])
df$lag_value[which(!duplicated(df$country))] = NA
df$perc_change = df$diff/df$lag_value
df$perc_change = df$perc_change*100

df$year = as.numeric(as.character(df$year))

## Absolute GDP

countrySelect = df[df$country %in% c('United Kingdom','India','Japan','United States',
                                     'Germany','Ukraine','Poland','Korea, Rep.','China'),]

p = plot_ly(countrySelect, x = ~year, y = ~gdp, name = ~country, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "GDP Growth Rates",
         xaxis = list(title = "Year"),
         yaxis = list(title = "GDP"))

p

## High Growth Nations

colorPal = c("thistle", "sky blue", "light green","red")

countrySelect = df[df$country %in% c('United States','China','India','Ethiopia') & df$year > 2000,]

p = plot_ly(countrySelect, x = ~year, y = ~perc_change, name = ~country, color = ~country,
            type = 'scatter', mode = 'lines+markers',colors = colorPal) %>%
  layout(title = "GDP Growth Rates: High Growth Nations",
         xaxis = list(title = "Year"),
         yaxis = list(title = "% Change in GDP"))

saveHtmlWidget(p,"high_growth_rates")

## Low Growth Nations

colorPal = c("thistle", "sky blue", "light green","red")

countrySelect = df[df$country %in% c('United States','United Kingdom','Japan','France') & df$year > 2000,]

p = plot_ly(countrySelect, x = ~year, y = ~perc_change, name = ~country, color = ~country,
            type = 'scatter', mode = 'lines+markers',colors = colorPal) %>%
  layout(title = "GDP Growth Rates: Low Growth Nations",
         xaxis = list(title = "Year"),
         yaxis = list(title = "% Change in GDP"))

saveHtmlWidget(p,"low_growth_rates")

## Asian Miracles

countrySelect = df[df$country %in% c('Hong Kong SAR, China','Singapore','Japan','Korea, Rep.') 
                   & df$year >= 1961 & df$year < 2000,]

p = plot_ly(countrySelect, x = ~year, y = ~perc_change, name = ~country, color = ~country,
            type = 'scatter', mode = 'lines+markers') %>%
  add_lines(y = ~fitted(loess(countrySelect$perc_change ~ as.numeric(as.character(countrySelect$year)))),
            line = list(color = 'red'),
            name = "Growth Trend", showlegend = FALSE) %>%
  layout(title = "GDP Growth Rates: Asian Miracle Nations",
         xaxis = list(title = "Year"),
         yaxis = list(title = "% Change in GDP")) 

saveHtmlWidget(p,"asian_miracles")



## US GDP

calculatePercGrowth = function(gdp,rate){
  
  perc_vector = c(gdp)
  i = 1
  
  while(i<89){
    
    gdp = gdp+gdp*rate
    i = i+1
    perc_vector = c(perc_vector,gdp)
    
  }
  
  return(perc_vector)
  
}


usgdp = read.xlsx("~/Desktop/gdp/gdplev.xlsx", sheetName = "Sheet1")

###processing
usgdp = usgdp[,1:3]
usgdp = usgdp[-c(1:3),]
colnames(usgdp) = c("year","gdp_curr","gdp_2012")
usgdp = usgdp[complete.cases(usgdp),]
usgdp$gdp_curr = as.numeric(as.character(usgdp$gdp_curr))
usgdp$gdp_2012 = as.numeric(as.character(usgdp$gdp_2012))

###create time series variables
(usgdp$gdp_2012[usgdp$year == 2017]-usgdp$gdp_2012[usgdp$year == 1929])/(2017-1929)
usgdp = usgdp[order(usgdp$year),]
usgdp$lag_value = c(NA, usgdp$gdp_2012[-nrow(usgdp)])
usgdp$diff = usgdp$gdp_2012 - usgdp$lag_value
usgdp$perc_change = usgdp$diff/usgdp$lag_value
usgdp$perc_change = round(usgdp$perc_change*100,2)

###calculate different growth rates
usgdp$one_perc = calculatePercGrowth(usgdp$gdp_2012[usgdp$year == 1929],.01)
usgdp$two_perc = calculatePercGrowth(usgdp$gdp_2012[usgdp$year == 1929],.02)
usgdp$three_perc = calculatePercGrowth(usgdp$gdp_2012[usgdp$year == 1929],.03)
usgdp$four_perc = calculatePercGrowth(usgdp$gdp_2012[usgdp$year == 1929],.04)
usgdp$five_perc = calculatePercGrowth(usgdp$gdp_2012[usgdp$year == 1929],.05)


## Actual Changes in GDP
p = plot_ly(usgdp, x = ~year, y = ~perc_change, name = "% Change",
            type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "US GDP Historical Growth Rates",
         xaxis = list(title = "Year"),
         yaxis = list(title = "% Change in GDP")) %>%
  add_lines(y = ~c(0,fitted(loess(usgdp$perc_change ~ as.numeric(as.character(usgdp$year))))),
            line = list(color = 'red'),
            name = "Growth Trend", showlegend = TRUE)

saveHtmlWidget(p,"us_gdp_growth")

## Hypothetical growth rates
p = plot_ly(usgdp, x = ~year, y = ~gdp_2012, name = "Current Growth",
            type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~one_perc, name = '1% Growth',mode = 'lines') %>%
  add_trace(y = ~two_perc, name = '2% Growth',mode = 'lines') %>%
  add_trace(y = ~three_perc, name = '3% Growth',mode = 'lines') %>%
  add_trace(y = ~four_perc, name = '4% Growth',mode = 'lines') %>%
  add_trace(y = ~five_perc, name = '5% Growth',mode = 'lines') %>%
  layout(title = "Hypothetical GDP Growth Rates",
         xaxis = list(title = "Year"),
         yaxis = list(title = "GDP (in trillions of chained 2012 dollars)"))

saveHtmlWidget(p,"us_hypothetical_growth")






