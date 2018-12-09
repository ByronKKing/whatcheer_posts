library("plotly")
library("htmlwidgets")
library("widgetframe")

saveHtmlWidget = function(plotObj,name){
  frameWidget(plotObj, height = '300')
  htmlwidgets::saveWidget(plotObj, 
                          file = paste("~/Desktop/gdp/plots/",name,".html",sep = ""), 
                          selfcontained = TRUE)
}

# 2017 US GDP
startGdp = 18050.7
endGdp = 18050.7*3

# Create function to calculate growth year by year
calculateGrowth = function(startGdp,endGdp,growthRate,varName){
  
  df = NULL
  
  currGdp = startGdp
  
  currYear = 1
  
  while(currGdp<endGdp){
    
    df = rbind(df,data.frame(currGdp,currYear,varName))
    
    currGdp = currGdp + (currGdp*growthRate)
    
    currYear = currYear+1
    
  }
  
  return(df)
  
}

# Create dataframes
growthRate = .05
varName = "5% Growth Rate"
fiveperc = calculateGrowth(startGdp,endGdp,growthRate,varName)

growthRate = .02
varName = "2% Growth Rate"
threeperc = calculateGrowth(startGdp,endGdp,growthRate,varName)

growthRate = .01
varName = "1% Growth Rate"
oneperc = calculateGrowth(startGdp,endGdp,growthRate,varName)

# Plot rates
plotdf = merge(oneperc,threeperc,by = "currYear",all.x = TRUE)
plotdf = merge(plotdf,fiveperc,by = "currYear",all.x = TRUE)

p = plot_ly(plotdf, x = ~currYear, y = ~currGdp.x, 
            name = ~varName.x, type = 'scatter', mode = 'markers',
            text = ~paste("Number of Years: ", currYear)) %>%
  add_trace(y = ~currGdp.y, name = ~varName.y, type = 'scatter',mode = 'markers') %>%
  add_trace(y = ~currGdp, name = ~varName, type = 'scatter',mode = 'markers') %>%
  layout(title = "GDP Growth Rates",
         xaxis = list(title = "Year"),
         yaxis = list(title = "GDP (trillions of dollars)"))


saveHtmlWidget(p,"hypothetical_growth_rates")













# 
# ####Notes
# The nation's gross domestic product totals trillions of dollars. Most often, the number you’ll hear people refer to as "GDP" is a percentage. That's the rate of change in real GDP from the previous quarter or year. "Real" or "chained" GDP numbers have been adjusted to remove the effects of inflation over time, so different periods can be compared.
# 
# "Current-dollar" or "nominal" GDP estimates are based on market prices during the period being measured.
# 
# This table shows percent change in GDP dating back to 1930.
# 
# This table shows dollar amounts for GDP dating back to 1929.
# #https://www.bea.gov/resources/learning-center/what-to-know-gdp
# 
# #GDP in billions of chained 2012 dollars
# 
# 
