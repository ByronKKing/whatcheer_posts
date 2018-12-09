library("readxl")
library("plotly")
library("ggplot2")

###RI + MA ports
###composition of RI ports (by vessel type)
###top ports

###time series analysis of one (trend, predictions, HoltWinters)


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



# Filter by states

df = finaldf[finaldf$state%in%c("RI","MA"),]
df = df[!(rowSums(is.na(df))==ncol(df)),]

df$calls = as.numeric(df$calls)
df = df[order(df$year),]

p = plot_ly(df, x = ~year, y = ~calls, name = ~port, color = ~port,
            type = 'scatter', mode = 'lines+markers') 
p


# Filter by ports
df = finaldf[finaldf$port=="Davisville",]
df = df[!(rowSums(is.na(df))==ncol(df)),]
df$calls = as.numeric(df$calls)
df = df[order(df$year),]

ggplot(data=df, aes(x=year, y=calls, group=1)) +
  geom_line()+
  geom_point()

p = plot_ly(df, x = ~year, y = ~calls,
            type = 'scatter', mode = 'lines+markers') 
p


#Time series for Davisville
df = finaldf[finaldf$port=="Davisville",]
df = df[!(rowSums(is.na(df))==ncol(df)),]
df$calls = as.numeric(df$calls)
df = df[order(df$year),]

tsData = ts(df[,c("year","calls")], start=c(2002), end=c(2015), frequency=1) 
decomposedRes = decompose(tsData[,2], type="mult")
acf(tsData[,2])
pacf(tsData[,2])


#Time series for overall dwt/calls

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

p = plot_ly(df, x = ~year, y = ~calls,
            type = 'scatter', mode = 'lines+markers') 
p

p = plot_ly(df, x = ~year, y = ~capacity_dwt,
            type = 'scatter', mode = 'lines+markers') 
p

tsData = ts(df[,c("calls","capacity_dwt")], start=c(2002), end=c(2015), frequency=1)
acf(tsData[,1])
pacf(tsData[,1])
acf(tsData[,2])
pacf(tsData[,2])


