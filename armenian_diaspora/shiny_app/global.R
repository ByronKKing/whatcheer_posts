library("xlsx")
library("dplyr")

ogdf = read.xlsx("data/processed_data.xlsx", sheetName = "Sheet 1", stringsAsFactors = FALSE)

df = ogdf

##one-offs
df$lat[df$area=="Fresno County"] = 36.746841
df$lon[df$area=="Fresno County"] = -119.772591

df$upper_estimate[df$area=="Stavropol region"] = "250,000"

##remove dups
df = df[!(duplicated(df)),]

##convert to numerics
num_chars = c("lower_estimate","upper_estimate","estimate_1","estimate_2","lat","lon")

df[,num_chars] = lapply(df[,num_chars],function(x) gsub(",","",x))
df[,num_chars] = lapply(df[,num_chars],function(x) as.numeric(x))

df = df[,colnames(df)[!(colnames(df) %in% c("estimations","largest_community"))]]

####To-Do

#fix data popup label
#fix data circle size
#fix lats and lons in data
#filter plotted circles by country? maybe zoom in on continent?



