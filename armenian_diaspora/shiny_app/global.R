library("xlsx")
library("dplyr")
library("countrycode")

ogdf = read.xlsx("data/processed_data.xlsx", sheetName = "Sheet 1", stringsAsFactors = FALSE)

df = ogdf

##retrieve continent
df$continent = countrycode(sourcevar = df[, "country"],
                            origin = "country.name",
                            destination = "continent")
df$continent[df$country %in% c("United States","Canada")] = "North America"
df$continent[df$country %in% c("Uruguay","Argentina","Brazil")] = "South America"

##remove dups
df = df[!(duplicated(df)),]

##convert to numerics
num_chars = c("lower_estimate","upper_estimate","estimate_1","estimate_2","lat","lon")

df[,num_chars] = lapply(df[,num_chars],function(x) gsub(",","",x))
df[,num_chars] = lapply(df[,num_chars],function(x) as.numeric(x))

df = df[,colnames(df)[!(colnames(df) %in% c("estimations","largest_community"))]]

####To-Do

#final button that zooms to a particular continent (average of geocoordinates in content with zoom = 4)



