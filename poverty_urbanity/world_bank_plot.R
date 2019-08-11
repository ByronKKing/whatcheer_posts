library("readxl")
library("plotly")

file_path = "~/whatcheer_posts/poverty_urbanity/world_bank_data.xlsx"

urbanpop = read_excel(file_path,1)
urbanpop = urbanpop[,c(1,2,63)]
colnames(urbanpop)[3] = "urban_pop"

poverty = read_excel(file_path,2)
poverty = poverty[,c(1,2,55)]
colnames(poverty)[3] = "poverty_rate"

gdp = read_excel(file_path,3)
gdp = gdp[,c(1,2,63)]
colnames(gdp)[3] = "gdp_per_capita"

totalpop = read_excel(file_path,4)
totalpop = totalpop[,c(1,2,63)]
colnames(totalpop)[3] = "total_population"

final = as.data.frame(unique(c(gdp$`Country Name`,totalpop$`Country Name`,poverty$`Country Name`,urbanpop$`Country Name`)))
names(final) = "Country Name"

final = merge(final,gdp[,c(2,3)],by = "Country Code")
final = merge(final,poverty[,c(2,3)], by = "Country Code")
final = merge(final,totalpop[,c(2,3)],by = "Country Code")
final = merge(final,urbanpop[,c(2,3)], by = "Country Code")

final = merge(final,gdp[,c(1,3)],by = "Country Name")
final = merge(final,poverty[,c(1,3)], by = "Country Name")
final = merge(final,totalpop[,c(1,3)],by = "Country Name")
final = merge(final,urbanpop[,c(1,3)], by = "Country Name")

final = final[complete.cases(final),]

###do this for all dataframes before join, then join on country code
final = final[!(final$`Country Name` %in% c("East Asia & Pacific","Europe & Central Asia",
                                            "Fragile and conflict affected situations","High income",
                                            "IDA total","Latin America & Caribbean",
                                            "Low income","Lower middle income","Low & middle income",
                                            "Middle East & North Africa","Sub-Saharan Africa",
                                            "Upper middle income","World")),]

final$income_type = ifelse(final$gdp_per_capita>=19948.9228,"High Income",
                           ifelse(final$gdp_per_capita<=2907.3176,"Low Income","Middle Income"))

cor(final$urban_pop,final$poverty_rate)
cor(final$urban_pop,final$gdp_per_capita)

final$size = sqrt(final$total_population*2.666051223553066e-06)

plot_ly(final, 
        x = ~urban_pop, y = ~poverty_rate, text = ~`Country Name`, 
        type = 'scatter', mode = 'markers',
        color = ~income_type,
        marker = list(size = ~size, opacity = 0.5)
        ) %>%
  add_lines(y = ~fitted(loess(poverty_rate ~ urban_pop)),
            line = list(color = '#07A4B5',width = 4,shape = "spline"),
            name = "Loess Smoother", showlegend = FALSE) %>%
  layout(title = 'Urbanity and Poverty',
         xaxis = list(title = "Urban Population (% of Total Population)",showgrid = FALSE),
         yaxis = list(title = "Poverty Rate (% of Total Population)", showgrid = FALSE))

##make size of dot function of population (need population)
##variable with fewer missing values for poverty?


###tie article to investing in building and keeping regulation low, especially for developing countries!!





