library("readxl")
library("plotly")

file_path = "~/whatcheer_posts/poverty_urbanity/world_bank_data.xlsx"

## Prepare dataframes from each worksheet

urbanpop = read_excel(file_path,1)
urbanpop = urbanpop[,c(1,2,63)]
colnames(urbanpop)[3] = "variable"

poverty = read_excel(file_path,2)

colSums(is.na(poverty[,5:length(poverty)]))
min(colSums(is.na(poverty[,5:length(poverty)])))
which.min(colSums(is.na(poverty[,5:length(poverty)]))) ##use 2010 data bc has most complete data

poverty = poverty[,c(1,2,55)]
colnames(poverty)[3] = "variable"

gdp = read_excel(file_path,3)
gdp = gdp[,c(1,2,63)]
colnames(gdp)[3] = "variable"

totalpop = read_excel(file_path,4)
totalpop = totalpop[,c(1,2,63)]
colnames(totalpop)[3] = "variable"

final = rbind(urbanpop,poverty,gdp,totalpop)

final = unique(final[,c("Country Name","Country Code")])

colnames(urbanpop)[3] = "urban_pop"
colnames(poverty)[3] = "poverty_rate"
colnames(gdp)[3] = "gdp_per_capita"
colnames(totalpop)[3] = "total_population"

## Create final dataframe

final = merge(final,gdp[,c(2,3)],by = "Country Code")
final = merge(final,poverty[,c(2,3)], by = "Country Code")
final = merge(final,totalpop[,c(2,3)],by = "Country Code")
final = merge(final,urbanpop[,c(2,3)], by = "Country Code")

final = final[!(final$`Country Name` %in% c("East Asia & Pacific","Europe & Central Asia",
                                            "Fragile and conflict affected situations","High income",
                                            "IDA total","Latin America & Caribbean",
                                            "Low income","Lower middle income","Low & middle income",
                                            "Middle East & North Africa","Sub-Saharan Africa",
                                            "Upper middle income","World","South Asia")),]

colSums(is.na(final))

final = final[complete.cases(final),]

final$income_type = ifelse(final$gdp_per_capita>=19948.9228,"High Income",
                           ifelse(final$gdp_per_capita<=2907.3176,"Low Income","Middle Income"))

## Correlations

cor(final$urban_pop,final$poverty_rate)
cor(final$urban_pop,final$gdp_per_capita)

## Create Plot using Plotly

final$size = final$total_population
final$size[final$`Country Code`=="CHN"] = final$size[final$`Country Code`=="CHN"]/3

plot_ly(final, 
        x = ~urban_pop, y = ~poverty_rate, text = ~`Country Name`, 
        type = 'scatter', mode = 'markers', color = ~income_type, size = ~size,
        marker = list(sizeref=0.2, sizemode="area")
        ) %>%
  # add_lines(y = ~fitted(loess(poverty_rate ~ urban_pop)),color = ~income_type,
  #           line = list(color = '#07A4B5',width = 3),
  #           name = "Loess Smoother", showlegend = FALSE,hoverinfo = "none") %>%
  layout(title = 'Urbanity and Poverty',
         xaxis = list(title = "Urban Population (% of Total Population)",showgrid = FALSE),
         yaxis = list(title = "Poverty Rate (% of Total Population)", showgrid = FALSE))

## Create Plot Using GGplotly

ggp = ggplot(final,aes(x = urban_pop,y = poverty_rate,color = income_type)) +
  geom_point(aes(size = total_population)) + 
  stat_smooth(method = "loess", formula = "poverty_rate ~ urban_pop", data = final, size = 1)

ggplotly(ggp)


##figure out loess or don't use
##use bbc_plot for ggplotly

