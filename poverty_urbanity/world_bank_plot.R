library("readxl")
library("plotly")

source("~/whatcheer_posts/bbc_style_function.R")

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

## Correlations and Regression

cor(final$urban_pop,final$poverty_rate)
cor(final$urban_pop,final$gdp_per_capita)

reg = lm(poverty_rate ~ urban_pop + total_population + gdp_per_capita,data = final)
summary(reg)

reg = lm(poverty_rate ~ urban_pop, data = final)
summary(reg)

## Create Plot using Plotly

final$size = final$total_population
final$size[final$`Country Code`=="CHN"] = final$size[final$`Country Code`=="CHN"]/3

plot_ly(final, 
        x = ~urban_pop, y = ~poverty_rate, text = ~`Country Name`, 
        type = 'scatter', mode = 'markers', color = ~income_type, size = ~size,
        marker = list(sizeref=0.1, sizemode="area")
        ) %>%
  # add_lines(y = ~fitted(loess(poverty_rate ~ urban_pop)),color = ~income_type,
  #           line = list(color = '#07A4B5',width = 3),
  #           name = "Loess Smoother", showlegend = FALSE,hoverinfo = "none") %>%
  layout(title = 'Urbanity and Poverty',
         xaxis = list(title = "Urban Population (% of Total Population)",showgrid = FALSE),
         yaxis = list(title = "Poverty Rate (% of Total Population)", showgrid = FALSE))

## Create Plot Using GGplotly

final$poverty_rate_pred = predict(reg,data = final)
final$poverty_rate_pred[final$poverty_rate_pred<0] = 0

ggp = ggplot(final,aes(x = urban_pop,y = poverty_rate,color = income_type,text = `Country Name`, group = 1)) +
  geom_point(aes(size = total_population), alpha = 0.5) + 
  geom_line(color='red', aes(y = poverty_rate_pred), size=1, alpha=0.4) +
  scale_size(range = c(1,14)) +
  scale_color_brewer(palette="Dark2") +
  bbc_style_new() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top", 
    legend.justification = "top",
    axis.text=element_text(size=10,face = "bold")) +
  labs(title="Urbanization and Poverty",
       subtitle = "Insider Revenue is an Increasingly Large Component of Total Revenue") +
  xlab("Urban Population (% of Total Population)") +
  ylab("Poverty Rate (% of Total Population)") 

ggplotly(ggp,
         tooltip=c("Country Name","urban_pop","poverty_rate","total_population"))

