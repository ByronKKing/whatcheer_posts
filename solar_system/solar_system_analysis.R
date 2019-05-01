library("ggplot2")
library("bbplot")
library("DT")

set.seed(8)

ogdf = read.csv("~/whatcheer_posts/solar_system/processed_data.csv")

bbc_style_new <- function(x_axis_title,y_axis_title) {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222",
                                       hjust=.5),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0),
                                          hjust=.5),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    # axis.title.x = x_axis_title,
    # axis.title.y = y_axis_title,
    axis.title = ggplot2::element_text(family=font,
                                       size=18,
                                       color="#222222"),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

setwd("~/whatcheer_posts/solar_system/")

#process df
df = ogdf

df$mass_kg = as.numeric(as.character(df$mass_kg))

df$density = as.character(df$density)
df$density = sapply(strsplit(as.character(df$density),'±'), "[", 1)
df$density = as.numeric(df$density)

df = df[,colnames(df)[!(colnames(df) %in% c("number","image"))]]
df = df[complete.cases(df),]

df$type = as.character(df$type)
df$type = gsub("\n","",df$type)

df$type[grepl("dwarf",df$type)] = "dwarf planet"
df$type[grepl("moon",df$type)] = "moon"
df$type[grepl("ice giant",df$type)] = "planet - ice giant"
df$type[grepl("gas giant",df$type)] = "planet - gas giant"
df$type[grepl("terrestrial",df$type)] = "planet - terrestrial"

df$has_rings = ifelse(grepl("giant",df$type),"yes","no")

#####

##overall description by type
summ_df = aggregate(cbind(gravity_g,mass_m,radius_r,volume_v,density)~type,df,mean)

summ_df[,2:length(summ_df)] = lapply(summ_df[,2:length(summ_df)],function(x) round(x,3))

datatable(
  summ_df
  ,options = list(dom = 't'),
  rownames = FALSE
) %>%
  DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6), fontSize = '9pt')

##solar planets masses bar chart
ggplot(df[grepl("planet -",df$type),], 
       aes(x = reorder(body, mass_m), y = mass_m, fill = type)) +
  geom_bar(stat="identity", 
           position="identity") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Planet") + ylab("Mass (Standardized Earth Mass)") +
  labs(title="All Planets by Mass") + 
  scale_fill_brewer(palette = "Dark2") 

##terrestrial + moons
ggplot(
  rbind(df[grepl("terrestrial",df$type),],
        head(df[grepl("moon",df$type),],6)), 
  aes(x = reorder(body, mass_m), y = mass_m, fill = type)) +
  geom_bar(stat="identity", 
           position="identity") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Body") + ylab("Mass (Standardized Earth Mass)") +
  labs(title="Smallest Planets and Biggest Moons by Mass") +
  scale_fill_manual(values=c("#990000", "#1380A1"))

##moons masses bar chart
ggplot(df[grepl("moon",df$type),], 
       aes(x = reorder(body, mass_m), y = mass_m)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#990000") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Body") + ylab("Mass (Standardized Earth Mass)") +
  labs(title="Moons by Mass")

##gravity varies, density varies, mass and radius and volume decrease in size
ggplot(rbind(df[grepl("planet",df$type),],head(df[grepl("moon",df$type),],6)),  
       aes(x = gravity_g, y = density, label = body, colour = factor(type))) +
  geom_point(aes(size=mass_m)) +
  bbc_style_new() +
  xlab("Gravity (Standard Gravity)") + ylab("Density (g per cubic centimeter)") +
  geom_text(aes(label=body),hjust=-0.2,vjust=0.5,
            check_overlap = TRUE) +
  theme(legend.position = "top", 
        legend.justification = "top",
        axis.text=element_text(size=10,face = "bold")) +
  labs(title="Planets and 6 Most Massive Moons",
       subtitle = "Larger Bubbles Constitute Greater Earth Mass ")

##cluster planets, can distinguish b/w ice giants/gas giants/terrestrial/moons?
# scaled_df = df[,c("body","type","gravity_g","mass_m","radius_r","volume_v","density")]
# #scaled_df = scale(scaled_df[,3:length(scaled_df)])
# scaled_df[,3:length(scaled_df)] = lapply(scaled_df[,3:length(scaled_df)],scale)
# fit = kmeans(scaled_df[,c(3:length(scaled_df))],6)
# scaled_df$cluster = fit$cluster
# scaled_df$cluster_type = ""
# scaled_df$cluster_type[scaled_df$cluster==5] = "star"
# scaled_df$cluster_type[scaled_df$cluster==2] = "planet_terrestrial"
# scaled_df$cluster_type[scaled_df$cluster==6] = "moon"
# scaled_df$cluster_type[scaled_df$cluster==3] = "large_planet/small_moon"
# scaled_df$cluster_type[scaled_df$cluster==1] = "large moon"
# scaled_df$cluster_type[scaled_df$cluster==4] = "dwarf planet"
# table(scaled_df$type,scaled_df$cluster_type)


scaled_df = df[,c("body","type","gravity_g","mass_m","radius_r","volume_v","density")]
scaled_df[,3:length(scaled_df)] = lapply(scaled_df[,3:length(scaled_df)],scale)
#fit = kmeans(scaled_df[,c(3:length(scaled_df))],6)
#save(fit,file = "./kmeans_fit.Rda")
load(file = "./kmeans_fit.Rda")
scaled_df$cluster = fit$cluster
scaled_df$cluster = as.character(scaled_df$cluster)
scaled_df$cluster_type = ""
scaled_df$cluster_type[scaled_df$cluster==1] = "mid-sized moons" #mars
scaled_df$cluster_type[scaled_df$cluster==2] = "small moons" #uranus
scaled_df$cluster_type[scaled_df$cluster==3] = "gas giants"
scaled_df$cluster_type[scaled_df$cluster==4] = "big moons/dwarf planets" #neptune
scaled_df$cluster_type[scaled_df$cluster==5] = "terrestrial planets"
scaled_df$cluster_type[scaled_df$cluster==6] = "star"
print(table(scaled_df$type,scaled_df$cluster_type))

datatable(
  scaled_df[,c("body","type","cluster_type")]
  ,#options = list(dom = 't'),
  rownames = FALSE
  ) %>%
  DT::formatStyle(columns = c(1, 2, 3), fontSize = '9pt')

##caught all dwarfs, all gas giants, star
##most terrestrial (except Mars), split up ice giants, split moons into big moons, midsized moons, small moons

##gravity_g,mass_m,radius_r,volume_v,density
##gravity_ms,mass_kg,radius_km,volume_km
##body,type
