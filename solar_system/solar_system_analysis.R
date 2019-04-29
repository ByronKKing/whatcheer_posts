library("ggplot2")
library("bbplot")

set.seed(8)

ogdf = read.csv("~/whatcheer_posts/solar_system/processed_data.csv")

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
aggregate(cbind(gravity_g,mass_m,radius_r,volume_v,density)~type,df,mean)

##solar planets masses bar chart
ggplot(df[grepl("planet -",df$type),], 
       aes(x = reorder(body, mass_m), y = mass_m)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style() +
  labs(title="Reunion is highest",
       subtitle = "Highest African life expectancy, 2007")
ggplot(df[grepl("terrestrial",df$type),], 
       aes(x = reorder(body, mass_m), y = mass_m)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style() +
  labs(title="Reunion is highest",
       subtitle = "Highest African life expectancy, 2007")
##non-gas giant planet masses bar chart
ggplot(df[grepl("giant",df$type),], 
       aes(x = reorder(body, mass_m), y = mass_m)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style() +
  labs(title="Reunion is highest",
       subtitle = "Highest African life expectancy, 2007")
##moons masses bar chart

ggplot(df[grepl("moon",df$type),], 
       aes(x = reorder(body, mass_m), y = mass_m)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style() +
  labs(title="Reunion is highest",
       subtitle = "Highest African life expectancy, 2007")

ggplot(
  rbind(df[grepl("terrestrial",df$type),],
        head(df[grepl("moon",df$type),],6)), 
       aes(x = reorder(body, mass_m), y = mass_m)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style() +
  labs(title="Reunion is highest",
       subtitle = "Highest African life expectancy, 2007")

##gravity varies, density varies, mass and radius and volume decrease in size
ggplot(rbind(df[grepl("planet",df$type),],head(df[grepl("moon",df$type),],6)),  
       aes(x = gravity_g, y = density, label = body, colour = factor(type))) +
  geom_point(aes(size=mass_m)) +
  bbc_style_new() +
  xlab("Gravity") + ylab("Density") +
  geom_text(aes(label=body),hjust=-0.2,vjust=0.5,
            check_overlap = TRUE) +
  theme(legend.position = "top", 
        legend.justification = "top",
        axis.text=element_text(size=10,face = "bold")) +
  labs(title="Hate Crimes Reported by Police",
       subtitle = "Source: Organization for Security and Co-operation in Europe")

##cluster planets, can distinguish b/w ice giants/gas giants/terrestrial/moons?
scaled_df = df[,c("body","type","gravity_g","mass_m","radius_r","volume_v","density")]
#scaled_df = scale(scaled_df[,3:length(scaled_df)])
scaled_df[,3:length(scaled_df)] = lapply(scaled_df[,3:length(scaled_df)],scale)
fit = kmeans(scaled_df[,c(3:length(scaled_df))],6)
scaled_df$cluster = fit$cluster
scaled_df$cluster_type = ""
scaled_df$cluster_type[scaled_df$cluster==5] = "star"
scaled_df$cluster_type[scaled_df$cluster==2] = "planet_terrestrial"
scaled_df$cluster_type[scaled_df$cluster==6] = "moon"
scaled_df$cluster_type[scaled_df$cluster==3] = "large_planet/small_moon"
scaled_df$cluster_type[scaled_df$cluster==1] = "large moon"
scaled_df$cluster_type[scaled_df$cluster==4] = "dwarf planet"
table(scaled_df$type,scaled_df$cluster_type)


scaled_df = df[,c("body","type","gravity_g","mass_m","radius_r","volume_v","density")]
scaled_df[,3:length(scaled_df)] = lapply(scaled_df[,3:length(scaled_df)],scale)
fit = kmeans(scaled_df[,c(3:length(scaled_df))],6)
scaled_df$cluster = fit$cluster
scaled_df$cluster = as.character(scaled_df$cluster)
scaled_df$cluster_type = ""
scaled_df$cluster_type[scaled_df$cluster==1] = "mid-sized moons" #mars
scaled_df$cluster_type[scaled_df$cluster==2] = "small moons" #uranus
scaled_df$cluster_type[scaled_df$cluster==3] = "gas giants"
scaled_df$cluster_type[scaled_df$cluster==4] = "big moons/dwarf planets" #neptune
scaled_df$cluster_type[scaled_df$cluster==5] = "terrestrial planets"
scaled_df$cluster_type[scaled_df$cluster==6] = "star"
table(scaled_df$type,scaled_df$cluster_type)

##caught all dwarfs, all gas giants, star
##most terrestrial (except Mars), split up ice giants, split moons into big moons, midsized moons, small moons

##different uses of planets in 2312 by type
###those for terraforming
###those for exporting nitrogen
###those for harvesting sunlight

######

##gravity_g,mass_m,radius_r,volume_v,density
##gravity_ms,mass_kg,radius_km,volume_km
##body,type


planets = df[grepl("planet",df$type),]
planets = planets[!(grepl("dwarf",planets$type)),]

planets$mass_kg = as.numeric(as.character(planets$mass_kg))
planets$volume_km = as.numeric(planets$volume_km)



ggplot(planets, aes(x= mass_kg, y= volume_km,
                     label=body,colour = factor(type)))+
  geom_point() +
  geom_text(aes(label=body),hjust=0,vjust=0,
            check_overlap = TRUE)


ggplot(planets, aes(x= radius_r, y= gravity_g,
                    label=body,colour = factor(type)))+
  geom_point() +
  geom_text(aes(label=body),hjust=0,vjust=0,
            check_overlap = TRUE)

