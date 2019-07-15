library("ggplot2")
library("bbplot")
library("scales")

setwd("~/Downloads/")

bbc_style_new <- function(x_axis_title,y_axis_title) {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=25,
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



##Processing
ogdf = read.csv("Journalists Killed between 1992 and 2019 - Motive Confirmed.csv",stringsAsFactors = FALSE)

df = ogdf[,c("year","fullName","primaryNationality","gender","typeOfDeath",
             "employedAs","country","location","localOrForeign")]

df = as.data.frame(apply(df,2, function(x) ifelse(x=="",NA,x)))

colSums(is.na(df))
str(df)

df[,colnames(df)] = lapply(df[,colnames(df)],function(x) as.character(x))

df$year = as.numeric(df$year)

df$primaryNationality[df$fullName=="Martin O'Hagan"] = "Ireland"

#Journalists Killed by Nationality
nationality = as.data.frame(table(df$primaryNationality))

ggplot(head(nationality[order(-nationality$Freq),],10), 
       aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat="identity", 
           position="identity",
           fill="#1380A1") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), hjust=-0.25,
            family="Helvetica",fontface = "bold",size=4) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Nationality") + ylab("Journalists Killed (Since 1992)") +
  labs(title="Journalists Killed by Nationality")

#Deadliest Years for Journalist
year = as.data.frame(table(df$year))
ggplot(head(year[order(-year$Freq),],10), 
       aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat="identity", 
           position="identity",
           fill="#1380A1") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), hjust=-0.25,
            family="Helvetica",fontface = "bold",size=4) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Year") + ylab("Journalists Killed") +
  labs(title="Deadliest Years for Journalists (Since 1992)")


#nationality_year = as.data.frame(table(df$year, df$primaryNationality))

#Journalists Killed (Location and Year)
df$location_country = paste(df$year,paste(df$location,df$country,sep = ", "),sep = "-")
location_year = as.data.frame(table(df$location_country))
ggplot(head(location_year[order(-location_year$Freq),],10), 
       aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat="identity", 
           position="identity",
           fill="#1380A1") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), hjust=-0.25,
            family="Helvetica",fontface = "bold",size=4) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Year and Location") + ylab("Journalists Killed") +
  labs(title="Journalists Killed (Location and Year)")

#Foreign Journalists Killed by Location
foreign = df[df$localOrForeign=="Foreign",]
foreign$location_country = paste(foreign$year,paste(foreign$location,foreign$country,sep = ", "),sep = "-")
location_year = as.data.frame(table(foreign$location_country))
ggplot(head(location_year[order(-location_year$Freq),],10), 
       aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat="identity", 
           position="identity",
           fill="#1380A1") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), hjust=-0.25,
            family="Helvetica",fontface = "bold",size=4) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  coord_flip() +
  bbc_style_new() +
  xlab("Year and Location") + ylab("Foreign Journalists Killed") +
  labs(title="Foreign Journalists Killed")


#Foreign and Local Journalists Killed
year = as.data.frame(table(df$year,df$localOrForeign))
year$Var1 =  as.Date(as.character(year$Var1),"%Y")
ggplot(year, aes(x = Var1, y = Freq, group=Var2,colour = Var2)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
  scale_x_date(labels = date_format("%Y"), breaks=date_breaks('5 years')) +
  bbc_style_new() +
  xlab("Year") + ylab("Journalists Killed") +
  labs(title="Foreign and Local Journalists Killed")

#percent change for journalists deaths
year = as.data.frame(table(df$year))
year = year[order(year$Var1,year$Freq),]
year$lag_value = c(NA, year$Freq[-nrow(year)])
year$diff = year$Freq-year$lag_value
year$perc_change = year$diff/year$lag_value
year$perc_change = year$perc_change*100

temp = as.data.frame(table(df$location_country[df$year=="2003"])) #iraq
temp = as.data.frame(table(df$location_country[df$year=="2001"])) #afghanistan
temp = as.data.frame(table(df$location_country[df$year=="2009"])) #phillipines
temp = as.data.frame(table(df$location_country[df$year=="2012"])) #syria

year = as.data.frame(table(df$year[df$localOrForeign=="Foreign"]))
year = year[order(year$Var1,year$Freq),]
year$lag_value = c(NA, year$Freq[-nrow(year)])
year$diff = year$Freq-year$lag_value
year$perc_change = year$diff/year$lag_value
year$perc_change = year$perc_change*100

temp = as.data.frame(table(df$location_country[df$year=="2003"&df$localOrForeign=="Foreign"])) #iraq
temp = as.data.frame(table(df$location_country[df$year=="2001"&df$localOrForeign=="Foreign"])) #afghanistan
temp = as.data.frame(table(df$location_country[df$year=="2006"&df$localOrForeign=="Foreign"])) #iraq/afghanistan
temp = as.data.frame(table(df$location_country[df$year=="1999"&df$localOrForeign=="Foreign"])) #yugoslavia


###Observations

#Recent years more journalists killed!
#surprising phillipines is so high!
#bc of one incident in phillipines, talk about kabul, kigali
#foreign journalists killed very low--yugoslavian countries have highest #s
#not many foreign journalists killed...why?
##2003-2005 biggest shift in local journalists killed...why??
##larger cost of killing foreign journalists?

temptemp = as.data.frame(table(df$year,df$country))

