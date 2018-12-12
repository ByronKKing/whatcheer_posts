library("readxl")
library("reshape2")
library("ggplot2")

setwd("~/whatcheer_posts/vessel_calls/vessel_calls_data/")
#setwd("~/Downloads/vessel_calls/vessel_calls_data/")

#dwt = mass -- "capacity"
#gt = volume -- 


# Get years 2002-2012 in one sheet

append_df = NULL
year = 2012
for(i in 2:12){
  
  df = read_excel("dsusportcalls2002-2012.xls", i)
  colnames(df) = df[3,]
  df = df[4:nrow(df),]
  df$year = year
  
  append_df = rbind(append_df,df)
  year = year-1
  
}

colnames(append_df) = c("port","state",
                        "overall_calls","overall_capacity",
                        "tankers_calls","tankers_capacity",
                        "tankers_b60_calls","tankers_b60_dwt",
                        "tankers_a60_calls","tankers_a60_dwt",
                        "containers_calls","containers_dwt","containers_teu",
                        "gas_calls","gas_dwt","gas_gas",
                        "roro_calls","roro_dwt",
                        "bulk_calls","bulk_capacity",
                        "general_calls","general_dwt",
                        "year")


# Get years 2013-2015 in separate sheets

append_df2 = NULL
year = 2013

for(i in 1:3){
  
  df = read_excel(paste("dsvesselcalls",as.character(year),".xlsx",sep = ""),1)
  colnames(df) = df[5,]
  df = df[6:nrow(df),]
  df$year = year
  df$state = trimws(sapply(strsplit(df$Port,",",fixed = FALSE), "[", 2)) 
  df$Port = sapply(strsplit(df$Port,",",fixed = FALSE), "[", 1)
  
  append_df2 = rbind(append_df2,df)
  year = year+1
  
}

colnames(append_df2) = c("port",
                         "overall_calls","overall_gt","overall_capacity",
                         "containers_calls","containers_gt","containers_dwt",
                         "bulk_calls","bulk_gt","bulk_capacity",
                         "gas_calls","gas_gt","gas_dwt",
                         "general_calls","general_gt","general_dwt",
                         "roro_calls","roro_gt","roro_dwt",
                         "tankers_calls","tankers_gt","tankers_capacity",
                         "year","state")


# Prepare both datasets to rbind together

add_cols2 = colnames(append_df)[!(colnames(append_df) %in% colnames(append_df2))]
add_cols = colnames(append_df2)[!(colnames(append_df2) %in% colnames(append_df))]

for(i in 1:length(add_cols2)){
  append_df2[,add_cols2[i]] = NA
}

for(i in 1:length(add_cols)){
  append_df[,add_cols[i]] = NA
}

append_df2 = append_df2[,match(colnames(append_df),colnames(append_df2))]

finaldf = rbind(append_df,append_df2)


# Analysis

###introduction to different ship types/ship metrics
##overall look at summed dwt for different ship types -- done
##look at top ports in each category and overall -- done
##overall look by state (and by vessel type) -- 12/11 -- done
##similar composition to RI? -- 12/11 -- done
##RI overview, Davisville spotlight -- 12/12
##cluster analysis of similar ports for 2015 (or over many years) -- 12/12
##which port is Davisville like, i.e. sister ports? -- 12/12



## Process dataset
keep_cols = colSums(is.na(finaldf))
df =  finaldf[,names(keep_cols[keep_cols < 300])]
#colSums(is.na(df))
df = df[!(is.na(df$port)),]
#View(df[is.na(df$state),])
df = df[df$port=="Grand Total"|!(is.na(df$state)),]


## overall look at summed dwt for different ship types, tankers,containers,gas,roro,bulk,general

###get only grand total sum
grand_df = df[df$port=="Grand Total",colnames(df)[!(colnames(df) %in% c("state","port"))]]
grand_df[,colnames(grand_df)] = lapply(grand_df[,colnames(grand_df)],as.numeric)

for(i in 2012:2002){
  year_sums = c()
  for(j in 1:(length(colnames(grand_df))-1)){
    year_sums = append(year_sums,
                       sum(as.numeric(
                         df[df$year == as.character(i),colnames(grand_df)[j]][[1]]
                       ),na.rm = TRUE))
  }
  year_sums = append(year_sums,i)
  grand_df = rbind(grand_df,year_sums)
}

grand_df = grand_df[order(grand_df$year),]
grand_df = grand_df[!duplicated(grand_df),]

###plot time series for calls
meltdf = melt(grand_df[,colnames(grand_df)[grepl("capacity|dwt|year",colnames(grand_df))]],
              id=c("year"))

plotdf = meltdf[meltdf$variable=="overall_capacity",]
ggplot(plotdf,aes(x=year,y=value)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2002,2015)) +
  xlab("Year") +
  ylab("Overall Capacity (DWT)") +
  ggtitle("US Vessel Capacity") +
  theme(plot.title = element_text(hjust = 0.5))

plotdf = meltdf[meltdf$variable!="overall_capacity",]
ggplot(plotdf,aes(x=year,y=value,colour=variable,group=variable)) + 
  geom_line() +
  geom_point()  +
  xlab("Year") +
  ylab("Capacity (DWT)") +
  ggtitle("US Vessel Capacity by Vessel Type") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5))

meltdf = melt(grand_df[,colnames(grand_df)[grepl("calls|year",colnames(grand_df))]],
              id=c("year"))
plotdf = meltdf[meltdf$variable!="overall_calls",]
ggplot(plotdf,aes(x=year,y=value,colour=variable,group=variable,label=value)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2002,2015)) +
  geom_text(aes(label=value),vjust=0,nudge_y = 1) +
  xlab("Year") +
  ylab("Calls") +
  ggtitle("US Vessel Calls by Vessel Type") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5))

###cleveland dot charts
ggplot(plotdf, aes(year, value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_continuous(breaks=seq(2002,2015)) +
  xlab("Year") +
  ylab("Calls") +
  ggtitle("US Vessel Calls by Vessel Type") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5))


## look at top ports in each category and overall
df[,colnames(df)[!(colnames(df) %in% c("port","state"))]] = 
  lapply(df[,colnames(df)[!(colnames(df) %in% c("port","state"))]],as.numeric)
topdf = df[df$port!="Grand Total",]
topdf = topdf[topdf$year==2015,]

###overall
ggplot(head(topdf[order(-topdf$overall_calls),c("port","state","overall_calls")],8), 
       aes(reorder(port, overall_calls), overall_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by Total 2015 Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

###by category
p1 = ggplot(head(topdf[order(-topdf$tankers_calls),c("port","state","tankers_calls")],8), 
            aes(reorder(port, tankers_calls), tankers_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(x=port, y=tankers_calls, label=tankers_calls, 
                hjust=ifelse(sign(tankers_calls)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by 2015 Tanker Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

p1

p2 = ggplot(head(topdf[order(-topdf$containers_calls),c("port","state","containers_calls")],8), 
            aes(reorder(port, containers_calls), containers_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(x=port, y=containers_calls, label=containers_calls, 
                hjust=ifelse(sign(containers_calls)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by 2015 Container Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

p2

p3 = ggplot(head(topdf[order(-topdf$gas_calls),c("port","state","gas_calls")],8), 
            aes(reorder(port, gas_calls), gas_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(x=port, y=gas_calls, label=gas_calls, 
                hjust=ifelse(sign(gas_calls)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by 2015 LNG Carrier Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

p3


p4 = ggplot(head(topdf[order(-topdf$roro_calls),c("port","state","roro_calls")],8), 
            aes(reorder(port, roro_calls), roro_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(x=port, y=roro_calls, label=roro_calls, 
                hjust=ifelse(sign(roro_calls)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by 2015 RoRo Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

p4

p5 = ggplot(head(topdf[order(-topdf$bulk_calls),c("port","state","bulk_calls")],8), 
            aes(reorder(port, bulk_calls), bulk_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(x=port, y=bulk_calls, label=bulk_calls, 
                hjust=ifelse(sign(bulk_calls)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by 2015 Bulk Carrier Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

p5

p6 = ggplot(head(topdf[order(-topdf$general_calls),c("port","state","general_calls")],8), 
            aes(reorder(port, general_calls), general_calls, fill = port)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(x=port, y=general_calls, label=general_calls, 
                hjust=ifelse(sign(general_calls)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  xlab("Port") +
  ylab("Calls") +
  ggtitle("Top Ports by 2015 General Carrier Calls") +
  labs(color='Vessel Type') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

p6

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(p1, p2, p3, p4, p5, p6, cols=3)

### by state

topdf[is.na(topdf)] = 0
state_df = do.call(data.frame, 
                  aggregate(cbind(overall_calls,overall_capacity,tankers_calls,tankers_capacity,
                            containers_calls,containers_dwt,gas_calls,gas_dwt,
                            roro_calls,roro_dwt,bulk_calls,bulk_capacity,general_calls,general_dwt)~state,
                  data = topdf, FUN = function(x) c(mn = mean(x), sm = sum(x)))) 
state_df$aver_capacity = state_df$overall_capacity.sm/state_df$overall_calls.sm
state_df[is.na(state_df)] = 0
fit = kmeans(state_df[,c(2:length(state_df))],5)
state_df$cluster = fit$cluster

ggplot(state_df, aes(x= overall_calls.sm, y= overall_capacity.sm,
                     label=state,colour = factor(cluster)))+
  geom_point() +
  geom_text(aes(label=state),hjust=0,vjust=0,
            check_overlap = TRUE) +
  scale_color_discrete() +
  labs(x = "LSCI", y = "# Ships",colour = "Cluster") + ggtitle("Country LSCI Clusters") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(state_df[state_df$cluster==state_df$cluster[state_df$state=="RI"]&state_df$state!="RI",], 
       aes(x= overall_calls.sm, y= overall_capacity.sm,label=state))+
  geom_point() +
  geom_text(aes(label=state),hjust=0,vjust=0,check_overlap = TRUE) +
  scale_color_discrete() +
  geom_point(data=state_df[state_df$state=="RI",], colour="red") +  # this adds a red point
  geom_text(data=state_df[state_df$state=="RI",], label="RI",hjust=1,vjust=0,check_overlap = TRUE) +
  labs(x = "LSCI", y = "# Ships",colour = "Cluster") + ggtitle("Country LSCI Clusters") +
  theme(plot.title = element_text(hjust = 0.5))

##MA is more like AK than any NE states