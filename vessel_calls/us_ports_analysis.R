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
##overall look at summed dwt for different ship types
##look at top ports in each category and overall
##RI overview, Davisville spotlight
##cluster analysis of similar ports for 2015 (or over many years)
##which port is Davisville like, i.e. sister ports?



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
plotdf = df[df$port!="Grand Total",]
plotdf = plotdf[plotdf$year==2015,]




