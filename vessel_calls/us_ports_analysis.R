library("readxl")

setwd("~/Desktop/vessel_calls/vessel_calls_data/")

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
