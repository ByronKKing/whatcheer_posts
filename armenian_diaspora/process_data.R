
library("xlsx")
library("ggmap")
library("rjson")

setwd("~/whatcheer_posts/armenian_diaspora/")

ogdf = read.xlsx("cleaned_data.xlsx", sheetName = "Sheet1", stringsAsFactors = FALSE)

df = ogdf

df$country = sub("^\\s+", "", df$country)

df$official_data = as.numeric(df$official_data)
df$census_year = as.numeric(df$census_year)

#process population estimates

df$estimates_list = strsplit(df$estimations, ", ")
df$length = sapply(df$estimates_list,length)
df$lower_estimate = sapply(df$estimates_list,`[`,1)
df$upper_estimate = NA
df$estimate_1 = NA
df$estimate_2 = NA

for(i in 1:nrow(df)){
  
  df$upper_estimate[i] = sapply(df$estimates_list[i],`[`,df$length[i])
  
  if(df$length[i] %in% c(3,4)){
    
    df$estimate_1[i] = sapply(df$estimates_list[i],`[`,2)
    
    if(df$length[i]==4){
      df$estimate_2[i] = sapply(df$estimates_list[i],`[`,3)
    }
  }
  
}


df = df[,colnames(df)[!(colnames(df) %in% c("length","estimates_list"))]]

#retrieve geocoordinates of locations

base_url = "https://nominatim.openstreetmap.org/search?city=$city&countrycodes=$country&limit=1&format=json"

df$lat = NA
df$lon = NA

df$area = gsub(" ","%20",df$area)

for(i in 1:nrow(df)){
  
  Sys.sleep(1)
  
  print(i)
  
  flag = FALSE
  
  url = gsub("$country",df$country[i],gsub("$city",df$area[i],base_url,fixed = TRUE),fixed = TRUE)
  
  tryCatch({
    response = fromJSON(file = url)
  }, error = function(e) {
    print("error")
    flag = TRUE
    df$lat[i] = NA
    df$lon[i] = NA
  })
  
  if(flag){
    next
  }
  
  if(length(response)==0){
    df$lat[i] = NA
    df$lon[i] = NA
    next
  }
  
  df$lat[i] = response[[1]]$lat
  df$lon[i] = response[[1]]$lon
  
}

df$area = gsub("%20"," ",df$area)

#save as excel workbook

wb = createWorkbook(type="xlsx")
sheet = createSheet(wb, sheetName = "Sheet 1")
addDataFrame(df, sheet, startRow=1, startColumn=1, 
             row.names = FALSE)
saveWorkbook(wb, "processed_data.xlsx")

