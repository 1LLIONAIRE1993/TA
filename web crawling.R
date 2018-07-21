library(magrittr) #pipe
library(RCurl) #crawler 
library(jsonlite) #read JSON 
url = "http://maps.google.com/maps/api/geocode/json?address=政治大學&language=zh-TW"
url %>% RCurl::getURL() %>% jsonlite::fromJSON()

getLoc = function(query){
  url = "http://maps.google.com/maps/api/geocode/json?address="
  req = paste(url,query,"&language=zh-TW",sep="")
  repeat{
    res = getURL(req) %>% fromJSON()
    if(res$status=="OK") break
    Sys.sleep(1)
  }
  ## get result
  loc = res$results$geometry$location[1,]
  loc_type = res$results$geometry$location_type[1]
  address = res$results$formatted_address[1]
  type = paste(res$results$types[[1]],collapse ="|") 
  res = c(query,loc[1,1],loc[1,2],loc_type,address,type)
  names(res) = c("query","lat","lng","loc_type","address","types")
  return(res)
}


#餐廳名稱
restaurant = c("李氏兄弟","里克德義廚房","政大羹大王","政大香香","老薑咖啡","JuicyBun","政大+小曼谷","舒曼六號","政大烤場","政大+滇味廚房","小公寓","my麵屋")
#餐廳類型
type = c("西式","西式","台式","台式","咖啡","美式","雲泰","西式","宵夜","雲泰","咖啡","台式")
df_temp = data.frame(restaurant,type,stringsAsFactors = F)
#抓取經緯度，建立data frame
info = sapply(restaurant,getLoc) %>% t() %>% as.data.frame()
info$query %<>% as.character()
info$lat %<>% as.character() %>% as.numeric()
info$lng %<>% as.character() %>% as.numeric()
#Merge
df_rest = merge(df_temp,info,by.x="restaurant",by.y="query")
df_rest %>% head()




library(leaflet)
## make custiom icon
col = factor(df_rest$type)
levels(col) = c("red","orange","green","blue","pink","gray")
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = as.character(col)
)

map <- leaflet(df_rest) %>%
  addTiles()  %>% 
  setView(lng=mean(df_rest$lng),lat=mean(df_rest$lat),zoom=17) %>% 
  addAwesomeMarkers(~lng, ~lat, icon=icons,
                    popup=~sub(".*\\+","",restaurant))
map