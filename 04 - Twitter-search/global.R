library(shiny)
library(leaflet)
# library(base64enc)
# library(twitteR)
# require(plyr)
library(ROAuth)
library(data.table)
library(streamR)
# library(textcat)
library(RColorBrewer)
load("twitCred.Rdata")




#               Funciones                  ############################################################

super_parseTweets <- function(file="tweets.json"){
  
  bpc <- fread("../data/bpc 47 lenguage list.csv")
  setnames(bpc,names(bpc),c("code3","lang","lenguage"))
  bpc <- bpc[lang!="",.(lang,lenguage)]
  bpc <- unique(bpc)
  setkey(bpc,lang)
  
  if (!file.exists(file))
  {filterStream(file, 
                timeout = 60, 
                locations=c(0.12,40.38,3.47,42.82),
                oauth = twitCred  )
  }
  
  tweets.dt <- as.data.table(parseTweets(file))
  # print(as.matrix(tweets.dt[,.(lon,lon)]))
  tweets.dt <- tweets.dt[ !is.na(lon)&!is.na(lat) ,]
  # print(as.matrix(tweets.dt[,.(lon,lon)]))
  tweets.dt$text <- iconv(tweets.dt$text,from = "UTF-8")
  
  setkey(tweets.dt,lang)
  tweets.dt <- merge(tweets.dt,bpc)
  
  tweets.dt$source <-apply(matrix(unlist(lapply(strsplit( gsub("[[:blank:]+]"," ", gsub("[[:punct:]]"," ",gsub("</a>","",tweets.dt$source))), " "),tail,3)),ncol = 3, byrow = T),1,paste,collapse=" ")
  tweets.dt$source <- gsub("nofollow","",tweets.dt$source)
  
  tweets.dt[,c("url","expanded_url","place_id","place_name","user_url","country_code","lang"):=NULL]
  
  list_to_delete <- NULL
  for(i in 1:ncol(tweets.dt))
  {
    if(length(unique(tweets.dt[, get(names(tweets.dt)[i]) ]))<2)
    {
      list_to_delete<-c(list_to_delete,names(tweets.dt)[i])
    }
  }
  tweets.dt[,(list_to_delete):=NULL]
  # print(tweets.dt)
  return (tweets.dt)
}

cols_for_shaping<- function(tweets.dt){
  list_to_return <- list(colour=NULL,
                         size=NULL)
  for(i in 1:ncol(tweets.dt))
  {
    if(  length(unique(tweets.dt[, get(names(tweets.dt)[i]) ]))<9  |
             is.numeric(tweets.dt[, get(names(tweets.dt)[i]) ])  )
    {
      list_to_return$colour<-c(list_to_return$colour,names(tweets.dt)[i])
    }
    if(is.numeric(tweets.dt[,get(names(tweets.dt)[i])]))
    {
      list_to_return$size<-c(list_to_return$size,names(tweets.dt)[i])
    }
  }
  list_to_return <- lapply(list_to_return,grep,pattern="lon|lat",list_to_return,value=T,invert=T)
  
 return(list_to_return)
}


get_colours <- function(tweets.dt,str_column_names){
      
  tweets.dt[is.na(get(str_column_names)),c(str_column_names):="No Informado"]
      
  if(is.numeric(tweets.dt[[str_column_names]]))
      {
         tweets.dt$color <- colorRampPalette(c('blue', 'red'))(length(tweets.dt[[str_column_names]]))[rank(tweets.dt[[str_column_names]])]
      }else{
        if(length(levels(factor(tweets.dt[[ str_column_names ]]) ) ) == 2) {
          palet.color <- data.table(label = levels(factor(tweets.dt[[str_column_names]])),
                                    color = c( "#1B9E77", "#D95F02"))
        }
        if(length(levels(factor(tweets.dt[[ str_column_names ]]) ) ) == 1) {
          palet.color <- data.table(label = levels(factor(tweets.dt[[str_column_names]])),
                                    color = c( "#1B9E77"))
        }
        if(length(levels(factor(tweets.dt[[ str_column_names ]]) ) ) > 2){
          palet.color <- data.table(label = levels(factor(tweets.dt[[str_column_names]])),
                                    color = I(brewer.pal(nlevels(factor(tweets.dt[[str_column_names]])), name = 'RdBu')))
          }
        setkeyv(tweets.dt, str_column_names)
        setkey(palet.color, "label")
        tweets.dt[,c("color"):=NULL]
        tweets.dt <- tweets.dt[palet.color]
      }
      return(tweets.dt)
}

updateMap <- function(tweets.dt,str_size_col,radius){
  mapa_leaflet <- leaflet() %>%
    addTiles() %>%  
    addCircleMarkers(lat=tweets.dt[,get("lat")], lng=tweets.dt[,get("lon")], popup=paste(tweets.dt[,get("color")],tweets.dt[,get("color")]),
                     stroke = F, 
                     # weight = 5,
                     radius = scale(tweets.dt[[str_size_col]],center=F)*radius,
                     fill = TRUE, 
                     fillColor = tweets.dt$color, 
                     fillOpacity = 0.5) 

  return(renderLeaflet(mapa_leaflet) )

}

#               INICIO                  #############################################################

tweets.dt <- super_parseTweets()
cols_ <- cols_for_shaping(tweets.dt)
# print(tweets.dt)
# str_column_names=cols_$colour[1]
# str_column_names="in_reply_to_user_id_str"
str_column_names="country"
tweets.dt <- get_colours(tweets.dt,str_column_names)
tweets.dt <- get_colours(tweets.dt,cols_$colour[1])

# print(tweets.dt)
