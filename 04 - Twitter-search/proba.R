# install.packages("twitteR")
# install.packages("ROAuth")
# install.packages("base64enc")
library(base64enc)
library(twitteR)
require(plyr)
library(ROAuth)

consumer_key <- "Anbf9IpnuSCy7E4ntV4sjIxzU"
consumer_secret <- "C5TcmcUBGaQEEw39rXaPiXL5naAecbtijukfRz7hxgJoTSl141"
access_token <- "2862529924-p8oOpk0fn08ThSDG9YrZxYglQmT0ZkRA9NR7BT5"
access_secret <- "AtHFmIP3ZUoKdUk5q1wAOa9hZJeHQgPWX0nlunTejIEh1"
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"


setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

tweets <- searchTwitter("#27s",n=100,geocode='41.3825,2.176944,1000mi')
tweets

tweets.df = ldply(tweets, function(t) t$toDataFrame() ) 



library(data.table)
library(streamR)
library(ROAuth)
library(textcat)
# consumer_key <- "Anbf9IpnuSCy7E4ntV4sjIxzU"
# consumer_secret <- "C5TcmcUBGaQEEw39rXaPiXL5naAecbtijukfRz7hxgJoTSl141"
# access_token <- "2862529924-p8oOpk0fn08ThSDG9YrZxYglQmT0ZkRA9NR7BT5"
# access_secret <- "AtHFmIP3ZUoKdUk5q1wAOa9hZJeHQgPWX0nlunTejIEh1"
# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"

# twitCred <- OAuthFactory$new(consumerKey=consumer_key,
#                              consumerSecret=consumer_secret,
#                              requestURL=requestURL,
#                              accessURL=accessURL,
#                              authURL=authURL)
# twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


# save.image("twitCred.Rdata")
load("twitCred.Rdata")

bpc <- fread("../data/bpc 47 lenguage list.csv")
setnames(bpc,names(bpc),c("code3","lang","lenguage"))
bpc <- bpc[lang!="",.(lang,lenguage)]
bpc <- unique(bpc)
setkey(bpc,lang)

file.remove("tweets.json")
# track = c("#27s","#27S", "junts pel si","@JuntsPelSi"), 
# locations=c(38, 0,42, 4), #Catalunya i balears
# locations=c(35.84, -9.50,42.73, 4.16), #peninsula iberica
# locations=c(40.38, 0.12,42.82, 3.47), #Catalunya

filterStream("tweets.json", 
             timeout = 100, 
             locations=c(0.12,40.38,3.47,42.82),
             oauth = twitCred
             )



tweets.dt <- as.data.table(parseTweets("tweets.json"))
tweets.dt$text <- iconv(tweets.dt$text,from = "UTF-8")
tweets.dt$text
setkey(tweets.dt,lang)
tweets.dt <- merge(tweets.dt,bpc)


# ############################################################################
# install.packages("ggmap")
library(ggmap)
library(sp)
library(tidyr)
MunicipisCataluna <- fread("../data/MunicipisCataluña.csv",sep=";",stringsAsFactors=T)
MunicipisCataluna$municipio <- iconv(MunicipisCataluna$municipio,from = "latin1")
p1[450104]

MunicipisCataluna[,c("x","y"):=list(as.numeric(gsub(",",".",lon,fixed = T)),as.numeric(gsub(",",".",lat,fixed = T)))]
MunicipisCataluna[,c("lat","lon"):=NULL]
MunicipisCataluna<-unite(MunicipisCataluna,region,c(municipio,compontnt),sep=" ",remove=FALSE)
MunicipisCataluna$provincia <- as.factor(as.numeric(MunicipisCataluna$codigoine) %/% 1000 )
background.map <- get_map(location=c(mean(MunicipisCataluna$x),mean(MunicipisCataluna$y)),source="google",maptype="toner",zoom=8)
mon.shapes <- geom_polygon(data=MunicipisCataluna,aes(x = MunicipisCataluna$x, y = MunicipisCataluna$y,group = region, fill=provincia),alpha=0.2,color=rgb(0,0,0,.3))


ggmap(background.map) +
  mon.shapes  +
  expand_limits(x = MunicipisCataluna$x, y = MunicipisCataluna$y)+
  geom_point(aes(y=tweets.dt$place_lat,x=tweets.dt$place_lon))+
  theme(legend.position="none")




# ############################################################################
# install.packages("leaflet")
library(RColorBrewer)
devtools::install_github("rstudio/leaflet")
library(leaflet)


palet.color <- data.table(label = levels(as.factor(tweets.dt[,get("place_type")])),
                          color = I(brewer.pal(nlevels(as.factor(tweets.dt[,get("place_type")])), name = 'Dark2')))

setkey(tweets.dt, "place_type")
setkey(palet.color, "label")

tweets.dt <- tweets.dt[palet.color]

m <- leaflet(data =tweets.dt) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lat=tweets.dt[,get("place_lat")], lng=tweets.dt[,get("place_lon")], popup=tweets.dt[,get("place_type")],
                   stroke = F, 
                   color = "#03F", 
                   weight = 5,
                   opacity = 0.7, 
                   fill = TRUE, 
                   fillColor = tweets.dt[,get("color")], 
                   fillOpacity = 0.5)
m  # Print the map



# ############################################################################








gsub("\\x","","Phenomenal | Eminem \xed\xa0\xbc\xed\xbe\xa7",fixed=TRUE)


tweets.df$lang2<- sapply(tweets.df$text,function (x) {try (textcat(x),silent=T)})



tweets.df$text[31]
tweets.df$lang

table(tweets.df$lang,tweets.df$lang2)



  for(i in 1:34)
  {
    
    try( textcat(tweets.df$text[i]),silent=T)
          print(i)
}

[23] "Phenomenal | Eminem \xed\xa0\xbc\xed\xbe\xa7"   



twitCred <- OAuthFactory$new(consumerKey=consumer_key,
                             consumerSecret=consumer_secret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake()

registerTwitterOAuth(twitCred)
6481831
setup_twitter_oauth

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
2361067
registerTwitterOAuth(twitCred)



scale()





x <- matrix( runif(10), ncol = 1)
(x-min(x))/(max(x)-min(x))
scale(x)
scale(x,center=F,scale=T)
scale(x,center=F,scale=F)+min(scale(x,center=T,scale=F))
(centered.x <- scale(x, scale = FALSE))
cov(centered.scaled.x <- scale(x)) # all 1
