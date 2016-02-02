require(data.table)
require(stringdist)
require(stringr)
require(ggplot2)
require(leaflet)
library(rgdal)

# require("shapefiles")
# read.dbf("../../data/Mapas_catalunya")

# Comunidades <- fread("../../data/Mapas_catalunya/Comunidades_utf8.csv")
# Comunidades$nombre
# ComunidadesP <- fread("../../data/Mapas_catalunya/ComunidadesP_utf8.csv")
MunicipisCatalunya <- fread("../../data/Mapas_catalunya/MunicipisCatalunya_utf8.csv")
MunicipisCatalunya[,municipio:=tolower(municipio) ]
MunicipisCatalunya[,c("lon","lat"):=list(as.numeric(gsub(",",".",lon) ),as.numeric(gsub(",",".",lat) ))]

# Comunidades <- fread("../../data/Mapas_catalunya/Comunidades.csv",encoding = "Latin-1")
# ComunidadesP <- fread("../../data/Mapas_catalunya/ComunidadesP.csv",encoding = "Latin-1")
# MunicipisCataluña <- fread("../../data/Mapas_catalunya/MunicipisCataluña.csv",encoding = "Latin-1")
# MunicipisCataluña[,provincia:=substr(codigoine,1,2)]
# MunicipisCataluña[,.N,by=provincia]
MunicipisCatalunya_centro <- MunicipisCatalunya[,.(lon=mean( as.numeric(gsub(",",".",lon) )),lat=mean(as.numeric(gsub(",",".",lat) ))),by= municipio]
MunicipisCatalunya_centro[,municipio:=tolower(municipio) ]


icaen_cert <- fread("../../data/open_data_catalunya/icaen_cert_catalunya.csv")
# icaen_cert[,provincia:=floor(`CODI POSTAL`/1000) ]
# icaen_cert[,.N,by=provincia]

# icaen_cert[provincia==7,]
icaen_cert[,poblacio:=str_trim(tolower(POBLACIÓ))]
icaen_cert[,municipio:=str_trim(paste(gsub("\\(|\\)","",ifelse(is.na(str_extract(poblacio,"\\(.*\\)")),"",str_extract(poblacio,"\\(.*\\)"))),
                                      gsub("\\s\\(.*\\)","",poblacio)))]

icaen_cert[,municipio:=str_trim(paste(gsub("\\,\\s","",ifelse(is.na(str_extract(municipio,"\\,.*")),"",str_extract(municipio,"\\,.*"))),
                                      gsub("\\,\\s.*","",municipio)))]

icaen_cert[,municipio:=str_trim(tolower(municipio))]
icaen_cert[,municipio:=gsub("'\\s","'",municipio)]
icaen_cert[,municipio:=gsub(" barà"," berà",municipio)]



if(0){
  # l' hospitalet de llobregat
  icaen_cert[municipio=="l\'hospitalet de llobregat",]
  MunicipisCatalunya_centro[municipio=="l\'hospitalet de llobregat",]
  grep("hospitalet",MunicipisCatalunya_centro$municipio,value=T)
  
  # roda de 
  unique(grep(" barà",icaen_cert$municipio,value=T))
  grep(" berà",MunicipisCatalunya_centro$municipio,value=T)
  
  # brull
  unique(grep("brull",icaen_cert$municipio,value=T))
  grep("brull",MunicipisCatalunya_centro$municipio,value=T)
}

icaen_cert_lat_lon <- merge( icaen_cert , MunicipisCatalunya_centro , by.x = "municipio",by.y = "municipio",all.x = T)


# icaen_cert_lat_lon[ is.na(lat), municipio]
miss_municipis <- icaen_cert_lat_lon[ is.na(lat), municipio]
length(miss_municipis)#266


for(i in 1:length(miss_municipis))
{
  if(i==1){POBLACIO <- MunicipisCatalunya_centro[which.min(stringdist(miss_municipis[i] ,MunicipisCatalunya_centro$municipio,method = c("dl"))),.(municipio,lat,lon)]
  }else{
    POBLACIO <- rbind(POBLACIO, MunicipisCatalunya_centro[which.min(stringdist(miss_municipis[i] ,MunicipisCatalunya_centro$municipio,method = c("dl"))),.(municipio,lat,lon)])
  }
  print(i/length(miss_municipis))
}

icaen_cert_lat_lon[ is.na(lat), ]$municipio <- POBLACIO$municipio
icaen_cert_lat_lon[ is.na(lat), ]$lon <- POBLACIO$lon
icaen_cert_lat_lon[ is.na(lat), ]$lat <- POBLACIO$lat

icaen_cert_lat_lon$QEP_factor <- as.factor(icaen_cert_lat_lon$`QUALIFICACIÓ ENERGIA PRIMARIA`)
icaen_cert_lat_lon$QEP_num  <- 8-as.numeric(icaen_cert_lat_lon$QEP_factor)

icaen_cert_mean_by_municipio <- icaen_cert_lat_lon[,.(QEP_mean=mean(QEP_num),mean_lat=mean(lat),mean_lon=mean(lon) ),by = municipio]

setkey(MunicipisCatalunya,"municipio")
setkey(icaen_cert_mean_by_municipio,"municipio")

poligons_qualificats <- MunicipisCatalunya[icaen_cert_mean_by_municipio]

poligons_qualificats[municipio %in% c("terrassa","matadepera")]
# GGPLOT
ggplot(poligons_qualificats)+
  geom_polygon(aes(lon,lat,order=order,group=paste(compontnt,municipio),fill=QEP_mean))+
theme(axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),plot.background=element_blank())+coord_map("ortho")


# leaflet----

layer <- ogrListLayers("../../data/Mapas_catalunya/poligonos_municipio_etrs89_catalunya")[1]
municipis <- readOGR("../../data/Mapas_catalunya/poligonos_municipio_etrs89_catalunya",
                     layer = layer, verbose = T)
municipis$QEP_mean<- NA
# str(municipis)
# nombre = municipis$nombre[1]
for( nombre in municipis$nombre)
{
  ind <- which(icaen_cert_mean_by_municipio$municipio == tolower(nombre) )
  if(length(ind)>0)   municipis$QEP_mean[municipis$nombre==nombre ] <- icaen_cert_mean_by_municipio$QEP_mean[ind]
}



leaflet(municipis) %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, 
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              popup=~paste("<table class='tg'>  <tr>  <td class='tg-yw4l'>",
                           "Municipio:"           ,"</td>  <td class='tg-yw4l'>",
                           nombre                 ,"</td>  </tr>  <tr>  <td class='tg-yw4l'>",
                           "Qualificacion Media: ","</td>  <td class='tg-yw4l'>",
                           QEP_mean               ,"</td>  </tr>  </table>"),
              color = ~colorQuantile("YlOrRd", municipis$QEP_mean)(QEP_mean)
              )




