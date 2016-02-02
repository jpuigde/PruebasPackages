require(data.table)
require(RJSONIO)

geocodeAdddress <- function(address) {
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}


certificats_catalunya_1 <- fread("../data/open_data_catalunya/icaen_cert_catalunya.csv")

certificats_catalunya_1[,addressa:=paste(`TIPUS VIA` ,`NOM VIA`,",",NÚMERO,",",POBLACIÓ)]
n=nrow(certificats_catalunya_1)
n=1000
n/5
i=1

if(i==1) lat_long <-data.frame(lat=NA,lon=NA)
system.time(
{
  for(i in 1:n){
  lat_lon_ <- geocodeAdddress(certificats_catalunya_1$addressa[i])
  lat_long[i,] <- lat_lon_  }
  }
)


street2coordinates(certificats_catalunya_1$addressa[1])




nrow(certificats_catalunya_1)/(5*3600)
certificats_catalunya_1[POBLACIÓ=="Sabadell"&`NOM VIA`=="Josep Esquirol",]




