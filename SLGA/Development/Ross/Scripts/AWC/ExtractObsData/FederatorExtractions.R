library(httr)
library(jsonlite)
library(raster)
library(sf)


usr <- 'ross.searle@csiro.au'
key <- 'a'

workDir <- 'Z:/Ross/TERN/AWC/ObsDataSDF'

# Get the available datasets
datasetsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
datasets<-datasetsDF$DataSet

groups <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups')

props <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties')

str(props)

PSAProps <- props[props$PropertyGroup == 'PSA', ]
write.csv(PSAProps, paste0(workDir, '/PSA_ALL.csv'))
# Manually edited to add PSA groups
allPropCodes <- read.csv(paste0(workDir, '/PSA_ALL.csv'))

ClayCodes <- allPropCodes[allPropCodes$PSAComponent=='CL',]$Property



#iterate through the data sets to get locations where there is any soil data

res <- vector("list", length = length(datasets))
# Iterate Datasets
for (j in 1:length(datasets)) {
  d <- datasets[j]
  print(paste0(d, ' : ', d))
  url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/ObservationLocations?DataSet=", d, "&format=json&usr=", usr, "&key=", key))
  resp <- GET(url, timeout(300))
  response <- content(resp, "text", encoding = 'UTF-8')
  odf <- fromJSON(response)
  
  # If there is data returned slot it into the list
  if(is.data.frame(odf)){
    if(nrow(odf)>0){
      res[[j]] <- odf
    }
  }
}
outDF = as.data.frame(data.table::rbindlist(res, fill=T))
write.csv(outDF, file='C:/Projects/Small/soilsites/soilLocs.csv', row.names = F)

ptsDF <- na.omit(data.frame(Longitude=outDF$Longitude, Latitude=outDF$Latitude))
pts = st_as_sf(ptsDF, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(pts))
st_write(pts, 'C:/Projects/Small/soilsites/soilLocs.shp')
