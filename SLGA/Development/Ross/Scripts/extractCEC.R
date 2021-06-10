library(rgdal)
library(sf)
library(aqp)
library(jsonlite)
library(SoilDataFeder8R)
library(sf)

machineName <- as.character(Sys.info()['nodename'])
#if(!asPkg){
if(machineName=='soils-discovery'){
  source(paste0('/srv/plumber/TERNLandscapes/SoilDataFederatoR/Backends.R'))
}else{
  source(paste0('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Backends.R'))
}
#}


usr <- 'ross.searle@csiro.au'; key <- 'a';

### just checking out the 15F1 issue - turns out it is now 15F3
DataSet='NSWGovernment'

df <- as.data.frame(getSoilData(DataSets='NSWGovernment', observedProperty='15F3', usr='ross.searle@csiro.au', key='a'))
locs <- unique(df[c("Longitude", "Latitude", "Dataset", "Location_ID")])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
head(pts)
length(unique(pts$Dataset))
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)


fromJSON("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Ion-exchange%20properties")



####  funtion Ross uses for extarcting from the SDF

sdfExtract <- function(att, props, datasets, outDir=NULL, usr='Demo', key='Demo'){
  
  # Iterate Properties
  cnt=0
  for(i in 136:length(props)){
    # Make an empty list to put individual results in for this property
    res <- vector("list", length = length(datasets))
    print(props[i])
    # Iterate Datasets
    for (j in 1:length(datasets)) {
      p <- props[i]
      d <- datasets[j]
      print(paste0(d, ' : ', p))
      url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
      resp <- GET(url, timeout(300))
      response <- content(resp, "text", encoding = 'UTF-8')
      odf <- fromJSON(response)
      
      # If there is data returned slot it into the list
      if(is.data.frame(odf)){
        if(nrow(odf)>0){
          res[[j]] <- odf
          cnt=cnt+1
        }
      }
    }
    # Merge the dataframes in the list into one dataframe
    outDF = as.data.frame(data.table::rbindlist(res, fill=T))
    write.csv(outDF, paste0(outDir, '/SDF_', att, '_', p, '.csv'))
    
  }
  return(paste0(cnt, ' datasets extracted'))
}



#### Get All the CEC methods
att='CEC'
datasetsDF <- fromJSON(paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?usr=', usr, '&key=', key))
datasets<-datasetsDF$DataSet
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Ion-exchange%20properties"))
resp <- GET(url, timeout(300))
response <- content(resp, "text", encoding = 'UTF-8')
propRecs <- fromJSON(response)
props <- propRecs$Property

workDir <- 'c:/temp/CECsdf'
sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


file_names <- list.files('c:/temp/CECsdf', full.names = T) #where you have your files

cdf <- do.call(rbind,lapply(file_names,read.csv))

bboxExt <- extent(112,154,-44,-10)
idxs <- which(cdf$Longitude >= bboxExt@xmin & cdf$Longitude <= bboxExt@xmax & cdf$Latitude >= bboxExt@ymin & cdf$Latitude <= bboxExt@ymax)
length(idxs)
cdf <- cdf[idxs, ]



######  Make some maps

locs <- unique(cdf[c("Longitude", "Latitude", "Dataset", "Location_ID")])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
head(pts)
length(unique(pts$Dataset))
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)


methods <- unique(cdf$ObservedProperty)

cecIdxs <- c(1,3,15,21,27,32,37,43,48, 58,69, 73, 75, 76, 82, 85, 86,87,88,89, 90,92, 94,95,96,97)
cdcCodes <- methods[cecIdxs]

idcs <- which(cdf$ObservedProperty %in% cdcCodes)
cecrecs <- cdf[idcs,]
locs <- unique(cecrecs[c("Longitude", "Latitude", "Dataset", "Location_ID", 'ObservedProperty')])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
head(pts)
length(unique(pts$Dataset))
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)

plot(pts["ObservedProperty"],  axes = TRUE, pch=3)





