
#################################
###  Author : Ross Searle         
###  Date : Thu Oct 15 11:50:47 2020                      
###  Project : TERN Landscapes
###  Purpose : 
#################################

##########################################################################
###       Packages
##########################################################################

library(raster)
library(doParallel)
library(sf)

##########################################################################
###       Inputs
##########################################################################

#HPC indexing
args = commandArgs(trailingOnly=T)
print(args)
k <- as.numeric(args[1])


k=1100

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work'
  malPath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates'
}else{
  basePath <- '/datasets/work/af-digiscapesm/work'
}

sitesPath <-'C:/Projects/DSM/ASC/ASCs.csv'

#Directory
#root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates'
data.directory = paste0(malPath, '/tiles')
outcome.directory = paste0(basePath, '/Ross/SLGAData/TileDrill')
RDS.directory = paste0(outcome.directory, '/RDS')
borders.filename = paste0(basePath, '/External/Covariates/metadata/allTiles_combined_RASTER.shp')



##########################################################################
###       Initialise
##########################################################################

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#Read in borders shape file
border = st_read(borders.filename)

soil.data <- read.csv(sitesPath, stringsAsFactors = F)
xField <- 'o_longitude_GDA94'
yField <- 'o_latitude_GDA94'


##########################################################################
###       Drill
##########################################################################

#Tif file location
covariates.file = list.files(paste0(data.directory, '/', tiles[k]), '.tif$', recursive = F, full.names = T )

#No spurious data
bboxExt.aus <- extent(110,153,-43,-9)
idxs.aus <- which(soil.data[xField] >= bboxExt.aus@xmin & soil.data[xField] <= bboxExt.aus@xmax & soil.data[yField] >= bboxExt.aus@ymin & soil.data[yField] <= bboxExt.aus@ymax)
soil.data <- soil.data[idxs.aus, ]

#Ensure data  within tile
sf = sf::st_as_sf(soil.data, coords = c(xField,yField))
st_crs(sf) = st_crs(border)
idxs= st_contains(border[(border$i==tiles[k]),], sf)[[1]]



plot(border[(border$i==tiles[k]),])
points(sf[idxs,])

#Drill if tile has data points
if (length(idxs) != 0) { 
  
  covariates = stack(covariates.file)
  
  training.points = soil.data[idxs, ]
  #coordinates(training.points)  <- ~Longitude+Latitude
  
  #Extracting samples
  covariate.training = as.data.frame(extract(covariates,training.points,method="simple"))
  input = data.frame(training.points[[training.id]], training.points@coords, training.points[[training.fieldname]], covariate.training)
  
  #Naming Columns
  colnames(input)[c(1,2,3,4)] <-c('SID','Easting','Northing', training.fieldname)
  
  #Save relevant RDS
  saveRDS(input, paste0(RDS.directory, '/Tile_', tiles[k], '.rds') )
  saveRDS(minValue(covariates), paste0(RDS.directory, '/ExtremeValues/Min_Tile_', tiles[k], '.rds'))
  saveRDS(maxValue(covariates), paste0(RDS.directory, '/ExtremeValues/Max_Tile_', tiles[k], '.rds'))
}  


cat("Fininished tile: k = ", k)  


