
#################################
###  Author : Ross Searle         
###  Date : Thu Oct 15 11:50:47 2020                      
###  Project : TERN Landscapes
###  Purpose : Drill the covariate Tiles on the HPC
#################################

# Use this command to merge the dataframes once the HPC has done its stuff
# dfm <- do.call('rbind', lapply(list.files('Y:/Ross/SLGAData/TileDrill/o_asc_ord', full.names = T), readRDS))

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

sitesPath <-args[2]
xField <- args[3]
yField <- args[4]
training.fieldname <- args[5]
IDField <- args[6]


print(sitesPath)
print(xField)
print(yField)
print(training.fieldname)
print(IDField)

# sitesPath <-'Y:/Ross/SLGAData/TileDrill/ASCs.csv'
# xField <- 'o_longitude_GDA94'
# yField <- 'o_latitude_GDA94'
# training.fieldname <- 'o_asc_ord'
# IDField <- 'SID'

#k=500

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work'
  malPath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates'
}else{
  basePath <- '/datasets/work/af-digiscapesm/work'
  malPath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates'
}



#Directory
#root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates'
data.directory = paste0(malPath, '/tiles')
outcome.directory = paste0(basePath, '/Ross/SLGAData/TileDrill')
outDir = paste0(outcome.directory, '/', training.fieldname)
borders.filename = paste0(basePath, '/External/Covariates/metadata/allTiles_combined_RASTER.shp')

if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

##########################################################################
###       Initialise
##########################################################################

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

outFile <- paste0(outDir, '/Tile_', tiles[k], '.rds')
print(outFile)

#Read in borders shape file
border = st_read(borders.filename)
soil.data <- read.csv(sitesPath, stringsAsFactors = F)

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

#Drill if tile has data points
if (length(idxs) != 0) { 
  
  covariates = stack(covariates.file)
  
  inPts = soil.data[idxs, ]
  training.points <- data.frame(ID=inPts[IDField], Xf=inPts[xField], Yf=inPts[yField], Yf=inPts[training.fieldname], row.names = NULL)
  training.points <- data.frame(Xf=inPts[xField], Yf=inPts[yField], row.names = NULL)
  colnames(training.points) <- c('X', 'Y')
  head(training.points)
  #coordinates(training.points)  <- ~Longitude+Latitude
  
  #Extracting samples
  covariate.training = extract(covariates, training.points$X, training.points$Y, df=T)
  input = data.frame(inPts[IDField],inPts[xField], inPts[yField], inPts[training.fieldname], covariate.training[-1])
  
  #Save relevant RDS
  saveRDS(input, outFile )
  print(paste0('Number of sampes = ', nrow(input)))
}  else{
  print('Number of sampes = 0')
}


print(paste0("Fininished tile: k = ", k)  )
print(paste0('Finished Successfully'))

