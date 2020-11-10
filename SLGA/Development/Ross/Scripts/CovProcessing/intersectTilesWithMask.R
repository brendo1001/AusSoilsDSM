# BARE EARTH iMAGERY RESCALE AND REPROJECT



# libraries
library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
  k=1
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work'
  args = commandArgs(trailingOnly=TRUE)
  k = as.numeric(args[1])
}

tic()

print(paste0("Processing iteraration = ", k))

sq <- seq(1,20041,5)
ivl <- sq[k]


maskshp <- st_read(paste0( basePath, '/datasets/national/covariates/30mMask2.shp'))

tilesshp <- st_read(paste0( basePath, '/datasets/national/covariates/vectors/25m_tiles/allTiles_combined_25m.shp'))

CRS(maskshp) <- CRS(tilesshp)

st_crs(maskshp)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
st_crs(tilesshp)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')


ts <- st_intersects(tilesshp,maskshp, sparse = FALSE)
ts1<-apply(ts, 1, any)
tilesshp$inter <- ts1

st_write(tilesshp, paste0( basePath, '/datasets/national/covariates/vectors/25m_tiles/allTiles_combined_25m_inter.shp'), delete_layer=T)
st_write(tilesshp[tilesshp$inter,],  paste0( basePath, '/datasets/national/covariates/vectors/25m_tiles/allTiles_combined_25m_interOnly.shp'), delete_layer=T)

nrow(tilesshp[tilesshp$inter,])
plot(tilesshp[tilesshp$inter,])
