library(raster)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC/FCMosaics'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC'
  outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC/FCMosaics'
}


# d <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FC/Tiffs/0_-19'
# fls <- list.files(path=d, recursive = T, pattern = '.tif', full.names = F )

types <- c('Max_BS', 'Max_NPV', 'Max_PV', 'Mean_BS', 'Mean_NPV', 'Mean_PV', 'Min_BS', 'Min_NPV', 'Min_PV', 'SD_BS', 'SD_NPV', 'SD_PV' )
fil <- types[k]
print('Loading rasters.....')

fls <- list.files(path=paste0(inDir, '/geoTiles'), recursive = T, pattern = fil, full.names = T )

#fls <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC/geoTiles/*.tif'
tmpD <- paste0(inDir, '/', fil)
if(!dir.exists(tmpD)){dir.create(tmpD)}
#file.copy(fls, tmpD, overwrite = F)

v<-gdalUtils::gdalbuildvrt(paste0(tmpD, '/*.tif'), paste0(inDir, '/', fil, '.vrt'), separate=F )
#gdalUtilities::gdalbuildvrt(gdalfile = paste0(tmpD, '/*.tif'), output.vrt = paste0(inDir, '/', fil, '.vrt'))
r <- raster(paste0(inDir, '/', fil, '.vrt'))
writeRaster(r,paste0(inDir, '/FC_', fil, '.tif'), overwrite=T )

