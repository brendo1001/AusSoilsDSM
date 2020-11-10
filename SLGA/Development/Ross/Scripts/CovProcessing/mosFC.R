library(raster)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC/geoTiles'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC/FCMosaics'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC/geoTiles'
  outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC/FCMosaics'
}


# d <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FC/Tiffs/0_-19'
# fls <- list.files(path=d, recursive = T, pattern = '.tif', full.names = F )

types <- c('Max_BS', 'Max_NPV', 'Max_PV', 'Mean_BS', 'Mean_NPV', 'Mean_PV', 'Min_BS', 'Min_NPV', 'Min_PV', 'SD_BS', 'SD_NPV', 'SD_PV' )
fil <- types[k]
fls <- list.files(path=inDir, recursive = T, pattern = fil, full.names = T )
print('Loading rasters.....')
raster_list <- list()
for (i in 1:length(fls)){
  r1<- raster(fls[i])
  raster_list <- append(raster_list, r1)
  print(i)
}

raster_list <- list()
for (i in 5:9){
  r1<- raster(fls[i])
  raster_list <- append(raster_list, r1)
  print(i)
}


if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

outfile <- paste0(outDir, '/FC_Mosaic_', fil, '.tif')

raster_list$filename <- outfile
raster_list$datatype <- "INT1U"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE
raster_list$fun <- 'mean'
raster_list$ tolerance <- 1

print('Doing the mosaic.....')
# do the mosaic
mos <- do.call(mosaic, raster_list )


toc()
print(paste0('Finished Successfully'))





