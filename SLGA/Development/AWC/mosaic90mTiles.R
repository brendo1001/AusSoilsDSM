library(raster)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)
print(args)

k = as.numeric(args[1])

if(k<7){
  itr = k
  att = 'DUL'
}else{
  itr = k-6
  att = 'DLL'
}
print(k)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '/scratch1/sea084/SLGA/AWCRF'
}else{
  basePath <- '/scratch1/sea084/SLGA/AWCRF'
}

source.folders <- list.dirs(path = basePath, full.names = T, recursive = F )
source.files<- list.files(path = basePath, full.names = T, recursive = T)


idxs <- which(grepl(paste0(att, '_', itr, '.tif$'), source.files))
inFls <- source.files[idxs]


outFile <- paste0('/scratch1/sea084/SLGA/', att, "_", itr, '.tif')

print('Loading rasters.....')
raster_list<-lapply(inFls, raster)
raster_list$filename <- outFile
#raster_list$datatype <- "INT2U"
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE


print('Mosaicing.....')
# do the mosaic
mos <- do.call(raster::merge, raster_list)
print(paste0('Finished Successfully'))
