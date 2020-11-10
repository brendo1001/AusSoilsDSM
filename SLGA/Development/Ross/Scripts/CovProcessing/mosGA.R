library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.5)

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
print(paste0('K = ', k))
tic()

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tiles_25m'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m'
  templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
 
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles_25m'
  outDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m'
  templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}


fls <- list.files(paste0(inDir, '/100'))
fil <- basename(fls)[k]
dirs <- list.dirs(inDir, recursive = F, full.names = T)

allfls <- paste0(dirs, '/', fil)

print('Loading rasters.....')
raster_list <- list()
for (i in 1:length(allfls)){
#for (i in 1:10){
  r1<- raster(allfls[i])
  raster_list <- append(raster_list, r1)
 # print(i)
}
print('Finished Loading rasters.....')

outfile <- paste0(outDir, '/mos_', fil)

raster_list$filename <- outfile
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)


toc()
print(paste0('Finished Successfully'))
