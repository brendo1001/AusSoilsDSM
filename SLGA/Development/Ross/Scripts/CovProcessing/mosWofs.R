library(raster)
library(tictoc)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work'
  
}else{
  basePath <- '/datasets/work/af-digiscapesm/work'
}

tic()
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

source.root<- paste0(basePath, '/Ross/SLGAData/Wofs')

types <- c('wofs_Log.tif', 'wofs_Sum.tif')
fil <- types[k]


fls <- list.files(path=source.root, recursive = T, pattern = fil, full.names = T )

raster_list <- list()
#for (i in 1:length(fls)){
  for (i in 1:10){
  r1<- raster(fls[i])
  raster_list <- append(raster_list, r1)
  print(i)
}

outDir <- paste0(basePath, '/Ross/SLGAData/WofsMosaics')
if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
outfile <- paste0(outDir, '/', fil)

raster_list$filename <- outfile
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)


toc()
print(paste0('Finished Successfully'))

