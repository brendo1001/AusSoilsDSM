library(raster)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)
print(args)

k = args[1]
print(k)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work'
}
source.root <- paste0(basePath, "/datasets/national/covariates/landcover/GA_bare_earth")

source.folders <- list.dirs(path = source.root, full.names = T, recursive = F )
source.folders

source.files<- list.files(path = source.folders[1], full.names = F)
source.files

layer <- source.files[as.numeric(k)]


outFile <- paste0(basePath, "/datasets/national/covariates/landcover/GAMosaics/", layer)

if(!file.exists(outFile)){


print(paste0('Processing ', layer))
raster_list <- list()

for (i in 1:length(source.folders)){
  fpath1<- paste0(source.folders[i], '/', layer)
  print(fpath1)
  r1<- raster(x = fpath1)
  raster_list <- append(raster_list, r1)
  print(i)
}

raster_list$filename <- outFile
#raster_list$datatype <- "INT2U"
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)


print('Finished Successfully')

}else{
  print(paste0('Mosaic already exists - ', layer))
}

