library(raster)


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
}else{
  basePath <- '/datasets/work/af-tern-mal-debv/work'
}
source.root<- paste0(basePath, "/datasets/national/covariates/landcover/GA_bare_earth")

source.folders<- list.dirs(path = source.root, full.names = T, recursive = F )
source.folders

raster_list <- list()
fls <- list.files(path='//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/landcover/GA_bare_earth', recursive = T, pattern = 'BLUE.tif$', full.names = T )


for (i in 1:length(source.folders)){
  fpath1<- paste0(source.folders[i], '/BLUE.tif')
  r1<- raster(x = fpath1)
  raster_list <- append(raster_list, r1)
  print(i)
}


raster_list$filename <- 'e:/temp/blue.tif'
raster_list$datatype <- "INT2U"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)
