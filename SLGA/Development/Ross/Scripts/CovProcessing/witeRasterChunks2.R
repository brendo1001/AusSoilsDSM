library(raster)
library(tictoc)
rasterOptions(progress = 'text')


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/Masked'
  outDir <- paste0('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/MaskedP')
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked'
  outDir <- paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/MaskedP')
}

fls<- list.files(basePath, recursive = F, full.names = F, pattern = '*.tif$')


for (i in 6:50) { 
  inR <- raster(paste0(basePath, '/', fls[i]))
  outname <- paste0(outDir, '/',  fls[i])
  print(paste0("Processing iteraration = ", i))
  crs(inR)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
  writeRaster(inR, filename=outname,datatype=dataType(inR))
}

