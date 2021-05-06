
library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', maxmemory = 2147483648)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/Masked'
  outDir <- paste0('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/MaskedP')
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked'
  outDir <- paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/MaskedP')
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))


tic()

fls<- list.files(basePath, recursive = F, full.names = F, pattern = '*.tif$')

inR <- raster(paste0(basePath, '/', fls[k]))
outname <- paste0(outDir, '/', fls[k])

crs(inR)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')

writeRaster(inR, paste0(outname), overwrite=F)
