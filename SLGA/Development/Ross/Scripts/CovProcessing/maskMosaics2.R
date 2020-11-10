library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', memfrac = 0.5)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m'
  templateR <- raster('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work//datasets/national/covariates/mosaics/30m/Masked/mask30.tif')
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m'
  templateR <- raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked/mask30.tif')
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

outDir <- paste0(basePath, '/MaskedCheck')

tic()

fls<- list.files(basePath, recursive = F, full.names = F, pattern = '*.tif$')

inR <- raster(paste0(basePath, '/', fls[k]))

res <- compareRaster(inR, templateR, stopiffalse = F )

if(!res){
  
  e1 <- extent(inR)
  eT <- extent(templateR)
  if(e1@xmin<eT@xmin){
    print(paste0('Extending ', fls[k]))
    ri <- extend(templateR, inR)
  }else{
    print(paste0('Cropping ', fls[k]))
    ri <- crop(inR, templateR)
  }
 # r2 <- resample(inR, templateR)
}else{
  print(paste0('Extents match for ', fls[k]))
  r2 <- inR
}


print('Applying mask.....')
mask(r2, templateR, overwrite=T, outfile=paste0(outDir, '/',fls[k] ))


toc()
print(paste0('Finished Successfully'))


