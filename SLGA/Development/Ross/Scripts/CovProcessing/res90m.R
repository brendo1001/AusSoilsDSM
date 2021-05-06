library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', memfrac = 0.5)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
   slgaRoot <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FPAR'
   outRoot <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FPARes'
   templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}else{
 
  slgaRoot <- '/datasets/work/af-digiscapesm/work/Ross/SLGAData/FPAR'
  outRoot <- '/datasets/work/af-tern-mal-deb/work/Ross/SLGAData/FPARes'
  templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/MaskedP/mask30.tif'
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

templateR <- raster(templatePath)

tic()

fls <- list.files(slgaRoot, recursive = F, full.names = F, pattern = '*.tif$')

inR <- raster(paste0(slgaRoot, '/', fls[k]))
print(fls[k])

r2 <- resample(inR, templateR, method = 'ngb')
crs(r2)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
writeRaster(r2, filename=paste0(outRoot, '/',fls[k]), overwrite=T)

toc()
print(paste0('Finished Successfully'))


