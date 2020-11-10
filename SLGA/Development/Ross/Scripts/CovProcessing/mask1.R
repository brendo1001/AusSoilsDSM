library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.5)

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
inDir = args[2]
outDir = args[3]
print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  
  #inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m'
  #outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m'
  templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}else{
  #inDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m'
  #outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/New/HeatMask'
  templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}



fls <- list.files(path = paste0(inDir), full.names = T, recursive = F, pattern = '.tif$')

rst <- fls[k]
n <- basename(rst)
outfile <- paste0(outDir, '/', n)

templateR <- raster(templatePath)
r <- raster(rst)
r2 <- resample(r, templateR , method='ngb')
rz<- mask(r2, templateR , filename = outfile, datatype='FLT4S', overwrite=T)


