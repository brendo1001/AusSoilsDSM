library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.5, tmpdir = 'E:/rtmp' )

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])

print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  
  #inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m'
  #outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m'
  templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC/FCMosaicsResample'
  outDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked'
  templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}



fls <- list.files(path = paste0(inDir), full.names = T, recursive = F, pattern = '.tif$')

rst <- fls[k]
n <- basename(rst)
outfile <- paste0(outDir, '/', n)


templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
templateR <- raster(templatePath)

r <- raster(rPath)
rs <- resample(r, templateR, method='ngb')
rz<- mask(rs, templateR , filename = 'M:/work/Ross/FC/FCMosaicsFilled/mask_FC_Mean_PV.tif', datatype='FLT4S', overwrite=T)


