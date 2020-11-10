library(raster)
library(tictoc)

rasterOptions(progress = 'text')

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC/FCMosaicsFilled'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC'
  outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC/FCMosaicsFilled'
}

#rsts <- c('FC_SD_NPV.tif','FC_SD_PV.tif','FC_SD_BS.tif', 'FC_Min_PV.tif')
rsts <- c('FC_Mean_PV.tif')
#k=1
rst <- rsts[k]

infile <- paste0(inDir, '/', rst)
r <- raster(infile)
outfile <- paste0(outDir, '/', rst)
b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= outfile, overwrite=TRUE)

