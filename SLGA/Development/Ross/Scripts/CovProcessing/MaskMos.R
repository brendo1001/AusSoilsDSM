library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.5, tmpdir = 'E:/rtmp' )

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])

print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/Resampled'
  #outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/MaskedNew'
  templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Resampled'
  outDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/MaskedNew'
  templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}


tic()
fls <- list.files(path = paste0(inDir), full.names = T, recursive = F, pattern = '.tif$')


rPath <- fls[k]
n <- basename(rPath)
outfile <- paste0(outDir, '/', n)

if(!file.exists(outfile)){

    templateR <- raster(templatePath)
    r <- raster(rPath)
    rz<- mask(r, templateR , filename = outfile, overwrite=T)

}else{
  
  print(paste0("File exists - ", outfile))
}

toc()
print(paste0('Finished Successfully'))
