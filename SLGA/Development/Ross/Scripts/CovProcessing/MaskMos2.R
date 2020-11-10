library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', memfrac = 0.5)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/Resampled'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/MaskedNew'
  templatePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/FinalMosaics/Resampled'
  outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/FinalMosaics/Masked'
  templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

fls <- c('alpsbk_aust_y2009_sd5a2.tif',
         'alpsbk_aust_y2009_se5a2.tif',
         'lztmre_aus_y20002011_dm7a2_d20050630.tif'
         )



rPath <- paste0(inDir, '/', fls[k])
n <- basename(rPath)
outfile <- paste0(outDir, '/', n)

tic()

#if(!file.exists(outfile)){
  
  templateR <- raster(templatePath)
  r <- raster(rPath)
  rz<- mask(r, templateR , filename = outfile, overwrite=T)
  
# }else{
#   
#   print(paste0("File exists - ", outfile))
# }

toc()
print(paste0('Finished Successfully'))
