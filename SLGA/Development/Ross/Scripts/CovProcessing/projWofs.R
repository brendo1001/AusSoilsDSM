library(raster)
library(tictoc);

rasterOptions(progress = 'text', memfrac = 0.5)



inP <- '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/wofs_Log.tif'
templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'

templateR <- raster(templatePath)
inR <- raster(inP)

rz<- projectRaster(from = inR,to = templateR,method = "ngb", filename = '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/wofs_Log_proj.tif' , overwrite=T)

toc()
print(paste0('Finished Successfully'))