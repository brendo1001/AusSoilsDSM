library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', memfrac = 0.5)

inRP <- paste0('/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/wofs_Log.tif')
outR <- '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/FinalMosaics/wofs_Log.tif'

tic()

templatePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif'
templateR <- raster(templatePath)
inR <- raster(inRP)
r2 <- resample(inR, templateR, filename=outR, method = 'ngb')


toc()
print(paste0('Finished Successfully'))


