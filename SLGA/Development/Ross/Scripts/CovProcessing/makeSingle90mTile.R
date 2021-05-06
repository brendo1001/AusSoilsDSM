library(raster)


inMos <- raster('M:/work/datasets/national/covariates/mosaics/90m/Clim_Prescott_LindaGregory.tif')
inMask <- raster('M:/work/datasets/national/covariates/tiles/3136/Relief_dems_3s_mosaic1.tif')
crop(inMos, inMask,  filename='M:/work/datasets/national/covariates/tiles/3136/Clim_Prescott_LindaGregory.tif')

inMos <- raster('M:/work/datasets/national/covariates/mosaics/90m/Clim_Prescott_LindaGregory.tif')
inMask <- raster('M:/work/datasets/national/covariates/tiles/3250/Relief_dems_3s_mosaic1.tif')
crop(inMos, inMask,  filename='M:/work/datasets/national/covariates/tiles/3250/Clim_Prescott_LindaGregory.tif')



