library(sp);library(rgdal);library(raster);library(tictoc);library(sf)
rasterOptions(progress = 'text', maxmemory = 214748364, todisk = TRUE, tmpdir='e:/rtmp')

inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/Veg_FPAR_Mean.tif')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/Veg_FPAR_Mean.tif')

inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/Veg_FPAR_Median.tif')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/Veg_FPAR_Median.tif')

inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/Veg_FPAR_Min.tif')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/Veg_FPAR_Min.tif')


inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/Veg_preEuropeanVeg.tif')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/Veg_preEuropeanVeg.tif')

inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/VEG_PERSISTANT_GREEN_VEG.TIF')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/VEG_PERSISTANT_GREEN_VEG.TIF')

inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/SOIL_ASC.TIF')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/SOIL_ASC.TIF')


inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/Veg_LandCoverTrend_evi_class.tif')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/Veg_LandCoverTrend_evi_class.tif')

inR<- raster('M:/work/datasets/national/covariates/mosaics/90m/Veg_LandCoverTrend_evi_mean.tif')
disaggregate(inR, fact=3, filename='E:/temp/FPAR/Veg_LandCoverTrend_evi_mean.tif')
