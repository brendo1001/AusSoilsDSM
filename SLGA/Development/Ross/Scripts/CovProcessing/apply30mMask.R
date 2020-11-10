library(raster)
library(parallel)

rasterOptions(progress = 'text')

# detectCores()
# beginCluster(n=15)
# r <- raster('E:/temp/FCFix/FC_max_BS.tif')
# m <- raster('E:/temp/mask302.tif')
# d <- clusterR(r, fun=resample, args=list(y=m,  method='ngb'), filename='E:/temp/masked/FC_max_BS.tif', overwrite=T)
# endCluster()


r <- raster('E:/temp/FCFix/FC_min_NPV.tif')
m <- raster('E:/temp/mask302.tif')
#b <- raster('W:/work/datasets/national/covariates/mosaics/30m/dem1sv1_0.tif')
resample(r, m, method='ngb', filename='E:/temp/masked/FC_min_NPV.tif')
