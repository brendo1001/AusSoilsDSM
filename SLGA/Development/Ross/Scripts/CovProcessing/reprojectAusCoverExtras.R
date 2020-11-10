library(raster)
library(parallel)

rasterOptions(progress = 'text', tmpdir= 'e:/temp/rtmp', maxmemory = 1e+09)

r <- raster('E:/Auscover/alpsbk_aust_y2009_sd5a2.tif')
m <- raster('E:/temp/mask30.tif')

rz<- projectRaster(from = r, to = m ,method = "ngb", filename = 'E:/Auscover/AusCoverResamp/alpsbk_aust_y2009_sd5a2.tif', datatype='INT2U', overwrite=T)



