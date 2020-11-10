library(raster)
rasterOptions(progress = 'text')

# r <- raster('E:/temp/FC/FC_Min_NPV.tif')
# b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_Min_NPV.tif', overwrite=TRUE)

#r <- raster('E:/temp/FC/FC_min_BS.tif')
#b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_min_BS.tif', overwrite=TRUE)


#r <- raster('E:/temp/FC/FC_min_PV.tif')
#b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_min_PV.tif', overwrite=TRUE)

#r <- raster('E:/temp/FC/FC_mean_NPV.tif')
#b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_mean_NPV.tif', overwrite=TRUE)

# r <- raster('E:/temp/FC/FC_mean_BS.tif')
# b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_mean_BS.tif', overwrite=TRUE)

# r <- raster('E:/temp/FC/FC_max_PV.tif')
# b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_max_PV.tif', overwrite=TRUE)

# r <- raster('E:/temp/FC/FC_max_NPV.tif')
# b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_max_NPV.tif', overwrite=TRUE)

r <- raster('E:/temp/FC/FC_max_BS.tif')
b1 <- focal(r, w=matrix(1,3,3), fun=mean, na.rm=T, NAonly=T, progress='text', filename= 'e:/temp/FCFix/FC_max_BS.tif', overwrite=TRUE)

