

r<- raster('M:/work/Ross/Heat/mosStats/B_MOD_DAY_4dim_3ord_Spatial_Temporal_med.tif')
hist(r)
quantile(na.omit(r[]), c(0.05, 0.95)) 

r2 <- clamp(r, lower=0, upper=7, useValues=TRUE)
plot(r2)


writeRaster(r2, 'M:/work/Ross/Heat/mosStats/B_MOD_DAY_4dim_3ord_Spatial_Temporal_med_Clipped.tif')



eta <- raster('M:/work/Ross/New/Heat/ETa_MOD_DAY_4dim_3ord_Spatial_Temporal_mean.tif')
H <- raster('M:/work/Ross/New/Heat/H_MOD_DAY_4dim_3ord_Spatial_Temporal_mean.tif')

B <- H/eta
#plot(B)
#hist(B)
quantile(na.omit(B[]), c(0.05, 0.95)) 
r2 <- clamp(B, lower=0.3, upper=4.8, useValues=TRUE)
plot(r2)
writeRaster(r2,'M:/work/Ross/New/Heat/Bowen_Ratio_mean.tif', overwrite=T)
