library(raster)


rasterOptions(tmpdir='E:/temp/rtmp', maxmemory=1000000000)
rasterOptions()


#inDir <- '//osm-12-cdc.it.csiro.au/OSM_CBR_LW_SATSOILMOIST_source/SMOS/L2SM/AU_daily'
inDir <- 'E:/temp/AU_daily'
fls <- list.files(path = inDir, full.names = T, recursive = T, pattern = '.flt$')

stk <- stack()
for (i in 1:length(fls)) {
  f <- fls[i]
  print(i)
  if(inherits(res, "try-error"))
  {
    print(paste0('ERROR Loading - ', f ))
    next
  }else{
    r <- raster(f)
    r[r==0]<-NA
    stk <- addLayer(stk, r)
  }
}

print('Calculating stats.....')
ravg <- calc(stk, fun=mean, na.rm=T)
print('Finished calculating the mean')
rmin <- calc(stk, fun=min, na.rm=T)
print('Finished calculating the min')
rmax <- calc(stk, fun=max, na.rm=T)
print('Finished calculating the max')
rstd <- calc(stk, fun=sd, na.rm=T)
print('Finished calculating the standard deviation')

