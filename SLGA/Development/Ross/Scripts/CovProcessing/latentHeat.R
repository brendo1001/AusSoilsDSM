library(raster)
li

inDir <- 'E:/temp/Heat/H_MOD_DAY_4dim_3ord_Spatial_Temporal'

#inDir <- '//osm-12-cdc.it.csiro.au/OSM_CBR_LW_SATSOILMOIST_source/SMOS/L2SM/AU_daily'
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
    #r[r==0]<-NA
    stk <- addLayer(stk, r)
  }
}


detectCores()
beginCluster(n=2)

d <- clusterR(stk, fun=calc, args=list(fun=mean, na.rm=T), filename='E:/temp/lat.tif', overwrite=T)
endCluster()
