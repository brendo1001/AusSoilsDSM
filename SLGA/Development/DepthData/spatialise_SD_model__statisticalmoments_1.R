## Soil depth
## spatialising soil depth predictions: statistical moments


## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)


#what tiles are we working on?
#tile folders
#dpw<- "Z:/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/"  #directory pathway
dpw<- "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
fols
length(fols)


###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)



# get raster (this will change from tile to tile)
oper1<- foreach(i=1:250, .packages = c("raster", "sp", "rgdal")) %dopar% {
  
  # select tile folder 
  sfol<- fols[i]
  

  #general path to the rasters
  nm2<- paste0(dpw, sfol)
  
  ## Mapped predictions
  preds<- list.files(path = nm2, full.names=T, all.files = T,recursive = T, pattern = "soilDepth_pred.tif") 
  preds
  
  # stack predictions
  s1<- stack()
  for (j in 1:length(preds)){
    s1<- stack(s1, raster(preds[j]))
  }
  
  ## Statistical moments
  #calculate quantiles
  f2<- function(x)(quantile(x,probs = c(0.1,0.5,0.9),na.rm=TRUE)) 
  predQuantile <- calc(s1, fun=f2)
  
  ## write quantile rasters to file
  
  #10th percentile
  r1<- predQuantile[[1]]
  names(r1)<- "tenthPercentile_SD"
  ras1<- paste0(nm2, "/tenthPercentile_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  
  #50th percentile
  r1<- predQuantile[[2]]
  names(r1)<- "median_SD"
  ras1<- paste0(nm2, "/median_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  
  #90th percentile
  r1<- predQuantile[[3]]
  names(r1)<- "nintiethPercentile_SD"
  ras1<- paste0(nm2, "/ninetiethPercentile_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  
  
  #calculate prediction greater than a particular threshold
  #10cm
  f3<- function(x) (sum(x >= 0.1)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh10cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  #30cm
  f3<- function(x) (sum(x >= 0.3)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh30cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  
  #50cm
  f3<- function(x) (sum(x >= 0.5)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh50cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  
  #100cm
  f3<- function(x) (sum(x >= 1)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh100cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  
  #150cm
  f3<- function(x) (sum(x >= 1.5)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh150cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  
  #200cm
  f3<- function(x) (sum(x >= 2)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh200cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  
  #300cm
  f3<- function(x) (sum(x >= 3)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh300cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  
  #500cm
  f3<- function(x) (sum(x >= 5)) 
  predRange <- calc(s1, fun=f3)
  f4<- function(x)(x/nlayers(s1))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh500cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  
  #write tile diognostic out
  write.table(nm2, 
              file = paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/slurm/slurmCheck/", sfol, "_tile.txt"),
              row.names = F, col.names = F, sep="")
              
}

### END

  
  
  
  
  
 
  
  
  
 