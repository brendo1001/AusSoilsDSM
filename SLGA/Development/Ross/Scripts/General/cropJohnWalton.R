library(raster)
library(sf)

bdy <- st_read('Y:/Ross/temp/Walton/LA_BB_GCS_GDA94.shp')
plot(bdy)
b<-as.numeric(st_bbox(bdy))
ext <- extent(c(b[1], b[3], b[2], b[4]) )

fls <- list.files('M:/work/datasets/national/covariates/mosaics/30m/Masked', pattern = 'tif$', full.names =T)
outDir <- 'e:/temp/jw'

for (i in 1:length(fls)) {
  print(i)
  inRp <- fls[i]
  
  outF <- paste0(outDir, '/', basename(inRp))
  
  if(!file.exists(outF)){
    inR <- raster(inRp)
    crs(inR) <-  CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    tm <- crop(inR,ext)
    crs(tm) <-  CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    outF <- paste0(outDir, '/', basename(inRp))
    writeRaster(tm,filename=outF,format = "GTiff", overwrite = TRUE )
  }
}

