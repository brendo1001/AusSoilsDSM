library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.7)

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
print(paste0('K = ', k))

print('Loading rasters.....')

basePath <- '/datasets/work/af-tern-mal-deb/work/Ross'
p <- paste0(basePath, '/New/Auscover/Tiles/alpsbk_aust_y2009_sd5a2')
fls <- list.files(path=p, recursive = T, pattern = '.tif$', full.names = T )

raster_list <- list()
#for (i in 1:length(fls)){
for (i in 1:10){
  r1<- raster(fls[i])
  raster_list <- append(raster_list, r1)
  print(i)
}

outfile <- paste0(basePath, '/New/Auscover/mos_alpsbk_aust_y2009_sd5a2.tif')

r <- raster(paste0(basePath, '/New/Auscover/Tiles/alpsbk_aust_y2009_sd5a2.vrt'))
writeRaster(r,outfile )




# raster_list$filename <- outfile
# raster_list$datatype <- "FLT4S"
# raster_list$format <- "GTiff"
# raster_list$overwrite <- TRUE
# raster_list$na.rm <- TRUE
# 
# # do the mosaic
# mos <- do.call(merge, raster_list)
# 
# 
