


library(raster)
library(sf)
library(fasterize)
library(rgdal)
library(tictoc)
library(gdalUtils)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  rootDir <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData'
  bPath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}'
}else{
  rootDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGAData'
  bPath <- '/datasets/work/af-tern-mal-deb'
}

tic()


bdys <- st_read (paste0(rootDir, '/GIS/Boundaries/LGA_Ozt.shp'))
templateR <- raster(paste0(bPath, '/work/datasets/national/covariates/mosaics/30m/dem1sv1_0.tif'))
tep <- fasterize(bdys, templateR, 'isin')
writeRaster(tep,paste0(bPath, '/work/datasets/national/covariates/mosaics/30m/mask30.tif') )


#e <- extent(templateR)
#m <- gdalUtils::gdal_rasterize( bdys, 'e:/temp/mask30m.tif', output_Raster=TRUE, tr=c(0.02777778, 0.02777778), te=c(e@xmin, e@ymin, e@xmax, e@ymax))



toc()
print(paste0('Finished Successfully'))







