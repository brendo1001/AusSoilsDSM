library(raster)
library(sf)
library(fasterize)
library(rgdal)
library(tictoc)


gdalUtilities::gdalinfo(paste0(bPath, '/work/datasets/national/covariates/mosaics/30m/dem1sv1_0.tif'))

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
outF <- paste0(bPath, '/work/datasets/national/covariates/mosaics/30m/msk.tif')

e <- extent(templateR)
gdalUtils::gdal_setInstallation(search_path =  '/apps/gdal/2.4.4/bin')
m <- gdalUtilities::gdal_rasterize( bdys, outF, a='isin', tr=c(0.02777778, 0.02777778), te=c(e@xmin, e@ymin, e@xmax, e@ymax))

m <- gdalUtilities::gdal_rasterize( bdys, outF, a='isin', tr='0.02777778, 0.02777778', dryrun = T)

rr <- raster(extent(templateR),resolution = res(templateR), crs = crs(templateR))
gdalUtils::gdalinfo(paste0(bPath, '/work/datasets/national/covariates/mosaics/30m/dem1sv1_0.tif'))

toc()
print(paste0('Finished Successfully'))
