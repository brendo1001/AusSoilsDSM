library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', memfrac = 0.5)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross'
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work/Ross'
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
att <- args[2] #'alpsbk_aust_y2009_sd5a2'
print(paste0("Processing iteraration = ", k))

outDir <- paste0(basePath, '/New/Auscover/Tiles/', att)
if(!dir.exists(outDir)){dir.create(outDir)}

tic()
shp <- st_read(paste0(basePath, '/intersquares.shp'))
inR <- raster(paste0(basePath, '/New/Auscover/', att, '.tif'))
p<-shp[k,]
st_crs(p)<-CRS('+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs ')
cr <- crop(inR, p)
pr2 <- projectRaster(cr, crs= CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '), res=0.0002777778, progress = 'text')



writeRaster(pr2, filename = paste0(outDir, '/geo_',k, '.tif'), overwrite=T)
plot(pr2)

toc()
print(paste0('Finished Successfully'))


