library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.5,chunksize=1e+08,maxmemory=1e+09)


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

print('Reading files to process...')
fls <- read.csv('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/toberesamped.csv', stringsAsFactors = F)


covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
# fls <- list.files(covDir, pattern = '.tif$', full.names = T)
# print(paste0('Trying to load a raster'))
# r <- raster(fls[138])
# print(r)

print(Sys.info()[["user"]])
tPath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_aspect.tif'

          
#tPath <- '/datasets/work/af-digiscapesm/work/Ross/Covariates/Relief_dem_foc2.tif'
file.exists(tPath)
print(paste0('Loading template ', tPath))
templateR <- raster::raster(tPath)
print(templateR)
outRoot <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m2'
  
               
tic()

fname <- fls[k,1]

print(fname)
inR <- raster(fname)


r2 <- resample(inR, templateR, method = 'ngb')
crs(r2)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
print(paste0('Writing raster - ', paste0(outRoot, '/',basename(fname))))
writeRaster(r2, filename=paste0(outRoot, '/',basename(fname)), overwrite=T)

toc()
print(paste0('Finished Successfully'))
