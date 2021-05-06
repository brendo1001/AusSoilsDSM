library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
  templateR <- raster('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work//datasets/national/covariates/mosaics/30m/mask30.tif')
  k=1
  start <- 10000
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work'
  templateR <- raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/mask30.tif')
  args = commandArgs(trailingOnly=TRUE)
  k = as.numeric(args[1])
  start = as.numeric(args[2])
}

tic()

print(paste0("Processing iteraration = ", k))


#sq <- seq(1,18359,5)
#ivl <- sq[k]

tilesshp <- st_read(paste0( basePath, '/datasets/national/covariates/vectors/25m_tiles/allTiles_combined_25m_interOnly.shp'))

mosaicsDir <- paste0(basePath, "/datasets/national/covariates/mosaics/30m")
source.files <- list.files(path = mosaicsDir, pattern = '*.tif$', full.names = T)

outDir <- paste0(basePath, '/datasets/national/covariates/tiles30m' )

reso <- res(templateR)[1]
buf <- reso*3


for (i in start:(start+50)) {
  

  
#  if(i<= nrow(tilesshp)){
    
    outDirSub<-(paste0(outDir, '/', i))
    if(!dir.exists(outDirSub)){dir.create(outDirSub)}
      
      
      tname <- paste0(outDirSub,"/cstone.tif")
      if(!file.exists(tname)){
          p <- st_geometry( tilesshp[i,])
          b<-as.numeric(st_bbox(p))
          ext <- extent(c((b[1]-buf), (b[3]+buf), (b[2]-buf), (b[4]+buf)) )
          tm <- crop(templateR,ext,filename = tname,format = "GTiff",datatype = "INT1U", overwrite = TRUE)
      }else{
        tm <- raster(tname)
      }
      
      for (j in 1:length(source.files)) {
        f <- source.files[j]
        outName <- basename(f)
        outfile <- paste0(outDirSub,"/",outName )
        if(!file.exists( outfile)){
            print(paste0( j, ' : ', i, ' : ', outName))
            inR <- raster(f)
            tc <- crop(inR,tm, filename =outfile ,format = "GTiff",datatype = dataType(inR), overwrite = TRUE)
        }else{
          print(paste0(outfile, ' exists'))
        }

      }
    
  }


toc()
print(paste0('Finished Successfully'))









