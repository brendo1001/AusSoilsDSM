library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
  templateR <- raster('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work//datasets/national/covariates/mosaics/30m/Masked/mask30.tif')
  k=1
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work'
  templateR <- raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked/mask30.tif')
  args = commandArgs(trailingOnly=TRUE)
  k = as.numeric(args[1])
}

tic()

print(paste0("Processing iteraration = ", k))


step <- 5

sq <- seq(1,18359,5)
ivl <- sq[k]

tilesshp <- st_read(paste0( basePath, '/datasets/national/covariates/vectors/25m_tiles/allTiles_combined_25m_interOnly.shp'))

mosaicsDir <- paste0(basePath, "/datasets/national/covariates/mosaics/30m/Masked")
source.files <- list.files(path = mosaicsDir, pattern = '*.tif$', full.names = T)

outDir <- paste0(basePath, '/datasets/national/covariates/tiles30m' )

reso <- res(templateR)[1]
buf <- reso*3

for(i in ivl:(ivl+(step-1))){
  
  if(i<= nrow(tilesshp)){

    outDirSub<-(paste0(outDir, '/', i))
    if(!dir.exists(outDirSub)){
      
      dir.create(outDirSub)
  
     p <- st_geometry( tilesshp[i,])
     b<-as.numeric(st_bbox(p))
     ext <- extent(c((b[1]-buf), (b[3]+buf), (b[2]-buf), (b[4]+buf)) )
     tm <- crop(templateR,ext,filename = paste0(outDirSub,"/cstone.tif"),format = "GTiff",datatype = "INT1U", overwrite = TRUE)
  
     for (j in 1:length(source.files)) {
       f <- source.files[j]
       inR <- raster(f)
       outName <- basename(f)
       print(paste0(i, ' : ', outName))
       tc <- crop(inR,tm, filename = paste0(outDirSub,"/",outName ),format = "GTiff",datatype = dataType(inR), overwrite = TRUE)
      }
    }
  }
}

  toc()
  print(paste0('Finished Successfully'))
  
    
  
#    if(dir.exists(paste0(root.tiles, '/', sfol))){
#   #if(as.numeric(sfol) < 34610){
#     
#   print(paste0("K = ", k))
#   print(paste0("i = ", i))
#   print(paste0('Processing tile ', sfol))
#   
#   # set up the base raster
#   inR <- paste0(root.base.rasters,'/',sfol,"/cstone.tif")
# 
#   paste0("Loading ", inR)
#   base.raster<- raster(inR)
#   base.raster
#   
#   # select to file to process [need to cycle through each source file]
#   for (m in 1:length(source.files)){
#   #for (m in 1:2){
#     proc.file<- source.files[m] # selected file 
#     
#     # re-project to the resolution and extent as the base raster
#     outname <- paste0(root.base.rasters,'/',sfol,"/", basename(proc.file))
#     if(!file.exists(outname)){
#       sel.raster<- raster(paste0(proc.file))
#       if(is.na(crs(sel.raster))){
#         crs(sel.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#         #crs(sel.raster) <- CRS('+init=EPSG:4326') 
#       }
#       print(paste0(proc.file))
#       #sel.raster
#       rz<- projectRaster(from = sel.raster,to = base.raster,method = "ngb")
#       writeRaster(rz, filename = outname, datatype=dataType(base.raster), overwrite=T)
#       print(paste0("Generated - ", outname))
#     }
#     else{
#       print(paste0("File exists - ", outname))
#     }
#   }
#  }
# }





