library(sp);library(rgdal);library(raster);library(tictoc);library(sf)

rasterOptions(progress = 'text', memfrac = 0.5)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/Masked'
  tilePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tiles_25m'
}else{
  #basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked'
  basePath <- '/datasets/work/af-tern-mal-deb/work/Ross/New/HeatMask'
  tilePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles_25m'
}



tic()
#fls<- list.files(basePath, full.names = T, recursive = F, pattern = 'Relief_1sec_')

args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
att = args[2]
#att = 'Relief_1sec_aspect_1s.tif'
print(paste0("Processing iteraration = ", k))


sq <- seq(1, 20041,50)
ivl <- sq[k]

mosR <- raster(paste0(basePath, '/', att))

#fols<- list.dirs(tilePath, full.names = F, recursive = F)
#write.csv(fols, file=paste0(tilePath, '.csv'), quote = F, row.names = F, col.names = F )
fols <- read.csv( file=paste0(tilePath, '.csv'))
for(i in ivl:(ivl+49)){
  
  print(i)
  sfol <- fols[i,1]
  print(paste0("K = ", k))
  print(paste0("i = ", i))
  print(paste0('Processing tile ', sfol))
  
  outFname <- paste0(tilePath,'/',sfol,"/", att )
  if(!file.exists(outFname)){
  
  # set up the base raster
  inR <- paste0(tilePath,'/',sfol,"/cstone.tif")
  paste0("Loading ", inR)
  tile.raster<- raster(inR)
  tile.raster
  outR <- crop(mosR, tile.raster, file=outFname, overwrite=T )
  }else{
    print(paste0('File exists - ', outFname))
  }
}

print('Successfully completed')
print(toc())

print(paste0('Finished Successfully'))

