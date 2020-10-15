# BARE EARTH iMAGERY RESCALE AND REPROJECT



# libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(tictoc)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work'
}

tic()
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

sq <- seq(1, 20041,5)
ivl <- sq[k]

#length(sq)

# roots
root.tiles<-  paste0(basePath, '/datasets/national/covariates/tiles_25m')
root.base.rasters<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles_25m"
source.root <- paste0(basePath, "/datasets/national/covariates/landcover/GAMosaics")

source.files <- list.files(path = source.root, full.names = T)
source.files

# output folders
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
fols<- fols[order(fols)]

for(i in ivl:(ivl+4)){

print(i)
  #select the folder
  sfol <- fols[i]
  print(paste0("K = ", k))
  print(paste0("i = ", i))
  print(paste0('Processing tile ', sfol))
  
  # set up the base raster
  inR <- paste0(root.base.rasters,'/',sfol,"/cstone.tif")
  paste0("Loading ", inR)
  base.raster<- raster(inR)
  base.raster
  
  # select to file to process [need to cycle through each source file]
  for (m in 1:length(source.files)){
  #for (m in 1:2){
    proc.file<- source.files[m] # selected file 
    
    # re-project to the resolution and extent as the base raster
    outname <- paste0(root.base.rasters,'/',sfol,"/", basename(proc.file))
    if(!file.exists(outname)){
      sel.raster<- raster(paste0(proc.file))
      print(paste0(proc.file))
      sel.raster
      rz<- projectRaster(from = sel.raster,to = base.raster,method = "ngb")
      writeRaster(rz, filename = outname, datatype=dataType(base.raster), overwrite=T)
      print(paste0("Generated - ", outname))
    }
    else{
      print(paste0("File exists - ", outname))
    }
  }

}

print('Successfully completed')
toc()




