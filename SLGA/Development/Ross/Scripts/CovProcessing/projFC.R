library(raster)
library(tictoc)


args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
print(paste0('K = ', k))

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FC/Tiffs'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC'
}else{
  outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC'
  inDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGAData/FC/Tiffs'
}

dirs <- list.dirs(inDir,recursive = F, full.names = T)
length(dirs)

ProcDir <- dirs[k]
print(paste0('Processing Directory is - ', ProcDir))

fls <- list.files(path = ProcDir, full.names = T, recursive = T)
#print(fls)

outd <- paste0(outDir, '/geoTiles', '/', basename(ProcDir))
print(outd)

if(!dir.exists(outd)){dir.create(outd, recursive = T)}

for(i in 1:length(fls)){
  
  f <- fls[i]

  outname <-  paste0(outd, '/', basename(f))
 
   if(!file.exists(outname)){
    sel.raster<- raster(f)
    #print(f)
    tp <- projectExtent(sel.raster, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
    rz<- projectRaster(from = sel.raster, to = tp ,method = "ngb")
    writeRaster(rz, filename = outname, datatype=dataType(sel.raster), overwrite=T)
    print(paste0("Generated - ", outname))
  }
  else{
    print(paste0("File exists - ", outname))
  }
}

print('Successfully completed')
toc()
