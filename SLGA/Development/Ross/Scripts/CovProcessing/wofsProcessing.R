
library(sp);library(rgdal);library(doParallel);library(raster);library(tictoc)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work'
  
}else{
  basePath <- '/datasets/work/af-digiscapesm/work'
}

tic()
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))


dirs <- list.dirs(paste0(basePath, '/Ross/wofs'),recursive = F, full.names = T)
length(dirs)

dir <- dirs[k]
fls <-  list.files(dir, '.tif$', full.names = T)

stk <- stack()
for (i in 1:length(fls)) {
  r <- raster(fls[i])
  r[r<0]<-NA
  stk <- addLayer(stk, r)
}

rsum <- calc(stk, fun=sum, na.rm=T)
#rsd <- calc(stk, fun=sd, na.rm=T)
#rstd <- calc(stk, fun=sd)


outdir <- paste0(basePath, '/Ross/SLGAData/Wofs/', basename(dir) )
if(!dir.exists(outdir)){dir.create(outdir)}
outF1 <- paste0(outdir,'/wofs_Sum.tif' )
outF2 <- paste0(outdir,'/wofs_Log.tif' )
writeRaster(rsum, filename = outF1)
writeRaster(log(rsum), filename = outF2)





toc()
print(paste0('Finished Successfully'))