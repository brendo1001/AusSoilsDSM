library(raster)
library(tictoc)
rasterOptions(progress = 'text', maxmemory = 2147483648, todisk=T)


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/Masked'
  outDir <- paste0('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/30m/MaskedP')
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked'
  outDir <- paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/MaskedP')
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))


tic()

fls<- list.files(basePath, recursive = F, full.names = F, pattern = '*.tif$')

inR <- raster(paste0(basePath, '/', fls[k]))
#inR <- raster('M:/work/datasets/national/covariates/mosaics/30m/Masked/Other_GREEN.tif')
outname <- paste0(outDir, '/',  fls[k])

bs<- blockSize(inR, n=2)
#outname <- paste0('e:/temp/mask302.tif')
crs(inR)<- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
outR<-writeStart(inR,filename=outname, overwrite=TRUE,datatype=dataType(inR))


cnt=1
for (i in 1:bs$n) { 
  print(i)
  vals<-getValues(inR, row=bs$row[i], nrows=bs$nrows[i] )
  outR <- writeValues(outR, vals, bs$row[i] )
  vals=NULL
  cnt = cnt+1
  if(cnt>25){
    gc()
    cnt=1
  }
}

outR<-writeStop(outR)

print(toc())

print(paste0('Finished Successfully - ', fls[k]))
