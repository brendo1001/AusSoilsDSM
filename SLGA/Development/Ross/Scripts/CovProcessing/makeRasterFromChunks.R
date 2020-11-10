
library(raster)
library(tictoc)


# args = commandArgs(trailingOnly=TRUE)
# 
# k = as.numeric(args[1])
# att <- args[2]
# rootDir <- args[3]

# machineName <- as.character(Sys.info()['nodename'])
# 
# if(machineName=='TERNSOIL2'){
#   rootDir <-  '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/Heat'
# }else{
#   rootDir <- '/datasets/work/af-tern-mal-deb/work/Ross/Heat'
# }

inDir <-  paste0('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/SMOSStats' )
rootDir <-  '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross'
#k=3
#att<- 'B_MOD_DAY_4dim_3ord_Spatial_Temporal'
#att<- 'ETa_MOD_DAY_4dim_3ord_Spatial_Temporal'
#att<- 'A_MOD_DAY_4dim_3ord_Spatial_Temporal'
#att<- 'H_MOD_DAY_4dim_3ord_Spatial_Temporal'
att <- 'AU_daily'


tic()
outputs <- c('mean', 'min', 'max', 'med', 'sd')

outDir <- paste0(rootDir, '/SMOS_Summary')
if(!dir.exists(outDir)){dir.create(outDir)}

tp <- paste0('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/AU_daily/2020/20200102_smos_L2SM_AU.flt')
templateR <- raster(tp)

for (k in 1:length(outputs)) {
  
    output <- outputs[k]
    outRaster <- paste0(outDir, '/SMOS_Summary_', output, '.tif' )
    print(paste0("creating - ", outRaster))
    fls <- list.files(path = paste0(inDir), full.names = T, recursive = T, pattern = paste0('_', output, '.rds$'))
    bs <- readRDS(paste0(rootDir, '/SmosChunks.rds') )
    
    predR<-raster(templateR)
    crs(predR) <- CRS("+proj=longlat +datum=WGS84")
    predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,datatype="FLT4S")
    
    for (i in 1:bs$n)
    {
      print(i)
      bname = paste0(rootDir, '/SMOSStats/', i,'_', output, '.rds')
      print(i)
      if(file.exists(bname)){
        b <- readRDS(bname)
        predR <- writeValues(predR, b, bs$row[i])
      }else{
        print(paste0('File does not exist - ', bname))
        stop()
      }
    }
    
    print('Writing raster to disk.....')
    predR<-writeStop(predR)

}

toc()
print(paste0('Finished Successfully'))
