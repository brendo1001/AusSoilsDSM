library(raster)
rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory

covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRF'
scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'

source(paste0(scriptDir, '/RFUtils.R'))

att = 'DUL'
#depth = '0'
#depth = '5'
depth = '15'
doMean = T
doUncerts = T

patt <- paste0(att, '_', depth)
print(patt)

chunkLines=20

templateR <- raster(paste0(covDir, '/Relief_dems_3s_mosaic1.tif'))
chk <-	getChunkInfo(chunkLines, nrow(templateR))
outDir <- paste0(workDir, '/Maps')
inDir <- paste0(workDir, '/Maps/',  patt,'/Chunks')

doMapMaking(inDir, outDir, patt, doUncerts, templateR, chk, datatype='FLT4S')

doMapMaking <- function(inDir, outDir, patt, doUncerts, templateR, bs, datatype){
  
  outRasterM <-   paste0(outDir, '/', patt, '_mean.tif')
  print(paste0("creating - ", outRasterM))
  pb <- txtProgressBar(min=0, max=2040, style=3)
  
  if(doMean){
    print(paste0("creating - ", outRasterM))
    predR<-raster(templateR)
    predR<-writeStart(predR,filename=outRasterM,overwrite=TRUE,NAflag=-9999,datatype=datatype)
  }
  
  if(doUncerts){
    predR_U<-raster(templateR)
    predR_U<-writeStart(predR_U,filename=paste0(outDir, '/', patt, '_Upper.tif'),overwrite=TRUE,NAflag=-9999,datatype=datatype)
    predR_L<-raster(templateR)
    predR_L<-writeStart(predR_L,filename=paste0(outDir, '/', patt, '_Lower.tif'),overwrite=TRUE,NAflag=-9999,datatype=datatype)
    print(paste0("creating - ", paste0(outDir, '/', patt, '_Lower.tif')))
    int(paste0("creating - ", paste0(outDir, '/', patt, '_Upper.tif')))
  }
  
  for (i in 1:chk$chunks)
  {
    #print(i)
    setTxtProgressBar(pb, i)
    bname = paste0(inDir, '/AllCellVals_', i, '.rds')
    csize<-chk$nlines[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
      
      if(!nrow(blockVals)==0){
      
      if(all(is.na(blockVals))){
        print(paste0(i, ' is all NA values'))
      }
      
      if(doMean){
        b[blockVals[,1]] <- blockVals[,2]
        predR <- writeValues(predR, b, chk$starts[i])
      }
      
      if(doUncerts){
        #lower
        bl <- rep(NAvalue(templateR), csize )
        bl[blockVals[,1]] <- blockVals[,3]
        predR_L <- writeValues(predR_L, bl, chk$starts[i])
        
        #upper
        bu <- rep(NAvalue(templateR), csize )
        bu[blockVals[,1]] <- blockVals[,4]
        predR_U <- writeValues(predR_U, bu, chk$starts[i])
      }
    }
  }
}
  
  if(doMean){
    predR<-writeStop(predR)
    print(paste0('Finished generating ', basename(outRasterM)))
  }
  
  if(doUncerts){
    predR_L<-writeStop(predR_L)
    predR_U<-writeStop(predR_U)
    paste0(outDir, '/', patt, '_Lower.tif')
    paste0(outDir, '/', patt, '_Upper.tif')
  }
  close(pb)
  print('')
  print(paste0('Finished generating ', basename(outRaster)))
  
  outPaths <- c(outRaster)
  
  return(outPaths)
}

