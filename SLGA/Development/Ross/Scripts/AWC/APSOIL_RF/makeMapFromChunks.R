library(raster)


rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRF'
scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'

source(paste0(scriptDir, '/RFUtils.R'))

att = 'DUL'
depth = '5'
doMean = T
doUncerts = F

patt <- paste0(att, '_', depth)

chunkLines=20

templateR <- raster(paste0(covDir, '/Relief_dems_3s_mosaic1.tif'))
chk <-	getChunkInfo(chunkLines, nrow(templateR))
outDir <- paste0(workDir, '/Maps')
inDir <- paste0(workDir, '/Maps/',  patt,'/Chunks')

doMapMaking(inDir, outDir, patt, doUncerts, templateR, chk, datatype='FLT4S')

doMapMaking <- function(inDir, outDir, patt, doUncerts, templateR, bs, datatype){
  
  outRasterM <-   paste0(outDir, '/', patt, '_mean.tif')
  print(paste0("creating - ", outRasterM))
  
  if(doMean){
    predR<-raster(templateR)
    predR<-writeStart(predR,filename=outRasterM,overwrite=TRUE,NAflag=-9999,datatype=datatype)
  }
  
  if(doUncerts){
    predR_U<-raster(templateR)
    predR_U<-writeStart(predR,filename=paste0(outDir, '/', patt, '_Upper.tif'),overwrite=TRUE,NAflag=-9999,datatype=datatype)
    predR_L<-raster(templateR)
    predR_L<-writeStart(predR,filename=paste0(outDir, '/', patt, '_Lower.tif'),overwrite=TRUE,NAflag=-9999,datatype=datatype)
  }
  
  for (i in 1:chk$chunks)
  {
    print(i)
    bname = paste0(inDir, '/AllCellVals_', i, '.rds')
    csize<-chk$nlines[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
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
  
  if(doMean){
    predR<-writeStop(predR)
  }
  
  if(doUncerts){
    predR_L<-writeStop(predR_L)
    predR_U<-writeStop(predR_U)
  }
  
  print('')
  print(paste0('Finished generating ', basename(outRaster)))
  
  outPaths <- c(outRaster)
  
  return(outPaths)
}

