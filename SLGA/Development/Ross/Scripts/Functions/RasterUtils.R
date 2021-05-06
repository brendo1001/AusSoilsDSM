library(rgdal)
library(raster)


RasterSummary <- function(r)
{ 
  print(r)
  r <- setMinMax(r)
  print(paste0('Minimum Value = ', minValue(r)))
  print(paste0('Minimum Value = ', maxValue(r)))
}



getWindowInfo <- function(origRaster, windowRaster)
{
  
  #   minX = 140
  #   maxX = 141.2
  #   minY = -18.2
  #   maxY = -17
  #   cellsize = res(templateR)[1]
  #   windowRaster <- raster(xmn=minX, xmx=maxX, ymn=minY, ymx=maxY, res=cellsize)
  #   
  #   origRaster <- raster(paste0(rootDir, rasterDir, '/', templatePath))
  
  print(origRaster)
  
  cellsize <- res(origRaster)[1]
  
  ew <- extent(windowRaster)
  print(ew)
  
  oCol <- colFromX(origRaster, ew[1])
  print(oCol)
  oRow <- rowFromY(origRaster, ew[3] + (cellsize/10))
  print(oRow)
  
  rmaxCol <- colFromX(origRaster, ew[2])
  rmaxRow <- rowFromY(origRaster, ew[4])
  
  outWinExt <- extent(oCol, rmaxCol,rmaxRow, oRow)
  return (outWinExt)
  
} 


writeRasterFromFiles<- function (templateR, rasterDir, outRaster, id, layerNum=0){
  
  
  bs <-readRDS(paste0(rasterDir, '/brickinfo.rds'))
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  for (i in 1:bs$n)
  {
    if(layerNum == 0)
    {
      bname = paste(rasterDir, '/r_' , id, '_', i, '.rds',  sep="")
    }
    else
    {
      bname = paste(rasterDir, '/r_' , id, '_', i, '_', layerNum, '.rds',  sep="")
    }
    
    # print(bname)
    if(file.exists(bname)){
      blockVals <-readRDS(bname)
      #cat(paste(i, " ", sep=''))
      predR <-writeValues(predR, blockVals, bs$row[i])
    }
  }
  predR<-writeStop(predR)
}

writeRasterFromFilesSimple<- function (templateR, rasterDir, outRaster ){
  
  
  
  bs <-readRDS(paste0(rasterDir, '/brickinfo.rds'))
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  
  
  #nrows = nrow(templateR)
  # nrows = 100;
  for (i in 1:bs$n)
  {
    bname = paste(rasterDir, '/r_', i, '.rds',  sep="")
    print(bname)
    if(file.exists(bname)){
      blockVals <-readRDS(bname)
      cat(paste(i, " ", sep=''))
      predR <-writeValues(predR, blockVals, bs$row[i])
    }
  }
  predR<-writeStop(predR)
}


#aggregateRasters('D:/Projects/NAWRA/FGARA/Suitability/limitationRasters', 'D:/Projects/NAWRA/FGARA/Suitability/limitationRastersRedRes20', 20)
aggregateRasters<- function(inDir, outDir, size)
{

  inRasterNames<-list.files(inDir, pattern = '\\.tif$', full.names = T)

  #theStack = stack()
  for (i in 1:length(inRasterNames))
  {
    r = raster(inRasterNames[i] )
    aggregate(r, fact=size, fun=mean, filename=paste0(outDir, '/', names(r)))
#     limRasters[[length(limRasters)+1]] <- r
#     names(limRasters)[length(limRasters)] <- names(r)
#     theStack<- addLayer(theStack,r)
  }  
}
