library(doParallel)
library(doSNOW)

makeMapParra <- function(model, templateR, stk, depth, outRasterName, numCPUs=NULL, minBlocks=NULL, tidyUp=T){

  ptm <- proc.time()

  outdir <- dirname(outRasterName)
  rName <- basename(outRasterName)
  
  print(outdir)
  #withoutext <- str_split(rName, '\\.')[[1]][1]

   scratchDir <- paste0(outdir)
  # if(!file.exists(scratchDir)){dir.create(scratchDir)}
  # unlink(paste0(scratchDir, "/*"))

  covNamesinModel<- getCovariatesUsedInModel(model)
  print(covNamesinModel)

  if(is.null(numCPUs)){
    numCPUs = detectCores()-1
  }

  cat(paste0('Using ', numCPUs, ' cores'), sep='\n')

  if(is.null(minBlocks)){
    mblocks = numCPUs
  }else{
    mblocks = minBlocks
  }
  
  bs <- blockSize(stk, minblocks = mblocks)
  saveRDS(bs, paste0(scratchDir, '/brickInfo.rds'))
  cat(paste0('Using ', bs$n, ' blocks'), sep='\n')
  cl <- makeSOCKcluster(numCPUs)
  registerDoSNOW(cl)

  pb <- txtProgressBar(max=bs$n, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  r <- foreach(k=1:bs$n, .options.snow=opts, .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallelWithDepth')) %dopar% applyMapParallelWithDepth(model, templateR, stk, depth, scratchDir, bs, covNamesinModel)
  close(pb)
  stopCluster(cl)


  # cl<-makeCluster(detectCores(),outfile="")
  # registerDoParallel(cl)
  # foreach(k=1:bs$n,  .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallel2')) %dopar% applyMapParallel2(model, templateR, stk, scratchDir, bs, covNamesinModel)

print ('Finished model')
  oRast <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=outRasterName)


  if(tidyUp){
    unlink(scratchDir, recursive = T)
  }
  print(proc.time() - ptm)

  #plot(oRast, maxpixels = 100000)
  return(oRast)


}






writeRasterFromFilesSimple<- function (templateR, rasterDir, outRaster ){
print(paste0(rasterDir, '/brickinfo.rds'))
  bs <-readRDS(paste0(rasterDir, '/brickInfo.rds'))
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")

  for (i in 1:bs$n)
  {
    bname = paste(rasterDir, '/r_', i, '.rds',  sep="")
    # print(bname)
    if(file.exists(bname)){
      blockVals <-readRDS(bname)

      predR <-writeValues(predR, blockVals, bs$row[i])
    }
  }
  predR<-writeStop(predR)

  r <- raster(outRaster)
  r2 <- mask(r, templateR)
  return(r2)
}



# model <- readRDS('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/Models/20181018-PYB_data_SiteID_5/Model_SiteID_.rds')
# bs <- readRDS('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/tmpData/brickInfo.rds')
# templateR <- raster('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/GeoTemplate/Template.tif')
# outDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/tmpData'
# covNamesinModel <- getCovariatesUsedInModel(model)
# fls <- list.files('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/Covariates', full.names = T)
# theStack <- stack(fls)
# k=1
# 
# depth = 10


applyMapParallelWithDepth<- function(model, templateR, theStack, depth, outDir, bs, covNamesinModel){
  
  ncells = bs$nrows[k] * ncol(templateR)
  theSeq = seq(ncells)
  cubCovVals = data.frame(theSeq)
  
  for (i in 1:nlayers(theStack)) 
  {
    rl = raster(theStack, layer=i)
    if(names(rl) %in% covNamesinModel){
      v <- getValues(rl, row=bs$row[k], nrows=bs$nrows[k] )
    }else{
      v <- rep(NA, ncells) # bit of a hack - df needs same structure as input df regardless of covariate usage in model
    }
    cubCovVals[names(rl)] <- v    
  }
  dv <- rep(depth, nrow(cubCovVals))
  indf <- cbind(Depth=dv, cubCovVals[, -1])
  str(indf)
  
  prediction = predict(model, indf )
  bname = paste0(outDir, '/r_' , k, '.rds',  sep="")
  cat(bname)
  saveRDS(prediction, bname)
}


getCovariatesUsedInModel <- function(model)
{
  m<-model
  usage <- m$usage
  covs<-usage[usage$Conditions > 0 | usage$Model > 0, ]
  return(covs$Variable)
}

