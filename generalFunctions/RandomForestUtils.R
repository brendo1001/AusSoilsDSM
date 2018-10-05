library(raster) 
library(rgdal)
library(doParallel)
library(ranger)


# this converst the factor in the Predict.all = T DF back to levels
as.numeric.factor <- function(x, model) {as.numeric(model$forest$levels)[x]}

getMissingRFInputs <- function(model, covs)
{
  m <- model$forest$independent.variable.names
  c <- names(covs)
  #c <- colnames(covs)
  inModel <- m[!m %in% c]
  inCovs <- c[!c %in% m]
  v = list(inModel=inModel, inCovs=inCovs)
}

getModalValue <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getCatPredsSummary <- function(v) {

  uniqv <- unique(v)
  if(length(uniqv) >1)
  {
    cnts <- tabulate(match(v, uniqv))
    modVal <- uniqv[which.max(cnts)]

    cntsDF <- matrix(c(uniqv,cnts),nrow=length(cnts))
     ordcntsDF <- cntsDF[order(-cnts),]
     v1 <- ordcntsDF[1,2]
     v2 <- ordcntsDF[2,2]
     ci <- v2/v1
  }
  else
  {
    modVal <- uniqv[1]
    ci <- 0
  }
  v = list(mode=modVal, confInd=ci)
}

doRFSummaryRasterParallel <- function(rootDir, att, templateR, bs){

  templateVals <- as.numeric(getValues(templateR, row=bs$row[k], nrows=bs$nrows[k]))
  t2 <- templateVals
  fname <- paste0(rootDir,'/OutputRasters/', att, '/Chunks/r_', k, '.rds')
  
  if(file.exists(fname)){
    chunkVals <- readRDS(fname)
    inds <- chunkVals[,1]
    sumData <- apply(chunkVals, 1, getCatPredsSummary)
    b <- do.call(rbind, sumData)
    
    sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
    
    vname <- paste0(rootDir,'/OutputRasters/', att, '/Chunks/v_', k, '.rds')
    saveRDS(sumDataDF2, vname)
  }
}

makerasterFromTreePredictionChunks <- function(rootDir = NULL, templateR = NULL, att = NULL, numcpus = 1){
  
  rasterDir <- paste0(rootDir, '/OutputRasters/', att, '/Chunks')
  outRaster <- paste0(rootDir, '/OutputRasters/', att, '/', att, '.tif')
  print(paste0("creating - ", outRaster))
  bs <- readRDS(paste0(rootDir,'/OutputRasters/', att, '/Chunks/chunkInfo.rds'))
  
  print("Doing model summary.....")
  
  cl<-makeCluster(numcpus,outfile="")
  registerDoParallel(cl)
  foreach(k=1:length(bs$row), .export= c("doRFSummaryRasterParallel", "getCatPredsSummary"), .packages=c('raster','rgdal', 'ranger')) %dopar% doRFSummaryRasterParallel(rootDir, att, templateR, bs)
  stopCluster(cl)
  
    predR<-raster(templateR)
    predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
    outRasterCI <- paste0(rootDir, '/OutputRasters/', att, '/', att , '_CI.tif')
    predCI<-raster(templateR)
    predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
    
    print(paste0("Creating Raster ", outRaster) ) 

  for (i in 1:bs$n)
  {
    bname = paste0(rasterDir, '/v_', i, '.rds')
    print(bname)
    
    csize<-bs$nrows[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
      cat(paste(i, " ", sep=''))
      b[blockVals$inds] <- blockVals$modalVal
      c[blockVals$inds] <- blockVals$ci
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
    }
  }
  
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  plot(predR)
  plot(predCI)
  print("Done")
}

loadStack <- function(covariates.ToUsePath = NULL, model = NULL, covariates.DirPath = NULL )
{
  cov_list<-read.table(covariates.ToUsePath, sep=",", header=FALSE,strip.white=TRUE)
  colnames(cov_list) <-c('Directory', 'Filename', 'isUsed', 'Datatype', 'Ext')
  covsinModel <- model$forest$independent.variable.names
  covsToUse <- cov_list[ cov_list$Filename %in% covsinModel,]
  
  
  theStack = stack()
  #theStack<- addLayer(theStack,templateR)
  for (i in 1:nrow(covsToUse)) 
  {
    rname <- paste0(covariates.DirPath ,'/', covsToUse$Directory[i], '/', covsToUse$Filename[i], '.tif')
    print(paste0(rname))
    theStack<- addLayer(theStack,raster(paste0(rname)))
  }
  nlayers(theStack)
  
  return (theStack)
}


MapRF <- function(att = NULL, model = NULL, templateR = NULL, theStack = NULL, rootDir = NULL, cpus = NULL, minBlocks, isNumeric = F){

  ptm <- proc.time()

  outRasterDir <- paste0(rootDir, '/OutputRasters/', att) 
  outRaster <- paste0(outRasterDir, '/', att, '_totPred.tif')
  chunkRasterDir <- paste0(outRasterDir, '/Chunks')
  if(!(file.exists(chunkRasterDir))){
    dir.create(chunkRasterDir, recursive=T )
  }
  
  hideres <- do.call(file.remove, list(list.files(chunkRasterDir, full.names = TRUE)))
  
  numcpus <- detectCores()
  if(!is.null(cpus))
  {
    numcpus <- cpus
  }
  
  bs <- blockSize(theStack, minblocks = minBlocks)
  saveRDS(bs, paste0(chunkRasterDir, '/chunkInfo.rds'))
  
  print(paste0('Running spatial model for ', att))
  print(paste0('Number of chunks = ', bs$n))
  
  cl<-makeCluster(numcpus,outfile="")
  registerDoParallel(cl)
  foreach(k=1:length(bs$row), .export= c("applyRFMapParallel"), .packages=c('raster','rgdal', 'ranger')) %dopar% applyRFMapParallel(model, bs, templateR, theStack, chunkRasterDir, isNumeric = F)
  stopCluster(cl)
  
  #makerasterFromChunks(chunkRasterDir, templateR)
  makerasterFromTreePredictionChunks(rootDir = rootDir,  templateR = templateR, att= att, numcpus = cpus)

  
  timeTaken <- proc.time() - ptm
  
  return (timeTaken)
}



# Depricated
applyRFMapParallel <- function(model = NULL, bs = NULL, templateR = NULL, theStack = NULL, outDir = NULL, isNumeric = F){
  
 # k=4
  #print(k)
  
  ncells = bs$nrows[k] * ncol(templateR)
  theSeq = seq(ncells)
  covs = data.frame(theSeq)
  
  for (i in 1:nlayers(theStack)) 
  {
    rl = raster(theStack, layer=i)
    v <- as.numeric(getValues(rl, row=bs$row[k], nrows=bs$nrows[k]))
    
    #### this is a hack to deal with NA needs to be sorted
    if(i > 1){
      badInds <- which(is.na(v))
      if(length(badInds) > 0)
      {
        v[badInds] <- 0
      }
    }
    covs[names(rl)] <- v    
  }
  
  valsi <- which(!is.na(covs[,2]))
  valsnd <- which(is.na(covs[,2]))
  
  if(length(valsi) > 0){
    
#     if(doCI)
#       {
#          prediction = predict(model, covs[valsi, -1],  predict.all = F)$predictions
#          predsPlusCellID <- cbind(valsi, as.numeric(as.character(prediction)))
#       }
#     else
#       {
        prediction = predict(model, covs[valsi,-1],  predict.all = T)$predictions
        if(isNumeric)
          {
            # if the target variable this transposes it back from factor values to actual value
            prediction[] <- as.numeric(model$forest$levels)[prediction]
          }
        #prediction[] <- as.numeric.factor(prediction, model)
        predsPlusCellID <- cbind(valsi, prediction)
      #}
    bname = paste0(chunkRasterDir, '/r_' , k, '.rds')
    saveRDS(predsPlusCellID, bname)
  }
  
 
  
}