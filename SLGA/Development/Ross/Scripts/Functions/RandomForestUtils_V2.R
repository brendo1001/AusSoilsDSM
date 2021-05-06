library(raster) 
library(rgdal)
library(doParallel)
library(ranger)
library(readxl)
library(stringr)
library(rmarkdown)

# this converst the factor in the Predict.all = T DF back to levels
as.numeric.factor <- function(x, model) {as.numeric(model$forest$levels)[x]}

getMissingRFInputs <- function(model, covs)
{
  m <- model$forest$independent.variable.names
  c <- names(covs)
  #c <- colnames(covs)
  inModel <- m[!m %in% c]
  inCovs <- c[!c %in% m]
  v = list(inModelButNotInCovariates=inModel, inCovariatesButNotInModels=inCovs)
}

getModalValue <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

CV <- function(mean, sd){
  (sd/mean)*100
}

getCategoricalPredictionSummary <- function(v) {

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

getContinuousPredictionSummary <- function(v) {
 
  m <- mean(v)
  stdDev <- sd(v)
  
  (stdDev/m)*100
  
  ci <-(stdDev/m)*100
  v = list(pred=m, confInd=ci)
}

#doRFSummaryRasterParallel(rootDir, catchment, att, modelType, templateR, bs)

doRFSummaryRasterParallel <- function(rootDir, catchment, att, modelType, templateR, bs){
  
  # k <- 77
  # catchment = 'Mitchell'
  # modelType = 'Regression'
# 
#   templateVals <- as.numeric(getValues(templateR, row=bs$row[k], nrows=bs$nrows[k]))
#   t2 <- templateVals
  
  fname <- paste0(rootDir, '/Attributes/', att, '/', catchment, '/Chunks/r_', k, '.rds')
  print(fname)
  
  #if(file.exists(fname)){
    chunkVals <- readRDS(fname)
    inds <- chunkVals[,1]
    
    if(modelType == 'Classification'){
    
        sumData <- apply(chunkVals[,-1], 1, getCategoricalPredictionSummary)
        
    } else if(modelType == 'Regression'){
      
        sumData <- apply(chunkVals[,-1], 1, getContinuousPredictionSummary)
    }
    
    b <- do.call(rbind, sumData)
    sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
    
    vname <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/Chunks/v_', k, '.rds')
    print(paste0('vname = ' , vname))
    saveRDS(sumDataDF2, vname)
  #}
}

makerasterFromTreePredictionChunks <- function(rootDir = NULL, catchment = NULL, templateR = NULL, att = NULL, modelType = NULL, numcpus = 1){
  
  rasterDir <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/Chunks')
  outRaster <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/', att, '_', catchment, '.tif')
  print(paste0("creating - ", outRaster))
  bs <- readRDS(paste0(rootDir, '/Attributes/',  att, '/', catchment, '/Chunks/chunkInfo.rds'))
  
  print(paste0("Doing Attribute summary....."))

  cl<-makeCluster(numcpus,outfile="")
  registerDoParallel(cl)
  foreach(k=1:length(bs$row), .export= c("doRFSummaryRasterParallel", "getCategoricalPredictionSummary", "getContinuousPredictionSummary"), .packages=c('raster','rgdal', 'ranger')) %dopar% doRFSummaryRasterParallel(rootDir, catchment, att, modelType, templateR, bs)
  stopCluster(cl)

  
  suffix <-'_Uncert'
  # if(modelType == 'Classification'){
  #   suffix <- '_CI.tif'
  # }else{
  #   suffix <- '_CoV.tif'
  # }
  
  
  outRasterCI <- paste0(rootDir, '/Attributes/',  att, '/', catchment, '/', att, '_', catchment, suffix)
  
    predR<-raster(templateR)
    predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
    #outRasterCI <- paste0(rootDir, '/Attributes/', catchment, '/', att, '/', att , suffix)
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
  plot(predR, main=paste0(att, ' - ', catchment ))
  plot(predCI, main=paste0(att, ' CI', ' - ', catchment ))
  print("Done")
}

loadStack <- function(covariates.ToUsePath = NULL, model = NULL, covariates.DirPath = NULL )
{
  #cov_list<-read.table(covariates.ToUsePath, sep=",", header=FALSE,strip.white=TRUE)
  cov_listRaw<- read_excel(covariates.ToUsePath,1)
  cov_list <- as.data.frame(cov_listRaw)
  #colnames(cov_list) <-c('Directory', 'Filename', 'isUsed', 'Datatype', 'Ext')
  colnames(cov_list) <-c('Directory', 'Filename', 'isUsed')
  covsinModel <- model$forest$independent.variable.names
  covsToUse <- cov_list[ str_trim(cov_list$Filename) %in% covsinModel,]
  
  theStack = stack()
  #theStack<- addLayer(theStack,templateR)
  for (i in 1:nrow(covsToUse)) 
  {
    rname <- paste0(covariates.DirPath ,'/', str_trim(covsToUse$Directory[i]), '/', str_trim(covsToUse$Filename[i]), '.tif')
    print(paste0(rname))
    theStack<- addLayer(theStack,raster(paste0(rname)))
  }
  print(paste0('There are ', nlayers(theStack), ' in the stack'))
  
  return (theStack)
}

MapRF <- function(att = NULL, model = NULL, catchment = NULL, templateR = NULL, theStack = NULL, rootDir = NULL, cpus = NULL, minBlocks, isnumeric=T){

  ptm <- proc.time()
  
  checkModelCovariates(model, theStack)

  outRasterDir <- paste0(rootDir, '/Attributes/', att, '/', catchment) 
  #outRaster <- paste0(outRasterDir, '/', att, '_totPred.tif')
  chunkRasterDir <- paste0(outRasterDir, '/Chunks')
  if(!(file.exists(chunkRasterDir))){
    dir.create(chunkRasterDir, recursive=T )
  }
  
  print(paste0('Chunk Directory - ', chunkRasterDir))
  
  #hideres <- do.call(file.remove, list(list.files(chunkRasterDir, full.names = TRUE)))
  
  numcpus <- detectCores()
  if(!is.null(cpus))
  {
    numcpus <- cpus
  }
  
  bs <- blockSize(theStack, minblocks = minBlocks)
  saveRDS(bs, paste0(chunkRasterDir, '/chunkInfo.rds'))
  
  print(paste0('Running spatial model for ', att))
  print(paste0('Number of chunks = ', bs$n))
  print(paste0('Is Numeric = ', isnumeric))
  
  cl<-makeCluster(numcpus,outfile="")
  registerDoParallel(cl)
  foreach(k=1:length(bs$row), .export= c("applyRFMapParallel", "convertToNumeric"), .packages=c('raster','rgdal', 'ranger')) %dopar% applyRFMapParallel(model, bs, templateR, theStack, chunkRasterDir, isnumeric)
  stopCluster(cl)


   makerasterFromTreePredictionChunks(rootDir = rootDir, catchment=catchment, modelType = model$treetype, templateR = templateR, att = att, numcpus = cpus)

  
  timeTaken <- proc.time() - ptm
  
  if(model$forest$treetype == 'Classification'){
    print( model$forest$levels)
    print(model$forest$class.values)
  }
  
 
  
  
  return (timeTaken)
}

ReportModelSummary <- function(rootDir, modelPath, attribute, catchment ){
  
  
 templatePath <-  'C:/Users/sea084/Dropbox/RossRCode/NAWRA/Production/ModelReportTemplate.Rmd'
 
 outDir <- paste0(rootDir, '/Reports')
 outfile <- paste0(outDir, '/ModelReport_', attribute)
 
 if(!dir.exists(outDir))
    dir.create(outDir)
 
 ps = list(  modelPath = modelPath, attribute = attribute, catchment = catchment, rootDir = rootDir)
 rmarkdown::render(templatePath, params = ps, output_format = 'pdf_document', output_file = paste0(outfile, '.pdf'), quiet=T)
 rmarkdown::render(templatePath, params = ps, output_format = 'html_document', output_file = paste0(outfile, '.html'), quiet=T)
 rmarkdown::render(templatePath, params = ps, output_format = 'word_document', output_file = paste0(outfile, '.docx'), quiet=T)
 
 #rmarkdown::render(templatePath, params = ps, output_format = 'all', output_file = paste0(outfile))
 #rmarkdown::render(templatePath, params = ps, output_format = c('html_document', 'pdf_document'), output_file = paste0(outfile))

 
 png(file =  paste0(outDir, '/', attribute, '_', catchment,  ".png"), bg = "white", width = 1000, height = 1000)
 r1 <- raster(paste0(rootDir, '/', attribute, '_', catchment, '.tif'))
 plot(r1, main = paste0(attribute, ' - ', catchment))
 dev.off()
 
 png(file =  paste0(outDir, '/', attribute, '_', catchment,  "_Uncert.png"), bg = "white", width = 1000, height = 1000)
 r1 <- raster(paste0(rootDir, '/', attribute, '_', catchment, '_Uncert.tif'))
 plot(r1, main = paste0('Cov for ', attribute, ' - ', catchment))
 dev.off()
 
 if(file.exists(paste0(outfile, '.html')))
    file.show( paste0(outfile, '.html'))
 
}


ModelSummary <- function(att = att, model = model, catchment = catchmentName){
  
  outSumDir <- paste0(rootDir, '/Attributes/', att, '/', catchment)
  sink("d:/Projects/NAWRA/Attributes/Attributes/SoilDepth/xSoilDepth_mod2_Outputs_D.txt")
  model
  model$variable.importance[order(model$variable.importance, decreasing = TRUE)]
  summariseRFModel(modelPath)
  getRasterDecodes(model)
  sink()
}


checkModelCovariates <- function(model = model, theStack = theStack){
  
  l = getMissingRFInputs(model, theStack)
  
  if (length(l$inModelButNotInCovariates) > 0)
  {
    cl <- str_c(l$inModelButNotInCovariates,collapse='\n') 
    errMsg <- paste0('\nERROR : CAN NOT RUN THE MODEL\nThe following covariates are in the model but not in the covariate stack\n', cl )
    stop(errMsg)
  }
  
  if (length(l$inCovariatesButNotInModels) > 0)
  {
    cl <- str_c(l$inCovariatesButNotInModels,collapse='\n') 
    errMsg <- paste0('\nERROR : CAN NOT RUN THE MODEL\nThe following covariates are in the covariate stack but not in the model\n', l$inCovariatesButNotInModels )
    stop(errMsg)
  }
}



convertToNumeric <- function(x){ as.numeric(as.character(model$forest$levels[x])) }


applyRFMapParallel <- function(model = NULL, bs = NULL, templateR = NULL, theStack = NULL, outDir = NULL, isnumeric=T){
  
  # k=50
  # catchment = 'Darwin'
  # outDir <- paste0(rootDir, '/Attributes/', catchment, '/', att)
  # isnumeric <- F
  # bs <- blockSize(theStack, minblocks = 100)

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
    
      prediction = predict(model, covs[valsi,-1],  predict.all = T)$predictions
      
           if(model$forest$treetype == 'Classification')
           {
              if(isnumeric)
                {
                  m <- matrix(prediction,nrow=nrow(prediction))
                  outM <- apply(m,2,convertToNumeric)
                }else{
                  outM <- prediction
                }
            }else{
              outM <- prediction
            }
           
      predsPlusCellID <- cbind(valsi, outM)

    bname = paste0(chunkRasterDir, '/r_' , k, '.rds')
    saveRDS(predsPlusCellID, bname)
  }
}


summariseRFModel <- function(modelPath= NULL, att='Unkown'){
  
  model <- readRDS(modelPath)
  
  # b <- as.list(model$call)[2]
  # c <- as.formula(b[[1]])
  # t <- terms.formula(c)
  # 
  # att <- t[[2]]
  # 
  # cat(paste0('Attribute = ', att, '\n'))
  cat(paste0('Path = ', modelPath, '\n'))
  cat(paste0('Type : ', model$treetype, '\n'))
  cat(paste0('Number of trees : ', model$num.trees, '\n'))
  cat(paste0('Sample size : ', model$num.samples, '\n'))
  cat(paste0('Number of independent variables : ', model$num.independent.variables, '\n'))
  cat(paste0('OOB prediction error : ', model$prediction.error, '\n'))
  cat( '\n')
  
  if(model$treetype == 'Classification'){
    cat(paste0('Class Values' , '\n' ))
    cat(model$forest$class.values)
    cat(paste0('\n', 'Class Levels : ', '\n' ))
    cat(paste0(model$forest$levels))
    cat( '\n')
    cat( '\n')
    cat("Confusion Matrix",'\n')
    print(model$confusion.matrix)
    cat('\n')
    
    #plot(model$predictions)
    
  }else if(model$treetype == 'Regression'){
    cat(paste0('R2 : ', model$r.squared, '\n'))
    #plot(model$predictions)
  }
  
  cat('\n')
  #cat(paste0('Variable Importance : ','\n'))
  #print(model$variable.importance[order(model$variable.importance, decreasing = TRUE)])
  
  imp <- model$variable.importance[order(model$variable.importance, decreasing = F)]
  par(mai=c(1,2,1,1))
  barplot(imp, main=paste0("Variable Importance - ", att) , horiz=TRUE, col=rainbow(50), las=1, cex.names=.5, las=1)

}


getRasterDecodes <- function(model = model){

  ids <- model$forest$class.values
  cats <-  as.numeric(as.character(model$forest$levels[ids]))
  decodes <- data.frame( RID = ids, Category = cats)
  decodesOrd <- decodes[order(decodes$RID),]
  return (decodesOrd)
  
}










