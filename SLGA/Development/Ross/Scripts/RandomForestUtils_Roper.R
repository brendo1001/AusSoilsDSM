library(raster) 
library(rgdal)
library(doParallel)
library(ranger)
library(readxl)
library(stringr)
library(rmarkdown)
require(doSNOW)
library(fst)

# this converst the factor in the Predict.all = T DF back to levels
#as.numeric.factor <- function(x, model) {as.numeric(model$forest$levels)[x]}

is.number <- function(x) all(grepl("[[:digit:]]", x))

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

doRFSummaryRasterParallel <- function(rootDir,  att, modelType, templateR, bs){
  
  fname <- paste0(rootDir, '/Attributes/', att, '/Chunks/r_', k, '.rds')
  print(fname)
  
  if(file.exists(fname)){
    chunkVals <- readRDS(fname)
    inds <- chunkVals[,1]
    
    if(modelType == 'Classification'){
    
        sumData <- apply(chunkVals[,-1], 1, getCategoricalPredictionSummary)
        
    } else if(modelType == 'Regression'){
      
        sumData <- apply(chunkVals[,-1], 1, getContinuousPredictionSummary)
    }
    
    b <- do.call(rbind, sumData)
    sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
    
    vname <- paste0(rootDir, '/Attributes/',  att, '/Chunks/v_', k, '.rds')
    print(paste0('vname = ' , vname))
    saveRDS(sumDataDF2, vname)
    
  }
  #gc()
}

makerasterFromTreePredictionChunks <- function(outDir = NULL, templateR = NULL, att = NULL, modelType = NULL, numcpus = 1, bs = NULL){
  gc()
  print(paste0("Doing Attribute summary....."))
  dname <- paste0(outDir, '/Attributes/', att, '/Chunks')
  fl <- list.files(dname, 'r_')
  cl <- makeSOCKcluster(numcpus)
  registerDoSNOW(cl)
  pb <- txtProgressBar(initial = 1, min=1, max=length(fl), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  foreach(k=1:length(fl), .options.snow=opts, .export= c("doRFSummaryRasterParallel", "getCategoricalPredictionSummary", "getContinuousPredictionSummary"), .packages=c('raster','rgdal', 'ranger')) %dopar% doRFSummaryRasterParallel(outDir, att, modelType, templateR, bs)
  stopCluster(cl)
  gc()
  generateRaster(outDir, templateR, att, modelType, bs)
  gc()
}

generateRaster <- function(outDir = NULL, templateR = NULL, att = NULL, modelType = NULL, bs = NULL){
  
  suffix <-'_Uncert.tif'
  
  rasterDir <- paste0(outDir, '/Attributes/',  att,  '/Chunks')
  outRaster <- paste0(outDir, '/Attributes/',  att, '/',  att, '.tif')
  print(paste0("creating - ", outRaster))

  outRasterCI <- paste0(outDir, '/Attributes/',  att, '/', att, suffix)
  
    predR<-raster(templateR)
    crs(predR) <- CRS("+proj=longlat +datum=WGS84")
    predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
    predCI<-raster(templateR)
    crs(predCI) <- CRS("+proj=longlat +datum=WGS84")
    predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
    
    print(paste0("Creating Raster ", outRaster) ) 
    
    pb <- txtProgressBar(min=1, max=length(bs$row), style=3)

  for (i in 1:bs$n)
  {
    setTxtProgressBar(pb, i)
    bname = paste0(rasterDir, '/v_', i, '.rds')
   # print(bname)
    
    csize<-bs$nrows[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    if(file.exists(bname)){
      blockVals <- readRDS(bname)
      #cat(paste(i, " ", sep=''))
      b[blockVals$inds] <- blockVals$modalVal
      c[blockVals$inds] <- blockVals$ci
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
    }
  }
    
    close(pb)
  
    print('Writing rasters to disk.....')
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  plot(predR, main=paste0(att ))
  plot(predCI, main=paste0(att ))
  print("Done")
}

loadStack <- function(covariates.ToUsePath = NULL, model = NULL, covariates.DirPath = NULL )
{
  cov_list<- read.csv(covariates.ToUsePath)
  covsinModel <- model$forest$independent.variable.names
  covsToUse <- cov_list[ str_trim(cov_list$Covariate) %in% covsinModel,]
  
  theStack = stack()

  for (i in 1:nrow(covsToUse)) 
  {
    rname <- paste0(covariates.DirPath , '/', str_trim(covsToUse$Covariate[i]), '.tif')
    #print(paste0(rname))
    theStack<- addLayer(theStack,raster(paste0(rname)))
  }
  print(paste0('There are ', nlayers(theStack), ' in the stack'))
  
  return (theStack)
}



MapRF <- function(att = NULL, model = NULL, templateR = NULL, theStack = NULL, rootDir = NULL, outDir=NULL, deleteFiles=F, cpus = NULL, bs=NULL, isLog = F){

  gc()
  tic()
  
  checkModelCovariates(model, theStack) 
  isnumeric <- is.number(model$forest$levels)

  outRasterDir <- paste0(outDir , '/Attributes/', att) 
 
  
  chunkRasterDir <- paste0(outRasterDir, '/Chunks')
  if(!(dir.exists(chunkRasterDir))){
    dir.create(chunkRasterDir, recursive=T )
  }else{
   if(deleteFiles){
     f <- list.files(outRasterDir, include.dirs = F, full.names = T, recursive = T)
     gone <- file.remove(f)
   }  
  }
  
  print("")
  print(paste0('Running spatial model for ', att))
  print(paste0('Output directory = ', outDir))
  print(paste0('Chunk Directory - ', chunkRasterDir))
  
  numcpus <- detectCores()-1
  if(!is.null(cpus)){numcpus <- cpus}

 
  print(paste0('Number of chunks = ', bs$n))
  print(paste0('Is Numeric = ', isnumeric))
  
  print(paste0('Delete files = ', deleteFiles))
  
  cl <- makeSOCKcluster(numcpus)
  registerDoSNOW(cl)
  pb <- txtProgressBar(initial=1, min=1, max=length(bs$row), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  result <- foreach(k=1:length(bs$row),.export= c("applyRFMapParallel", "convertToNumeric"), .packages=c('raster','rgdal', 'ranger'), .options.snow=opts)  %dopar% { applyRFMapParallel(model, bs, templateR, theStack, chunkRasterDir, isnumeric, isLog)  }
  #result <- foreach(k=1:100,.export= c("applyRFMapParallel"), .packages=c('raster','rgdal', 'ranger'), .options.snow=opts)  %dopar% { applyRFMapParallel(model, bs, templateR, theStack, chunkRasterDir, isnumeric, isLog)  }
  
  close(pb)
  stopCluster(cl)
  gc()
  
  makerasterFromTreePredictionChunks(outDir=outDir, modelType = model$treetype, templateR = templateR, att = att, numcpus = numcpus, bs = bs)

  gc()
  
  if(model$forest$treetype == 'Classification'){
    print( model$forest$levels)
    print(model$forest$class.values)
  }
  toc()
  #return ()
}



ReportModelSummary <- function(rootOutDir, scriptRoot, modelPath, attribute, catchment ){
  
  
 templatePath <-  paste0(scriptRoot, "/Modelling/ModelReportTemplate.Rmd")
 
 outDirR <- paste0(rootOutDir,'/Attributes/',attribute, '/Reports')
 outfile <- paste0(outDirR, '/ModelReport_', attribute)
 
 if(!dir.exists(outDirR))
    dir.create(outDirR)
 
 ps = list(  modelPath = modelPath, attribute = attribute, catchment = catchment, rootDir = rootOutDir)
 rmarkdown::render(templatePath, params = ps, output_format = 'pdf_document', output_file = paste0(outfile, '.pdf'), quiet=T)
 rmarkdown::render(templatePath, params = ps, output_format = 'html_document', output_file = paste0(outfile, '.html'), quiet=T)
 rmarkdown::render(templatePath, params = ps, output_format = 'word_document', output_file = paste0(outfile, '.docx'), quiet=T)
 
 #rmarkdown::render(templatePath, params = ps, output_format = 'all', output_file = paste0(outfile))
 #rmarkdown::render(templatePath, params = ps, output_format = c('html_document', 'pdf_document'), output_file = paste0(outfile))

 
 png(file =  paste0(outDirR, '/',  attribute, '_', catchment,  ".png"), bg = "white", width = 1000, height = 1000)
 r1 <- raster(paste0(rootOutDir, '/Attributes/', attribute, '/', attribute, '.tif'))
 plot(r1, main = paste0(attribute, ' - ', catchment))
 dev.off()
 
 png(file =  paste0(outDirR, '/', attribute, '_', catchment,  "_Uncert.png"), bg = "white", width = 1000, height = 1000)
 r1 <- raster(paste0(rootOutDir, '/Attributes/', attribute, '/', attribute, '_Uncert.tif'))
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


predictChunkonHPC <- function(att = NULL, model = NULL, templateR = NULL, theStack = NULL, rootDir = NULL, outDir=NULL, deleteFiles=F, cpus = NULL, bs=NULL, isLog = F, itNum){
  
  tic()
  
  checkModelCovariates(model, theStack) 
  isnumeric <- is.number(model$forest$levels)
  
  outRasterDir <- paste0(outDir , '/Attributes/', att) 
  chunkRasterDir <- paste0(outRasterDir, '/Chunks')
  if(!(dir.exists(chunkRasterDir))){dir.create(chunkRasterDir, recursive=T )}

  print("")
  print(paste0('Running spatial model for ', att))
  print(paste0('Output directory = ', outDir))
  print(paste0('Chunk Directory - ', chunkRasterDir))
  print(paste0('Chunk Number = ', itNum))
  print(paste0('Is Numeric = ', isnumeric))

  k <<- as.numeric(itNum)
  applyRFMapParallel(model, bs, templateR, theStack, chunkRasterDir, isnumeric, isLog)  
  print(toc())

}



applyRFMapParallel <- function(model = NULL, bs = NULL, templateR = NULL, theStack = NULL, outDir = NULL, isnumeric=NULL, isLogAtt = F){

  bname = paste0(outDir, '/r_' , k, '.rds')
  print(bname)
  print(bs$nrows[k])
  
  if(!file.exists(bname)){

  ncells = as.numeric(bs$nrows[k]) * ncol(templateR)
  
  theSeq = seq(from=1,to=ncells)
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

      prediction = predict(model, covs[valsi,-1],  predict.all = T, num.threads=1)$predictions

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

      if(isLogAtt){
        outM <- exp(outM)
      }


      predsPlusCellID <- as.data.frame( cbind(valsi, outM))

   
    saveRDS(predsPlusCellID, bname)
   # fst::write_fst(predsPlusCellID, bname, compress = 100)
    gc()
  }
  }
}


summariseRFModel <- function(modelPath= NULL){
  
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
  #cats <-  as.numeric(as.character(model$forest$levels[ids]))
  cats <-  as.character(model$forest$levels[ids])
  decodes <- data.frame( RID = ids, Category = cats)
  decodesOrd <- decodes[order(decodes$RID),]
  return (decodesOrd)
  
}










