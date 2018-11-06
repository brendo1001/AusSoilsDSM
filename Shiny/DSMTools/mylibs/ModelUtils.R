library(epiR)


fitRegressionStats <- function(obsVal, modelVal, responseName, predName, outfilename = NULL, legPos='topright', subtitle='', verbose = F, showFitInfo = T)
{
  dfn <- na.omit(data.frame(obsVal,modelVal))
  obsVal <- dfn[,1]
  modelVal <- dfn[,2]
  
  cccC <- epi.ccc(obsVal, modelVal, ci = "z-transform",conf.level = 0.95)
  r.sqC <- cor(obsVal, modelVal)^2
  
  fitC <- lm(modelVal ~ obsVal-1, data=dfn)
  validC = data.frame(obsVal, modelVal)
  
  totAp <- sum(obsVal)
  totS <- sum(modelVal)
  prop <- totAp/totS
  
  
  minX = min(obsVal)
  maxX = max(obsVal)
  
  minY = min(modelVal)
  maxY = max(modelVal)
  #maxVal = max(maxX, maxY)
  
  
  plot(validC, main=paste0( 'Model Fit - ', predName, ' Vs ', responseName), xlab = predName, ylab = responseName , pch=3, cex =0.5, xlim = c(minX,maxX), ylim = c(minY,maxY))
  
  if(showFitInfo){
      abline(fitC, col="red")
      #abline(0,1, col="green")
      mtext(subtitle, cex=0.5)
      
      tx =  maxX *0.6
      print(maxY)
      ty1 =  minY * 1.05
      
      
      
      legend(legPos, c('Regression Line') , lty=1, col=c('red'), bty='n', cex=1)
      
      text(tx,ty1, paste("R2 = ",round(r.sqC, digits = 2)), pos=4)
      
      
  }
  
  if(verbose){
    
    # cat ("\n")
    # cat(paste("Model Fit Stats For - ", attName, "\n"))
    # cat(paste('-----------------------------------------------------', "\n"))
    # cat(paste("R2 = ", round(r.sqC, digits = 2), "\n"))
    # cat(paste("LCCC = ", round(cccC$rho.c[,1], digits = 2), "\n" ))
    # cat(paste("RMSE = ", round(mean((obsVal - modelVal)^2)^0.5, digits = 3), "\n" ))
    # cat(paste("ME = ", round(mean(modelVal-obsVal), digits = 3), "\n"))
    # cat(paste('Proportion = ',  ' - ', round(prop, digits = 3), '\n'))
    # cat(paste('Overall average Observed - ', ' = ', round(mean(obsVal/numYears), digits = 3), '\n'))
    # cat(paste('Overall average Modelled = ', ' = ', round(mean(modelVal/numYears), digits = 3), '\n'))
    # cat ("\n")
    # cat ( paste("Stats file written to :", outfilename, "\n"))
    # cat (paste('-----------------------------------------------------', "\n"))
    # cat ("\n")
    
    l1 <- c('Model Fit Stats For ', '', 'R2', 'LCCC', 'RMSE', 'ME', 'Proportion', 'Observed Average', 'Modelled Average')
    l2 <- c(paste0( 'Model Fit - ', predName, ' Vs ', responseName), '', round(r.sqC, digits = 2), round(cccC$rho.c[,1], digits = 2), round(mean((obsVal - modelVal)^2)^0.5, digits = 3),
            round(mean(modelVal-obsVal), digits = 3),  round(prop, digits = 3), round(mean(obsVal), digits = 3), round(mean(modelVal), digits = 3))
    
    outdf <- data.frame(Metric = l1, Value = l2)
    outdf
    
  }
  
  
  if(!is.null(outfilename)){
    file.create(outfilename)
    write(paste("Model Fit Stats For - ", paste0(predName , ' Vs ', responseName), "run on", Sys.time(), Sys.Date() ), file=outfilename, append=TRUE)
    write((paste('-----------------------------------------------------', "\n")))
    write(paste("R2 = ", round(r.sqC, digits = 2) ), file=outfilename, append=TRUE)
    write(paste("LCCC = ", round(cccC$rho.c[,1], digits = 2) ), file=outfilename, append=TRUE)
    write(paste("RMSE = ", round(mean((obsVal - modelVal)^2)^0.5, digits = 3) ), file=outfilename, append=TRUE)
    write(paste("ME = ", round(mean(modelVal-obsVal), digits = 3)), file=outfilename, append=TRUE)
    write(paste('Proportion = ', round(prop, digits = 3)), file=outfilename, append=TRUE)
    write(paste('Overall average Observed = ', round(mean(obsVal), digits = 3)), file=outfilename, append=TRUE)
    write(paste('Overall average Modelled = ', round(mean(modelVal), digits = 3)), file=outfilename, append=TRUE)
  }
  
  #outText <- paste0( round(r.sqC, digits = 2), ",",round(cccC$rho.c[,1], digits = 2), ",", round(mean((obsVal - modelVal)^2)^0.5, digits = 3), ",", round(mean(modelVal-obsVal), digits = 3), ",", round(prop, digits = 3), ",",  round(mean(obsVal/numYears), digits = 3), ",", round(mean(modelVal/numYears), digits = 3) )
  
  
  
  return (outdf)
  
}


fitStats <- function(obsVal, modelVal, attName = 'Att', outfilename = '', legPos='topright', subtitle='', numYears=1, verbose = F)
{
  dfn <- na.omit(data.frame(obsVal,modelVal))
  obsVal <- dfn[,1]
  modelVal <- dfn[,2]

  cccC <- epi.ccc(obsVal, modelVal, ci = "z-transform",conf.level = 0.95)
  r.sqC <- cor(obsVal, modelVal)^2

  fitC <- lm(modelVal ~ obsVal-1, data=dfn)
  validC = data.frame(obsVal, modelVal)

  totAp <- sum(obsVal)
  totS <- sum(modelVal)
  prop <- totAp/totS

  minVal = 0
  maxX = max(obsVal)
  maxY = max(modelVal)
  maxVal = max(maxX, maxY)


  plot(validC, main=paste( 'Model Fit', attName ), xlab='Observed', ylab = 'Predicted', pch=3, cex =0.5, xlim = c(minVal,maxVal), ylim = c(minVal,maxVal))
  abline(fitC, col="red")
  abline(0,1, col="green")
  mtext(subtitle, cex=0.5)

  tx =  maxVal *0.6
  ty1 =  maxVal * 0.05
  ty2 =  maxVal * 0.15

  legend(legPos, c('Regression Line', '1:1') , lty=1, col=c('red','green'), bty='n', cex=1)

  text(tx,ty1, paste("R2 = ",round(r.sqC, digits = 2)), pos=4)
  text(tx,ty2, paste("LCCC = ", round(cccC$rho.c[,1], digits = 2)), pos=4)
  
  
  if(verbose){

  # cat ("\n")
  # cat(paste("Model Fit Stats For - ", attName, "\n"))
  # cat(paste('-----------------------------------------------------', "\n"))
  # cat(paste("R2 = ", round(r.sqC, digits = 2), "\n"))
  # cat(paste("LCCC = ", round(cccC$rho.c[,1], digits = 2), "\n" ))
  # cat(paste("RMSE = ", round(mean((obsVal - modelVal)^2)^0.5, digits = 3), "\n" ))
  # cat(paste("ME = ", round(mean(modelVal-obsVal), digits = 3), "\n"))
  # cat(paste('Proportion = ',  ' - ', round(prop, digits = 3), '\n'))
  # cat(paste('Overall average Observed - ', ' = ', round(mean(obsVal/numYears), digits = 3), '\n'))
  # cat(paste('Overall average Modelled = ', ' = ', round(mean(modelVal/numYears), digits = 3), '\n'))
  # cat ("\n")
  # cat ( paste("Stats file written to :", outfilename, "\n"))
  # cat (paste('-----------------------------------------------------', "\n"))
  # cat ("\n")
  
    l1 <- c('Model Fit Stats For ', '', 'R2', 'LCCC', 'RMSE', 'ME', 'Proportion', 'Observed Average', 'Modelled Average')
    l2 <- c(attName, '', round(r.sqC, digits = 2), round(cccC$rho.c[,1], digits = 2), round(mean((obsVal - modelVal)^2)^0.5, digits = 3),
            round(mean(modelVal-obsVal), digits = 3),  round(prop, digits = 3), round(mean(obsVal/numYears), digits = 3), round(mean(modelVal/numYears), digits = 3))
    
    outdf <- data.frame(Metric = l1, Value = l2)
    outdf

  }


  if(!outfilename==""){
  file.create(outfilename)
  write(paste("Model Fit Stats For - ", attName, "run on", Sys.time(), Sys.Date() ), file=outfilename, append=TRUE)
  write((paste('-----------------------------------------------------', "\n")))
  write(paste("R2 = ", round(r.sqC, digits = 2) ), file=outfilename, append=TRUE)
  write(paste("LCCC = ", round(cccC$rho.c[,1], digits = 2) ), file=outfilename, append=TRUE)
  write(paste("RMSE = ", round(mean((obsVal - modelVal)^2)^0.5, digits = 3) ), file=outfilename, append=TRUE)
  write(paste("ME = ", round(mean(modelVal-obsVal), digits = 3)), file=outfilename, append=TRUE)
  write(paste('Proportion = ', round(prop, digits = 3)), file=outfilename, append=TRUE)
  write(paste('Overall average Observed = ', round(mean(obsVal/numYears), digits = 3)), file=outfilename, append=TRUE)
  write(paste('Overall average Modelled = ', round(mean(modelVal/numYears), digits = 3)), file=outfilename, append=TRUE)
  }

  #outText <- paste0( round(r.sqC, digits = 2), ",",round(cccC$rho.c[,1], digits = 2), ",", round(mean((obsVal - modelVal)^2)^0.5, digits = 3), ",", round(mean(modelVal-obsVal), digits = 3), ",", round(prop, digits = 3), ",",  round(mean(obsVal/numYears), digits = 3), ",", round(mean(modelVal/numYears), digits = 3) )

 
  
  return (outdf)
  
}

applyMapParallel<- function(model, templateR, theStack, outDir, bs){

  ncells <- bs$nrows[k] * ncol(templateR)
  theSeq <- seq(ncells)
  cubCovVals <- data.frame(theSeq)

  for (i in 1:nlayers(theStack))
  {
    rl <- raster(theStack, layer=i)
    v <- getValues(rl, row=bs$row[k], nrows=bs$nrows[k] )
    cubCovVals[names(rl)] <- v
  }

  prediction <- predict(model, cubCovVals[, -1])
  bname <- paste0(outDir, '/r_' , k, '.rds',  sep="")
  cat(bname)
  saveRDS(prediction, bname)
  
  prediction <- NULL
  cubCovVals<-NULL
  theSeq <- NULL
  theStack <- NULL
}




applyMapParallel2<- function(model, templateR, theStack, outDir, bs, covNamesinModel){
  
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
  
  prediction = predict(model, cubCovVals[, -1])
  bname = paste0(outDir, '/r_' , k, '.rds',  sep="")
  cat(bname)
  saveRDS(prediction, bname)
}

applyBootstrapMapParallel<- function(modelDir, attribute, templateR2, theStack, covsInModels, outDir, bs, numBoots, doCIs=F, limits){

  ncells = bs$nrows[k] * ncol(templateR2)
  theSeq = seq(ncells)
  cubCovVals = data.frame(theSeq)
  print(ncells)
  
  for (i in 1:nlayers(theStack)) 
  {
    rl = raster(theStack, layer=i)
    if(names(rl) %in% covsInModels){
      v <- getValues(rl, row=bs$row[k], nrows=bs$nrows[k] )
    }else{
      v <- rep(NA, ncells) # bit of a hack - df needs same structure as input df regardless of covariate usage in model
    }
    cubCovVals[names(rl)] <- v    
  }
  
  
  ids <- rep(0,ncells)
  bootOuts = data.frame(ids)
  for(j in 1:numBoots){
    
    print(paste0(modelDir, '/'  , attribute,'_CubistModel_' ,j, '.rds'))
    model<-readRDS(paste0(modelDir,'/' , attribute,'_CubistModel_' ,j, '.rds'))
    prediction = predict(model, cubCovVals[, -1])
    bootOuts[paste0('P_',j)] <- prediction
  }
  
  means <- apply(bootOuts[,-1], 1, mean, na.rm=TRUE)
  
  meanName = paste0(outDir, '/r_mean_' , k, '.rds',  sep="")
  cat(meanName)
  saveRDS(means, meanName)
  
  if(doCIs){
    cis <- apply(bootOuts[,-1], 1, quantile, probs = c(limits[[1]]/100, limits[[2]]/100),  na.rm = TRUE)
    lb <- cis[1,]
    ub <- cis[2,]
    lbName = paste0(outDir,  '/r_', limits[[1]], '_' , k, '.rds',  sep="")
    cat(lbName)
    saveRDS(lb, lbName)
    ubName = paste0(outDir,  '/r_', limits[[2]], '_'  , k, '.rds',  sep="")
    cat(ubName)
    saveRDS(ub, ubName)
  }
}


applyBootstrapMapParallel2.5D<- function(modelDir, attribute, templateR, theStack, covsInModels, outDir, bs, numBoots, doCIs=F, limits){
  
  cat(paste0(k, ".."))
  #k=20
  ncells = bs$nrows[k] * ncol(templateR)
  theSeq = rep(2.5, ncells) #seq(ncells)
  cubCovVals = data.frame(theSeq)
  colnames(cubCovVals)[1]<- 'Depth1'
  cubCovVals$Depth2 <- rep(10, ncells)
  cubCovVals$Depth3 <- rep(22.5, ncells)
  cubCovVals$Depth4 <- rep(45, ncells)
  cubCovVals$Depth5 <- rep(80, ncells)
  cubCovVals$Depth6 <- rep(150, ncells)
 

  for (i in 1:nlayers(theStack)) 
  {
    rl = raster(theStack, layer=i)
    if(names(rl) %in% covsInModels){
      v <- getValues(rl, row=bs$row[k], nrows=bs$nrows[k] )
    }else{
      v <- rep(NA, ncells) # bit of a hack - df needs same structure as input df regardless of covariate usage in model
    }
    cubCovVals[names(rl)] <- v    
  }
  
 
 d1<- cubCovVals[,!(names(cubCovVals) %in% c('Depth2','Depth3','Depth4','Depth5','Depth6'))]
 colnames(d1)[1] <- 'Depth'
 d2<- cubCovVals[,!(names(cubCovVals) %in% c('Depth1','Depth3','Depth4','Depth5','Depth6'))]
 colnames(d2)[1] <- 'Depth'
 d3<- cubCovVals[,!(names(cubCovVals) %in% c('Depth1','Depth2','Depth4','Depth5','Depth6'))]
 colnames(d3)[1] <- 'Depth'
 d4<- cubCovVals[,!(names(cubCovVals) %in% c('Depth1','Depth2','Depth3','Depth5','Depth6'))]
 colnames(d4)[1] <- 'Depth'
 d5<- cubCovVals[,!(names(cubCovVals) %in% c('Depth1','Depth2','Depth3','Depth4','Depth6'))]
 colnames(d5)[1] <- 'Depth'
 d6<- cubCovVals[,!(names(cubCovVals) %in% c('Depth1','Depth2','Depth3','Depth4','Depth5'))]
 colnames(d6)[1] <- 'Depth'
  
  ids <- rep(0,ncells)
  bootOuts1 = data.frame(ids)
  bootOuts2 = data.frame(ids)
  bootOuts3 = data.frame(ids)
  bootOuts4 = data.frame(ids)
  bootOuts5 = data.frame(ids)
  bootOuts6 = data.frame(ids)
  
  for(j in 1:numBoots){
    #for(j in 1:2){
    cat(paste0(j,'..'))
    mName<-paste0(modelDir, '/', attribute, '_CubistModel_' ,j, '.rds')
    print(mName)
    model<-readRDS(mName)

    prediction1 = predict(model, d1)
    bootOuts1[paste0('P_',j)] <- prediction1
    cat(k)
    
#     prediction2 = predict(model, d2)
#     bootOuts2[paste0('P_',j)] <- prediction2
#     
#     prediction3 = predict(model, d3)
#     bootOuts3[paste0('P_',j)] <- prediction3
#     
#     prediction4 = predict(model, d4)
#     bootOuts4[paste0('P_',j)] <- prediction4
#     
#     prediction5 = predict(model, d5)
#     bootOuts5[paste0('P_',j)] <- prediction5
    
    prediction6 = predict(model, d6)
    bootOuts6[paste0('P_',j)] <- prediction6
  }

  means1 <- apply(bootOuts1[,-1], 1, mean, na.rm=TRUE)
#   means2 <- apply(bootOuts2[,-1], 1, mean, na.rm=TRUE)
#   means3 <- apply(bootOuts3[,-1], 1, mean, na.rm=TRUE)
#   means4 <- apply(bootOuts4[,-1], 1, mean, na.rm=TRUE)
#   means5 <- apply(bootOuts5[,-1], 1, mean, na.rm=TRUE)
  means6 <- apply(bootOuts6[,-1], 1, mean, na.rm=TRUE)
  
  meanName1 = paste0(outDir, '/r_mean_' , k, '_1.rds',  sep="")
#   meanName2 = paste0(outDir, '/r_mean_' , k, '_2.rds',  sep="")
#   meanName3 = paste0(outDir, '/r_mean_' , k, '_3.rds',  sep="")
#   meanName4 = paste0(outDir, '/r_mean_' , k, '_4.rds',  sep="")
#   meanName5 = paste0(outDir, '/r_mean_' , k, '_5.rds',  sep="")
  meanName6 = paste0(outDir, '/r_mean_' , k, '_6.rds',  sep="")
  
  #print(meanName)
  saveRDS(means1, meanName1)
#   saveRDS(means2, meanName2)
#   saveRDS(means3, meanName3)
#   saveRDS(means4, meanName4)
#   saveRDS(means5, meanName5)
  saveRDS(means6, meanName6)
  
  if(doCIs){
    cis <- apply(bootOuts1[,-1], 1, quantile, probs = c(limits[[1]]/100, limits[[2]]/100),  na.rm = TRUE)
    lb <- cis[1,]
    ub <- cis[2,]
    lbName = paste0(outDir, '/r_', limits[[1]], '_' , k, '_1.rds',  sep="")
    cat(lbName)
    saveRDS(lb, lbName)
    ubName = paste0(outDir,  '/r_', limits[[2]], '_'  , k, '_1.rds',  sep="")
    cat(ubName)
    saveRDS(ub, ubName)
  }
}

applyBootstrapMapParallel2.5D3<- function(depthsToModel, modelDir, attribute, templateR, theStack, covsInModels, outDir, bs, numBoots, doCIs=F, limits){
  
  cat(paste0(k, ".."))
  #k=200
  
  
  ncells = bs$nrows[k] * ncol(templateR)
  theSeq = seq(ncells)
  cubCovVals = data.frame(theSeq)
  
  for (i in 1:nlayers(theStack)) 
  {
    rl = raster(theStack, layer=i)
    if(names(rl) %in% covsInModels){
      v <- getValues(rl, row=bs$row[k], nrows=bs$nrows[k] )
    }else{
      v <- rep(NA, ncells) # bit of a hack - df needs same structure as input df regardless of covariate usage in model
    }
    cubCovVals[names(rl)] <- v    
  }
  
  
  
  for (i in 1:nrow(depthsToModel))
  {
    
    lno <- depthsToModel[i,1]
    theDepth <- depthsToModel[i,2]
    
    theDepthNums <- rep(theDepth,ncells)
    
    cubData<- data.frame(theDepthNums, cubCovVals[,-1])
    colnames(cubData)[1] <- 'Depth'
    
    ids <- rep(0,ncells)
    bootOuts = data.frame(ids)
    
    for(j in 1:numBoots){
      
      mName<-paste0(modelDir, '/', attribute, '_CubistModel_' ,j, '.rds')
      #print(mName)
      model<-readRDS(mName)
      
      prediction = predict(model, cubData)
      bootOuts[paste0('P_',j)] <- prediction
    }
    
    theMeans <- apply(bootOuts[,-1], 1, mean, na.rm=TRUE)
    meanName = paste0(outDir, '/r_mean_' , k, '_', lno, '.rds',  sep="")
    saveRDS(theMeans, meanName)
    
    if(doCIs){
      cis <- apply(bootOuts[,-1], 1, quantile, probs = c(limits[[1]]/100, limits[[2]]/100),  na.rm = TRUE)
      lb <- cis[1,]
      ub <- cis[2,]
      lbName = paste0(outDir, '/r_', limits[[1]], '_' , k, '_', lno, '.rds',  sep="")
      saveRDS(lb, lbName)
      ubName = paste0(outDir,  '/r_', limits[[2]], '_'  , k, '_', lno, '.rds',  sep="")
      saveRDS(ub, ubName)
    }
  }
}



createTrainingSample <- function(dataframe, seed=NULL, percentOfTraining) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  #trainindex <- sample(index, trunc(length(index)/percentOftest))
  trainindex <- sample(index, trunc(length(index) * percentOfTraining/100),replace = FALSE, prob = NULL)
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  
  colnames(trainset) <- colnames(dataframe)
  colnames(testset) <- colnames(dataframe)

  list(trainset=trainset,testset=testset)
}

getBootstrapSamples <- function( i, bootSamples, outDir){
  cat(paste0(i, ".."))
  trainsetx = bootSamples[sample(1:nrow(bootSamples),replace=T),]
  write.table(trainsetx, paste0(outDir, '/b_', i,  '_trainingData.csv'), sep=",", row.names=F)
  trainset
}

getBootstrapSamplesKeepValidationSet <- function( i, bootSamples, outDir, attribute){
  cat(paste0(i, ".."))
  trainsetx = bootSamples[sample(1:nrow(bootSamples),replace=T),]
  bootValid <- trainsetx[ ! trainsetx %in% bootSamples ]
  bootValidIDs <- setdiff( bootSamples$SID, trainsetx$SID)
  validDF <- data.frame(bootValidIDs)
  colnames(validDF) <- 'SID'
  vldAtts = merge(validDF, bootSamples, by.x = 'SID', by.y = 'SID')

  write.table(trainsetx, paste0(outDir, '/b_', i,'_', attribute,  '_trainingData.csv'), sep=",", row.names=F)
  write.table(vldAtts, paste0(outDir, '/b_', i,'_', attribute,  '_validationData.csv'), sep=",", row.names=F)
  #trainset
}

runCubistModel <-function(){
  cubistIn<-read.table(paste0(cubistInputsDir, '/b_',k,'_', trainingData.FieldName, '_trainingData_With_Attributes.csv'), sep=",", header=T,strip.white=T)
  model <- cubist(x = cubistIn[, -1:-4], y = cubistIn[,2], committees= cubist.Committees,  cubistControl( label = trainingData.FieldName, cubist.MaxRules))
  #summary(model)
  modelFilename <- paste0(cubistModelDir, '/', trainingData.FieldName,'_CubistModel_', k)
  saveRDS(model, paste0(modelFilename, '.rds'))
  outfilename <-paste0(modelFilename, '.rules')
  file.create(outfilename)
  modelText <- summary(model)
  writeLines(modelText$output, outfilename)
  model
}

runCubistModel2 <-function(inDir, attName, maxRules, noCommittees){
 
  print(paste0(inDir, '/b_',k,'_', attName, '_trainingData.csv'))
   cubistIn<-read.table(paste0(inDir, '/b_',k,'_', attName, '_trainingData.csv'), sep=",", header=T,strip.white=T)
  model <- cubist(x = cubistIn[, -1:-2], y = cubistIn[,2], committees= noCommittees,  cubistControl( label = attName, maxRules))
  #print(summary(model))
  modelFilename <- paste0(cubistModelDir, '/', attName,'_CubistModel_', k)
  saveRDS(model, paste0(modelFilename, '.rds'))
  outfilename <-paste0(modelFilename, '.rules')
  file.create(outfilename)
  modelText <- summary(model)
  writeLines(modelText$output, outfilename)
  #model
}

getModelCovariateStack <- function(covariatesToUse, covNamesinModel, crootDirPath)
{
  covariatesToUse<-covsToUse
  theModelStack = stack()
  for (i in 1:nrow(covariatesToUse)) 
  {
    if(covsToUse$Filename[i] %in%  covNamesinModel ){
      
      rname <- paste0(crootDirPath ,'/', covsToUse$Directory[i], '/', covsToUse$Filename[i], '.tif')
      if(file.exists(rname))
      {
        print(paste0(rname))
        r = raster(paste0(rname))
        theModelStack<- addLayer(theModelStack,r)
      }
    }
  }
  return (theModelStack)
}

getCovariatesUsedInModelFromSummaryFile <- function(FileName)
{
  print(FileName)
  con <- file(FileName, open = "r") 
  
  cov.list = NULL
  i <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if(line == '	Attribute usage:'){
      
      throwaway = readLines(con, 2)
      while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) 
      {
        if(line == ''){break}
        y<- substring(line, 19, nchar(line))
        print(y)
        cov.list = append(cov.list, y)
        i <- i+1
      }
    }
  } 
  
  close(con)
  
  return (cov.list)
}

getCovariatesUsedInModel <- function(model)
{
   m<-model
   usage <- m$usage
   covs<-usage[usage$Conditions > 0 | usage$Model > 0, ]
   return(covs$Variable)
}

getCovariatesUsedBootstrapModels <- function(numBoots, modelDir, attribute)
{
    v <- character()
    for(i in 1:numBoots){
      model<-readRDS(paste0(modelDir, '/'  , attribute,'_CubistModel_' ,i, '.rds'))
      covNamesinModel <- getCovariatesUsedInModel(model)
      for(j in 1:length(covNamesinModel))
      {
        if(!covNamesinModel[j] %in% v)
        {
          v <- c(v, covNamesinModel[j]);
        }
      }
    }
    return (v)
}



