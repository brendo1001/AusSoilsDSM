


convertToNumeric <- function(x){ as.numeric(as.character(model$forest$levels[x])) }
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
  x = list(pred=m, SD=stdDev)
  x
}


getChunk <- function(chunkLines, tot, i){
  
  remainder <- tot%%chunkLines
  
  if(remainder > 0){
    chunks <- ceiling(tot/chunkLines)
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
    nlines[length(nlines)] <- remainder
    
  }else{
    chunks <- tot/chunkLines
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
  }
  
  return(list(starts=starts[i], nlines=nlines[i]))
}

getChunkInfo <- function(chunkLines, tot){
  
  remainder <- tot%%chunkLines
  
  if(remainder > 0){
    chunks <- ceiling(tot/chunkLines)
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
    nlines[length(nlines)] <- remainder
    
  }else{
    chunks <- tot/chunkLines
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
  }
  
  return(list(chunks=length(starts), starts=starts, nlines=nlines))
}
