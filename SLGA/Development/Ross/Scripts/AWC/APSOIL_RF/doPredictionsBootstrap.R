library(raster)
library(rgdal)
library(ranger)
library(stringr)
library(tictoc)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory
#utils:::format.object_size(ncol(templateR)*4*50 * 50, "auto")



getMean <- function(v) {
return(mean(v))
}
getLow <- function(v){
  return(quantile(v, c(0.05)))
}
getHigh <- function(v){
  return(quantile(v, c(0.95)))
}

getContinuousPredictionSummary <- function(v) {
 #Not used - just left for info 
  o<-NULL
  m <- mean(v)
  o$m <- m
  
  qts = quantile(v, c(0.05, 0.95))
  low <- as.numeric(qts[1])
  high <- as.numeric(qts[2])
  
  
  return(o)
  # stdDev <- sd(v)
  # 
  # (stdDev/m)*100
  # 
  # ci <-(stdDev/m)*100
  # v = list(pred=m, confInd=ci)
}

#####################################################################
#########                                                  ##########
#########  Functions above. Actual processing starts here  ##########
#########                                                  ##########
#####################################################################

args = commandArgs(trailingOnly=T)
tic()


#   rootDir <- '/datasets/work/LW_ROWRA_WORK/3_Land_suitability/0_Working/Uta/Roper'
   # k=400
   # patt = 'DUL_0'
   # modelFileName = 'RFmodel_DUL_0.rds'
   # chunkLines = 50

   k = as.numeric(args[1])
   att = args[2]
   depth = args[3]
   chunkLines = as.numeric(args[4])
   isnumeric = as.logical(args[5])

   numBoots=50
   
   patt <- paste0(att, '_', depth)
   
print(paste0('args : k=', k, ' att=', att, 'depth=', depth, ' chunklines=', chunkLines, ' isnumeric=', isnumeric))
   
scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRF'

source(paste0(scriptDir, '/RFUtils.R'))


cat(paste0('Iteration No. = ', k, '\n'))

outChkDir <- paste0(workDir, '/Maps/', patt, '/Chunks')
if(!dir.exists(outChkDir)){dir.create(outChkDir, recursive = T)}

outfile <- paste0(workDir, '/Maps/', patt, '/Chunks/AllCellVals_', k, '.rds')
if ( !file.exists(outfile)){
  
  cat("\nLoading  Covariate Data.....\n")
  cat("-------------------------\n")
  covs <- read.csv(paste0('/datasets/work/af-digiscapesm/work/Ross/TERN', '/covsToUse.csv'), stringsAsFactors = F)
  covsToUse <- paste0(covDir, '/', covs[covs$UseAll==1, 1], '.tif')

  theStack <- stack(covsToUse)
  cat(paste0('\nNumber of covariates being used = ', nlayers(theStack), '\n'))
  names(theStack)
  
  templateR <- raster(paste0(covDir, '/Relief_dems_3s_mosaic1.tif'))
  cat('Template\n')
  cat('------------------\n')
  print(templateR)
  
  cat("\nTemplate successfully loaded\n")

  chk <-	getChunkInfo(chunkLines, nrow(templateR))
  
  ncells = chk$nlines[k] * ncol(templateR)
  
  theSeq = seq(ncells)
  covs = data.frame(theSeq)
  
  start_time <- Sys.time()
  print(start_time)

  for (i in 1:nlayers(theStack))
  {
    print(i)
    rl = raster(theStack, layer=i)
    v <- as.numeric(getValues(rl, row=chk$starts[k], nrows=chk$nlines[k]))
    
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
  
  tt <- Sys.time() - start_time
  
  cat('Covariate data successfully loaded in ', tt, ' mins\n\n')
  cat('Running model predictions on raster chunk .....\n')
  cat("--------------------------------------------------\n")
  cat('Data will be written to ', outfile, '\n')
  
  outDF <- data.frame(valsi)
  
  if(length(valsi) > 0){
    
    for (j in 1:numBoots) {

      print(paste0('Running model bootstrap number ', j))
        model <- readRDS(paste0(workDir, '/Models/Depth_', depth, '/RFmodel_', att,  '_', depth, '_', j, '.rds'))
        prediction = predict(model, covs[valsi,-1],  predict.all = F, num.threads=1)$predictions
        
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
        outDF <- cbind(outDF, outM)
    
    }

    
    mData <- apply(outDF[,-1], 1, getMean)
    lData <- apply(outDF[,-1], 1, getLow)
    hData <- apply(outDF[,-1], 1, getHigh)
    
    odf <- data.frame(valsi=outDF[,1], mean=mData, low=lData, high=hData)
    
    saveRDS(odf, outfile)
    
    cat('Model predictions run successfully\n\n')
    
    print(toc())
    cat(paste0('Iteration Finished Successfully'))
    
  }else{
    cat('\nNo data in chunk - ', paste0('cellSummaries_', k, '.rds'))
    cat(paste0('\nIteration Finished Successfully'))
  }
}else{
  
  cat('\nFile exists - ', paste0('cellSummaries_', k, '.rds'))
  cat(paste0('\nIteration Finished Successfully'))
}

