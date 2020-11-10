library(raster)
library(tictoc)


getContinuousPredictionSummary <- function(v, type) {
  
  if(type=='mean'){
    m <- mean(v , na.rm = T)
  }else if(type=='sd'){
    m <- as.numeric(sd(v, na.rm = T))
  }else if(type=='min'){
    m <- as.numeric(min(v, na.rm = T))
  }else if(type=='max'){
    m <- as.numeric(max(v, na.rm = T))
  }else if(type=='med'){
    m <- as.numeric(median(v, na.rm = T))
  }
  
  d <-  as.numeric(unlist(m[[1]]))
 return(d)
}

tic()

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])
att <- args[2]
rootDir <- args[3]

outputs <- c('mean', 'min', 'max', 'med', 'sd')


outDir <- paste0(rootDir, '/SMOSStats/')
if(!dir.exists(outDir)) {dir.create(outDir, recursive = T)}

print(paste0('K = ', k))

fls <- list.files(path = paste0(rootDir, '/', att), full.names = T, recursive = T, pattern = '.flt$')
length(fls)
print(fls)
rtm <- raster(fls[1])
bs <- blockSize(rtm, chunksize = 100000)
#saveRDS(bs, paste0(outDir, '/chunks.rds') )

ncells = as.numeric(bs$nrows[k]) * ncol(rtm)
theSeq = seq(from=1,to=ncells)
covs = data.frame(theSeq)

print('Loading Data....')
for (i in 1:length(fls[1:20])) {

  print(i)
  rl = raster(fls[i])
  v <- getValues(rl, row=bs$row[k], nrows=bs$nrows[k])

  v[v<=0] <- NA
  covs[names(rl)] <- v
}

v1 <- covs[,-1]
# valsidx <- which(!is.na(v1[,2]))
# valsnd <- which(is.na(v1[,2]))



#object.size(covs)
for(i in 1:length(outputs)){
  
  outfile1 <- paste0(outDir,'/', k, '_',outputs[i],'.rds' )
 # if(!file.exists(outfile1)){
  print(paste0('calculating ',  outputs[i]))
  out <- numeric(nrow(v1))
  out[]<-NA
  sumData <- apply(v1, 1, getContinuousPredictionSummary, outputs[i])
  out <- sumData
  
  saveRDS(out, outfile1 )
  # }else{
  #   print(paste0('File exists - ', outfile1))
  # }
}

toc()
print(paste0('Finished Successfully'))




