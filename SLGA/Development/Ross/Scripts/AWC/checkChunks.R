
rootDir2 <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRF'

att='DUL'
depth = 15
chks<- 2040

# Check if files exist and have data
for (i in 1:chks) {
  cat(paste0(i, '_'))
  f <- paste0(rootDir2, '/Maps/', att, '_', depth, '/Chunks/AllCellVals_', i, '.rds')
  if(!file.exists(f)){
    print(paste0('Missing - ', f))
  }else{
    blockVals <- readRDS(f)
    if(all(is.na(blockVals[,2]))){
      print(paste0(i, ' is all NA values'))
    }
  }
}


# Just check if files exist
for (i in 1:chks) {
  f <- paste0(rootDir2, '/Maps/', att, '_', depth, '/Chunks/AllCellVals_', i, '.rds')
  if(!file.exists(f)){
    print(paste0('Missing - ', f))
  }
}