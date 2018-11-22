library(raster)
library(doParallel)
library(Cubist)
library(rgdal)


mPath <- 'C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/LocalPC/r4/Models/20181018-PYB_data!SiteID!5'
covDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/LocalPC/r4/Covariates'
scratchDir <- 'c:/temp/modelling'
templateR <- raster('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/LocalPC/r4/GeoTemplate/Template.tif')
minBlocks <- 30
numReps <- 2
numFolds <- 2

depth = 5


dir.create(scratchDir)

mPfs <- list.files(mPath, '.rds', full.names = T)

models <- lapply(mPfs,function(i){ readRDS(i)})
covsinMod <- lapply(models,function(i){ getCovariatesUsedInModel(i)})
covNamesinModel <- unique(do.call(c, covsinMod))

rpaths <- list.files( covDir, pattern = paste0( '.tif$'), full.names = T, recursive =F)
covpaths <- rpaths[!grepl('_RedRes', rpaths, ignore.case = T)]
stk <- stack(covpaths)


numCPUs = detectCores()-1



bs <- blockSize(stk, minblocks = minBlocks)
saveRDS(bs, paste0(scratchDir, '/brickInfo.rds'))
cat(paste0('Using ', bs$n, ' blocks'), sep='\n')
cl<-makeCluster(numCPUs)
registerDoParallel(cl)

itCnt <- numReps * numFolds

pp <- ceiling(minBlocks/numCPUs)

cntr=1
lcnt <-0
for (i in 1:bs$n) {
  

  print(p)
  
  totCnt <- min((i * numCPUs), minBlocks)

  r <- foreach(k=1:bs$n,  .packages=c('raster','rgdal', 'Cubist')) %dopar% applyMapParallelWithDepthBooty(models, templateR, stk, depth, scratchDir, bs, covNamesinModel,doCIs=F)

}

stopCluster(cl)



k=15
applyMapParallelWithDepthBooty(models, templateR, theStack=stk, depth, outDir=scratchDir, bs, covNamesinModel)
  
  
  
 m<- models[[1]]
  
  
  
  

