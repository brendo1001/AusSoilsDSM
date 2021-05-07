library(raster)
library(ranger)
library(hexbin)
library(Cubist)
library(stringr)
library(ggplot2)

rasterOptions(progress = 'text', chunksize=1e+08,maxmemory=1e+09)

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
rootDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN'

source(paste0(scriptDir, '/GeneralUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))
source(paste0(scriptDir, '/RandomForestUtils_V2.R'))

numBoots=50


#fls <- list.files(covDir, pattern = '.tif$', full.names = T)
#write.csv(str_remove(basename(fls), '.tif'), paste0('/datasets/work/af-digiscapesm/work/Ross/TERN/covsToUse.csv'))
covs <- read.csv(paste0(rootDir, '/covsToUse.csv'), stringsAsFactors = F)
covsToUse <- paste0(covDir, '/', covs[covs$UseAll==1, 1], '.tif')
covsNotToUse <-covs[covs$UseAll==0, 1]
stk<-stack(covsToUse)


inDF <- read.csv('/datasets/work/af-digiscapesm/work/APSOIL_spatialmodelling/APSOIL_splined_with_allcovariates.csv')

pts <- inDF[,1:9]
head(pts, 20)
coordinates(pts)<- ~ Longitude + Latitude
crs(pts)<- crs(stk)
drill <- extract(stk, pts, df=T)

alldf <- cbind(pts@data, drill)
cc <- complete.cases(alldf)

# Replace NAs with mean values 
for (i in 9:ncol(alldf)) {
  #print(paste0(colnames(alldf)[i], ' = ', sum(is.na(alldf[,i]))))
  m <- mean(alldf[i][,1], na.rm=T)
  idxs<-which(is.na(alldf[,i]))
  alldf[idxs, i] <- m
}
write.csv(alldf, paste0(rootDir, '/AWC/ApsoilRF/covsDrillApsoil_noNAs.csv'), row.names = F)




alldf <- read.csv(paste0(rootDir, '/AWC/ApsoilRF/covsDrillApsoil_noNAs.csv'), check.names=FALSE)

ggplot(alldf, aes(x=factor(std_upper_depth), y=DUL, fill=factor(std_upper_depth))) + 
  geom_boxplot()

ds <- c(0,5,15,30,60,100)
for (i in 1:length(ds)) {
  
  ddf <- alldf[alldf$std_upper_depth == ds[i], ]
  hist(ddf$DUL, main=paste0("Histogram of DUL ", ds[i], 'cm'))
  d <- density(na.omit(ddf$DUL))
  plot(d, main=paste0("Kernel Density of DUL ", ds[i], 'cm'))
  polygon(d, col="blue", border="blue")
  print(paste0('number of samples = ', length(ddf$DUL)))
  print(summary(ddf$DUL))
}



#########  Start here for new model generation

#att = 'DUL'
att = 'LL15'
# depth = 0
# depth = 5
# depth = 15
# depth = 30
# depth = 60
 depth = 100

alldf <- read.csv(paste0(rootDir, '/AWC/ApsoilRF/covsDrillApsoil_noNAs.csv'), check.names=FALSE)
colnames(alldf) <- str_replace_all(colnames(alldf), '[.]', '-')

alldf <- alldf[alldf$std_upper_depth == depth, ]

covs<- read.csv(paste0(rootDir, '/covsToUse.csv'), stringsAsFactors = F)
covsToUse <- covs[covs$UseAll==1, 1]
idxs <- which(!is.na(match(colnames(alldf), covsToUse)))
alldf <- alldf[, c(1:8, idxs)]

dSet <- alldf
head(dSet)

#####  Make bootstraps  - dont do this for different atts use existing bootstraps


bootDir <- paste0(rootDir, '/AWC/ApsoilRF/Boots/Depth_', depth)
if(!dir.exists(bootDir)){dir.create(bootDir)}
for (i in 1:numBoots) {
  print(i)
  trainsetx = dSet[sample(1:nrow(dSet),replace=T),]
  bootValid <- setdiff( dSet , trainsetx )
  write.csv(trainsetx, paste0(bootDir, '/AWC_boot_train_', att, '_', depth, '_', i,'.csv'), row.names = F)
  write.csv(bootValid, paste0(bootDir, '/AWC_boot_valid_', att, '_', depth, '_', i,'.csv'), row.names = F)
}





bootDir <- paste0(rootDir, '/AWC/ApsoilRF/Boots/Depth_', depth)

for (i in 1:numBoots) {
  print(i)
  # trainSet <- read.csv(paste0(bootDir, '/AWC_boot_train_', att, '_', depth, '_', i, '.csv'))
  # validSet <- read.csv(paste0(bootDir, '/AWC_boot_valid_', att, '_', depth, '_', i, '.csv'))
  
   trainSet <- read.csv(paste0(bootDir, '/AWC_boot_train_DUL_', depth, '_', i, '.csv'))
   validSet <- read.csv(paste0(bootDir, '/AWC_boot_valid_DUL_', depth, '_', i, '.csv'))
  

  ##  Tidy up inputs
    tobs <- trainSet[,att]
    idxsy <- which(is.na(tobs))
    if(length(idxsy) > 0 ){
      tobs <- tobs[-idxsy]
      tcovs <- trainSet[-idxsy, c(9:ncol(trainSet))]
    }else{
      tcovs <- trainSet[, c(9:ncol(trainSet))]
    }
    
    vobs <- validSet[,att]
    idxsyv <- which(is.na(vobs))
    if(length(idxsyv) > 0 ){
     vobs <- vobs[-idxsyv]
     vcovs <- validSet[-idxsyv, c(9:ncol(validSet))]
    }else{
      vcovs <- validSet[, c(9:ncol(validSet))]
    }
    
    #### Generate a RF model
    Rmodel <- ranger(tobs ~ ., data = tcovs, write.forest = TRUE, importance = 'impurity', num.trees = 500)
    modelDir <- paste0(rootDir, '/AWC/ApsoilRF/Models/Depth_', depth)
    if(!dir.exists(modelDir))dir.create(modelDir, recursive = T)
    RmodelPath = paste0(modelDir, '/RFmodel_', att, '_', depth, '_', i, '.rds')
    saveRDS(Rmodel,RmodelPath )
    Rmodel <- readRDS(RmodelPath)
    summariseRFModel( RmodelPath, att)
    
    preds = predict(Rmodel, data=vcovs,  predict.all = F)
    Rtdf <- data.frame(vobs, preds$predictions)
    RmodelFilename <- paste0(rootDir, '/AWC/ApsoilRF/Models/Depth_', depth, '/RFmodel_', att, '_', depth, '_', i)
    Coutfilename <-paste0(RmodelFilename, '.rules')
    fitStats(Rtdf[1],Rtdf[2], paste0(att,' ', depth, ' - RF'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
    write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
}

