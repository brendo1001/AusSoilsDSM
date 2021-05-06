library(raster)
library(ranger)
library(hexbin)
library(Cubist)
library(stringr)

rasterOptions(progress = 'text', chunksize=1e+08,maxmemory=1e+09)

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN'

source(paste0(scriptDir, '/GeneralUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))
source(paste0(scriptDir, '/RandomForestUtils_V2.R'))



#fls <- list.files(covDir, pattern = '.tif$', full.names = T)
#write.csv(str_remove(basename(fls), '.tif'), paste0('/datasets/work/af-digiscapesm/work/Ross/TERN/covsToUse.csv'))
covs <- read.csv(paste0(workDir, '/covsToUse.csv'), stringsAsFactors = F)
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




write.csv(alldf, paste0(workDir, '/AWC/ApsoilRF/covsDrillApsoil_noNAs.csv'), row.names = F)
alldf <- read.csv(paste0(workDir, '/AWC/ApsoilRF/covsDrillApsoil_noNAs.csv'))

idxs <- which(!is.na(match(colnames(alldf), covsNotToUse)))
alldf <- alldf[, -idxs]

modelling.samplePercentage = 70


splitSamples <-createTrainingSample(alldf, 1, modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(alldf)
colnames(validSet) <- colnames(alldf)


write.table(trainSet, paste0(workDir, '/trainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/validationData.csv'), sep=",", row.names=F)

trainSet <- read.csv(paste0(workDir, '/trainingData.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/validationData.csv'), stringsAsFactors = F)


###   Generate a cubist model for DUL
att = 'DUL'
depth = 0

trainSet <- trainSet[trainSet$std_upper_depth == depth, ]
validSet <- validSet[validSet$std_upper_depth == depth, ]
str(trainSet)

tobs <- trainSet[,att]
idxsy <- which(is.na(y))
if(length(idxsy) > 0 ){
  tobs <- y[-idxsy]
  tcovs <- trainSet[-idxsy, c(9:ncol(trainSet))]
}else{
  tcovs <- trainSet[, c(9:ncol(trainSet))]
}

vobs <- validSet[,att]
idxsyv <- which(is.na(yv))
if(length(idxsyv) > 0 ){
 vobs <- yv[-idxsy]
 vcovs <- validSet[-idxsyv, c(9:ncol(validSet))]
}else{
  vcovs <- validSet[, c(9:ncol(validSet))]
}


Cmodel <- cubist(x = tcovs, y = tobs, committees=10, cubistControl( rules = 100, extrapolation = 100))
summary(Cmodel)

CmodelFilename <- paste0(workDir, '/Models/model_Cubist_', att, '_', depth)
saveRDS(Cmodel, paste0(CmodelFilename, '.rds'))

Cmodel <- readRDS(paste0(CmodelFilename, '.rds'))

Coutfilename <-paste0(CmodelFilename, '.rules')
file.create(Coutfilename)
modelText <- summary(Cmodel)
writeLines(modelText$output, Coutfilename)

preds <- predict(Cmodel, vcovs)
Ctdf <- data.frame(vobs, preds)


fitStats(obsVal=Ctdf[1],modelVal=Ctdf[2], attName=paste0(att, ' - Cubist'), outfilename=paste0(CmodelFilename, '_ModelStats.txt'), legPos='topleft',subtitle='', numYears=1, verbose = T)

write.table(Ctdf, paste0(CmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
hexbinplot( Ctdf$Cmv ~ Ctdf$yp, colramp= function(n){BTC(n,beg=15,end=225)})



#### Generate a RF model
Rmodel <- ranger(tobs ~ ., data = tcovs, write.forest = TRUE, importance = 'impurity', num.trees = 500)
RmodelPath = paste0(workDir, '/Models/RFmodel_', att, '_', depth, '.rds')
saveRDS(Rmodel,RmodelPath )
Rmodel <- readRDS(RmodelPath)
summariseRFModel( RmodelPath, att)

preds = predict(Rmodel, data=vcovs,  predict.all = F)
Rtdf <- data.frame(vobs, preds$predictions)
RmodelFilename <- paste0(workDir, '/Models/RFmodel_', att, '_', depth)
Coutfilename <-paste0(RmodelFilename, '.rules')
fitStats(Rtdf[1],Rtdf[2], paste0(att, ' - RF'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)

hexbinplot( Rtdf$Rmv.predictions ~ Rtdf$yp)

########  subset the list of covariate to the most important ones
covImp <- names(Rmodel$variable.importance[order(Rmodel$variable.importance, decreasing = T)])
numCovs=30
idxs = c()
for (i in 1:numCovs) {
  #print(which(colnames(trainSet) == covImp[i]))
  #idxs[i]<- which(colnames(trainSet) == covImp[i])
  idxs <- c(idxs, which(colnames(trainSet) == covImp[i]))
  
}
trainSet <- trainSet[, c(1:8, idxs)]
validSet <- validSet[, c(1:8, idxs)]
head(trainSet)
# now go back to line and rerun and see what happens

