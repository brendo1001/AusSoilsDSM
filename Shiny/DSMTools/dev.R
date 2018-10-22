



templateR <- raster('C:/Projects/AgDataShop/Data/Demo/MachineryData/Harvest/2010/Rasters/_Barley_2010.tif')




paths <- list.files( paste0('C:/Projects/AgDataShop/TimsPlace/covs'), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)
outDir <- 'C:/Projects/Myanmar/Covs'
for (i in 1:length(paths)) {
  
  inPath <- paths[i]
  outName <- basename(inPath)
  r <- raster(inPath)
  pr <- projectRaster(r, crs=crs(templateR))
  aggregate(pr,  fact=3, fun=mean, expand=TRUE, na.rm=TRUE, filename=paste0(outDir, '/', outName), overwrite=T)
  
}


r <- raster('C:/Projects/Myanmar/Covs/aspect2.tif')
r

inDir <- paste0(rootDir, '/', currentUser)
list.dirs(inDir, full.names = F, recursive = F)

rsum <- paste0('dimensions  : ', dim(r)[1], ' ',dim(r)[2], ' ', dim(r)[1] * dim(r)[2],  ' (nrow, ncol, ncell) <br>')
rsum <- paste0(rsum, 'resolution  : ', res(r)[1], ' ',res(r)[2], ' (x, y) <br>')

reso <- dim(r)
str(reso)

pretty

format(3333333.3333333333333, scientific = F, digits = 3)
x=3333333.3333333333333
format(round(x, 2), nsmall = 2)



rsum <- paste0('dimensions  : ', dim(r)[1], ' ',dim(r)[2], ' ', dim(r)[1] * dim(r)[2],  ' (nrow, ncol, ncell) \n')
rsum <- paste0(rsum, 'resolution  : ',format(round(res(r)[1], 5), nsmall = 2) , ' ',format(round(res(r)[2], 5), nsmall = 2), ' (x, y) \n')
rsum <- paste0(rsum, 'extent      : ', format(round(extent(r)[1], 5), nsmall = 2), ' ',format(round(extent(r)[2], 5), nsmall = 2), ' ', format(round(extent(r)[3], 5), nsmall = 2), ' ', format(round(extent(r)[4], 5), nsmall = 2), '    (xmin, xmax, ymin, ymax) \n')
rsum <- paste0('coord. ref. : ', crs(r))
rsum <- paste0('values      : ', minValue(r), ' ', minValue(r), '  (min, max)')
inMemory()

print(rsum)

r
min(r[])


dens <- density(getValues(r),adjust = .5)


rootDir = 'C:/Projects/AgDataShop/SLGA'
covariates.rootDirPath  = 'd:/TERNSoils/National_digital_soil_property_maps'
covariates.outrootDirPath = paste0( 'C:/Projects/Myanmar/SLGA/')
covsToUse = list.files( paste0(covariates.rootDirPath), pattern = '*\\.tif$', full.names = T, recursive = T)
covsToUse1 <- covsToUse[!grepl("Associated", covsToUse, ignore.case = T)]
covsToUse2 <- covsToUse1[grepl("_EV_", covsToUse1, ignore.case = T)]

clipTemplate <- raster('C:/Projects/Myanmar/Covs/rad_k.tif')
clipArea <- extent(clipTemplate)



theStack = stack()
for (i in 1:length(covsToUse2)) 
{
  rname <-covsToUse2[i]
  
  if(file.exists(rname)){
    print(rname)
    if(basename(dirname(rname)) != "Associated"){
      
      r = raster(rname)
      clipAlign <- alignExtent(clipArea, r, snap='near') # should adjust the polygon-derived extent to edges of raster cells...??
      
      
      outDir <- paste0(covariates.outrootDirPath,'/', basename(dirname(rname)))
      if(!(file.exists(outDir))){
        dir.create(outDir, recursive = T)
      }
      
      outfile <- paste0(outDir, '/', basename(rname) )
      cr <- crop(r, clipAlign)
      rr <- resample(cr,clipTemplate,  method='bilinear')
      mr <- mask(rr, clipTemplate, overwrite=T, filename=outfile)
      theStack<- addLayer(theStack,r)
    }
  }
}
nlayers(theStack)


r <- raster('C:/Projects/Myanmar/SLGA/Clay/CLY_000_005_EV_N_P_AU_NAT_C_20140801.tif')




paths <- list.files( paste0('C:/Projects/Myanmar/SLGA'), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =T)
p2 <- paths[grepl('_000_005_EV_', paths, ignore.case = T) | grepl('_015_030_EV_', paths, ignore.case = T) | grepl('_060_100_EV_', paths, ignore.case = T)]
SLGARs <- p2[grepl('AWC_', p2, ignore.case = T) | grepl('BDW_', p2, ignore.case = T) |grepl('CLY_', p2, ignore.case = T)
             |grepl('ECE_', p2, ignore.case = T)
             | grepl('pHc_', p2, ignore.case = T) |  grepl('SND_', p2, ignore.case = T) | grepl('SOC_', p2, ignore.case = T)
             | grepl('PTO_', p2, ignore.case = T)]





#paths <- list.files( paste0('C:/Projects/AgDataShop/TimsPlace/SLGA'), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)
stk <- stack(paths)
samps <- sampleRandom(stk,200, sp=T, na.rm=T, xy=T, cells=T)
head(samps)
plot(stk[[1]])
points(samps)
pdks <- readOGR('C:/Projects/AgDataShop/DataDumps/Raw/McClelland', 'PropertyBoundaries')
lines(pdks)
head(samps)

write.csv(samps, 'C:/Projects/Myanmar/Samples/samples.csv')

newDF <- data.frame(sid=character(), x=numeric(), y=numeric(), UpperDepth=numeric(), LowerDepth=numeric(), Attribute=character(), Val=numeric())

indata <- samps@data
head(indata)
for(i in 4:length(colnames(indata))) {
  
cn <- colnames(indata)[i]
bits <- str_split(cn, '_')
att <- bits[[1]][1]
upperD <- bits[[1]][2]
lowerD <- bits[[1]][3]
x <- indata$x
y <- indata$y
sid <- indata$cell
vals <-indata[,i]

recDF <- data.frame(sid=sid,x=x, y=y, UpperDepth=upperD, LowerDepth=lowerD, Attribute=att, Val=vals)
newDF <- rbind(newDF, recDF)
}

head(newDF)
stddf <- newDF[with(newDF, order(sid, Attribute, UpperDepth)),]
head(stddf)
write.csv(stddf, 'C:/Projects/Myanmar/Samples/samples2.csv')

df <- read.csv('C:/Projects/Myanmar/Samples/samples2.csv')
head(df)

install.packages("ithir", repos="http://R-Forge.R-project.org")
library(ithir)
library(aqp)
library(plyr)
library(sp)


#Fit spline 
data(oneProfile)
class(oneProfile)

head(newDF)
att='BDW'
spdf <- stddf[stddf$Attribute==att, c(1, 4,5,7)]
colnames()
head(spdf)
oneProfile




sp.fit <- ea_spline(spdf, var.name="Val")

atts <- c("BDW", "CLY", "AWC")

for (i in 1: length(atts)) {
  att <- atts[i]
  spdf <- stddf[stddf$Attribute==att, c(1,4,5,7)]
  colnames(spdf)[4] <- att
  head(spdf)
  sp.fit <- ea_spline(spdf, var.name=att)
  saveRDS(sp.fit, paste0('c:/temp/atts/', att, '_spline.rds'))
  
}

minx <-
  
  

spls <- readRDS('C:/Projects/Myanmar/ShinyDeploy/Demo/Development/Samples/samples2/CLY_spline.rds')
stdDeps <- c('2.5', '10', '22.5', '45', '80', '150')

sid <- which(spls$harmonised$id == 33964)
pstd <- spls$harmonised[sid,]

d = spls$var.1cm[,sid]
minx <- min(d) - min(d) * 0.2
maxx <- max(d) + max(d) * 0.2
plot( d, 1:length(d), type="n", main=paste( paste0('Spline of ')), ylab = "Depth", yaxs = "i", xaxs = "i", xlim = c(minx, maxx), ylim = rev(range(c(0,200))))
lines( d, 1:length(d), type="l") 
stdVals <- c(spls$harmonised$`0-5 cm`[sid], spls$harmonised$`5-15 cm`[sid], spls$harmonised$`15-30 cm`[sid], spls$harmonised$`30-60 cm`[sid], spls$harmonised$`60-100 cm`[sid], spls$harmonised$`100-200 cm` [sid])
stdPts <- data.frame(stdDeps, stdVals)
points(stdVals, stdDeps, col='red', pch=19)












source('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/generalFunctions/ModelUtils.R')
library(ranger)

spls <- readRDS('C:/Projects/Myanmar/ShinyDeploy/Demo/Development/Samples/samples2/CLY_spline.rds')
str(spls)
head(spls$harmonised)

svals <- spls$harmonised['0-5 cm']
ids <- spls$harmonised['id']
svalsDF <- data.frame(sid=ids, vals=svals)
colnames(svalsDF) <- c('sid', 'vals')

#inDF <- RV$SampleFileData 
#att <- input$SampleAtt
inDF <- df
att <- 'CLY'
locs <- inDF[inDF$Attribute == att &inDF$UpperDepth == 0, ]
locs2 <- locs[, c(1,2,3)]
sdf <- merge(svalsDF, locs2, by.x = 'sid' , by.y = 'sid')

coordinates(sdf) <- ~x+y

covpaths <- list.files( 'C:/Projects/Myanmar/ShinyDeploy/Demo/Development/Covariates', pattern = paste0( '.tif$'), full.names = T, recursive =F)

stk <- stack(covpaths)
covPts <- extract(stk, sdf, df=T)
modDF <- na.omit(data.frame(sdf, covPts[,-1] ))
modDF2 <- modDF[, c(-1, -3,-4,-5)]

colnames(modDF2)[1] <- att
head(modDF2)


splitSamples <-createTrainingSample(modDF2, 1, 66)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(modDF2)
colnames(validSet) <- colnames(modDF2)


write.csv(trainSet, 'c:/temp/rf.csv')

Rmodel <- ranger(trainSet[,1] ~ ., data = trainSet[,-1], write.forest = TRUE, importance = 'impurity', num.trees = 500)

mvals <- predict(Rmodel, validSet[,-1])
mfit <- data.frame(validSet[,1], mvals$predictions)
colnames(mfit) <- c('obs', 'mod')

modelDir <- 'C:/temp/amodel'
createDirectory(modelDir)
RmodelFilename <- paste0(modelDir, '/', att)
fitStats(mfit$obs ,mfit$mod , paste0( att,' using Ranger'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)

MapRF(att=att, model = Rmodel, templateR = templateR, cpus = 7, theStack = stk, rootDir = modelDir, minBlocks = 14)




require(ranger)

## Classification forest with default settings
ranger(Species ~ ., data = iris)
