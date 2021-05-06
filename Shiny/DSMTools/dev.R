library(raster)



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



a <- c('Ross', 'Searle', 'Ben', 'boy')
m <- c('Ross', 'Be', 'se')

match(a, m)

a %in% m

unlist(sapply(m, grep, a, USE.NAMES = F, ignore.case = T ))


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
write.csv(validSet, 'c:/temp/rf_valid.csv')


Rmodel <- ranger(trainSet[,1] ~ ., data = trainSet[,-1], write.forest = TRUE, importance = 'impurity', num.trees = 500)

mvals <- predict(Rmodel, validSet[,-1])
mfit <- data.frame(validSet[,1], mvals$predictions)
colnames(mfit) <- c('obs', 'mod')

modelDir <- 'C:/temp/amodel'
createDirectory(modelDir)
RmodelFilename <- paste0(modelDir, '/', att)
fitStats(mfit$obs ,mfit$mod , paste0( att,' using Ranger'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)

MapRF(att=att, model = Rmodel, templateR = templateR, cpus = 7, theStack = stk, rootDir = modelDir, minBlocks = 14)





library(Cubist)
trainSet <- read.csv('c:/temp/rf.csv')
validSet <- read.csv('c:/temp/rf_valid.csv')
Cmodel <- cubist(x = trainSet[,-1], y = trainSet[,1], committees=1,  cubistControl( label = 'clay', rules = 5))
summary(Cmodel)
Cmv <- predict(Cmodel, validSet[,-1])
Ctdf <- data.frame(validSet[,1], Cmv)

fitStats(Ctdf[1],Ctdf[2], paste0('Surface Clay - Cubist'),  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)

outRaster <- 'C:/temp/clay.tif'
templateR <- raster('C:/Projects/Myanmar/ShinyDeploy/Demo/Development/Covariates/rad_k.tif')



modelR <- makeMapParra(model=Cmodel, templateR, stk, outRaster, numCPUs=NULL, tidyUp=T)




library(doParallel)
library(doSNOW)

makeMapParra <- function(model, templateR, stk, outRasterName, numCPUs=NULL, minBlocks=NULL, tidyUp=T){
  
  ptm <- proc.time()
  
  outdir <- dirname(outRasterName)
  rName <- basename(outRasterName)
  withoutext <- str_split(rName, '\\.')[[1]][1]
  
  scratchDir <- paste0(outdir, '/scratch_', withoutext)
  if(!file.exists(scratchDir)){dir.create(scratchDir)}
  unlink(paste0(scratchDir, "/*"))
  
  covNamesinModel<- getCovariatesUsedInModel(model)
  
  if(is.null(numCPUs)){
    numCPUs = detectCores()-1
  }
  
  cat(paste0('Using ', numCPUs, ' cores'), sep='\n')
  
  if(is.null(minBlocks)){
    mblocks = numCPUs
  }else{
    mblocks = minBlocks
  }
  
  bs <- blockSize(stk, minblocks = mblocks)
  saveRDS(bs, paste0(scratchDir, '/brickInfo.rds'))
  cat(paste0('Using ', bs$n, ' blocks'), sep='\n')
  cl <- makeSOCKcluster(numCPUs)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max=bs$n, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  r <- foreach(k=1:bs$n, .options.snow=opts, .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallel2')) %dopar% applyMapParallel2(model, templateR, stk, scratchDir, bs, covNamesinModel)
  close(pb)
  stopCluster(cl)
  
  
  # cl<-makeCluster(detectCores(),outfile="")
  # registerDoParallel(cl)
  # foreach(k=1:bs$n,  .packages=c('raster','rgdal', 'Cubist'), .export = c('applyMapParallel2')) %dopar% applyMapParallel2(model, templateR, stk, scratchDir, bs, covNamesinModel)
  
  
  oRast <- writeRasterFromFilesSimple(templateR=templateR, rasterDir=scratchDir, outRaster=outRasterName)
  
  
  if(tidyUp){
    unlink(scratchDir, recursive = T)
  }
  print(proc.time() - ptm)
  
  plot(oRast, maxpixels = 100000)
  return(oRast)
  
  
}






###############################   covariate preperation    #########


templateR <- raster('C:/Temp/clusters.tif')


inR <- raster('C:/Temp/climtest.tif')

templateProj <- crs(templateR)

#if(!compareRaster(templateR, inR, stopiffalse=F)){

    if(!compareCRS(templateR, inR)){
      inR2 <- projectRaster(inR, crs=templateProj,method="ngb")
    }else{
      inR2 <- inR
    }

if(!compareRaster(templateR, inR, stopiffalse = F, exten = T, rowcol = T, res = T, orig = T)){
  
  inR3 <- resample(inR2, templateR)
}else{
  inR3 <- inR2
}


inR4 <- mask(inR3, templateR)
plot(inR4)


r <- templateR
outR <- r^as.numeric(2)
outR <- exp(r)
outR <- log(r, 30)
outR <- cos(r)

filename(r)

is.numeric(as.numeric(NULL))


a <- as.numeric(' ')

if(is.na(a)){
  diag
}else{
  
}
is.null(NULL)



setwd('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools')
getwd()
rootDir <- 'Data'
currentUser <- 'Demo'

state <- read.csv(paste0(rootDir, '/', currentUser, '/state.cfg'), sep = "=", header = F )
previousProj <- as.character(state[1,2])






mydf <- read.csv('C:/Projects/Myanmar/MarkData/20181018-PYB_data.csv')  
mydf[mydf == -9999] <- NA
mydf2 <- Filter(is.numeric, mydf)

do.call(cbind, lapply(mydf2, summary))

dfs <- data.frame(Stat=names(s), Value=as.numeric(s[1:7]), stringsAsFactors = F)
vad <- length(which(!is.na(mydf2$pH_H2O)))
dfs[8,] <- c('# Vals', vad)

s <- summary(mydf2$pH_H2O)

sl <- lapply(mydf$pH_H2O, summary)

Filter(is.numeric, mydf)



r <- raster('C:/Projects/Myanmar/MarkData/Convergence_060.tif')
r

df <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/dev1/Samples/20181018-PYB_data.csv')
str(df)
sdf <- unique(df[c("SiteID", "Easting", "Easting")])
head(sdf)
coordinates(sdf) <- ~Easting+Northing
crs(sdf) <- crs('+proj=utm +zone=46 +ellps=WGS84 +units=m +no_defs ')
psdf <- spTransform(sdf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


plot(psdf)

f <-'RockFrag'
df[f]







germG <- spTransform(mapG, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))




if (interactive()) {
  
  demoAirDatepicker("datepicker")
  
}


model <- readRDS('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/Outputs/20181018-PYB_data_SiteID_5/model_20181018-PYB_data_SiteID_5.rds')

ms <- summary(model)

cat(ms$output, file = 'c:/temp/am.txt')

x <- model$model
cat(x, 'c:/temp/amodel.txt')

read.table(text=x,col.names=c('ID','Name'))
model$output
model$control
model$caseWeights
model$maxd
model$dims
model$splits
str(model$usage)
model$vars



inTable <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/Demo/Myanmar/Outputs/20181018-PYB_data!SiteID!5/Model!20181018-PYB_data!SiteID!5!ExternalValidation.csv')

round(mean(inTable$R2), 2)




shell("explorer C:\\Users", intern=TRUE) 



library(leaflet)
r <- raster('C:/Projects/Myanmar/MarkData/Terrain_MidSlopePosition.tif')
ra <- aggregate(r, 5, 'mean')
m <- leaflet()
m <- addTiles(m)
m <- addRasterImage(m, ra)
m


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  
m  # Print the map

source('C:/Projects/Myanmar/RInno/DSMTools/ShinyApp/mylibs/ithirSub.R')
gof <- goof(observed = c(1,2, 3, 4) , predicted = c(1.2, 1.9, 3.3, 3.7) )




paths <- list.files( paste0('C:/Projects/Myanmar/MarkData'), pattern = paste0('[a-zA-Z0-9\\/_:-]*', '.tif$'), full.names = T, recursive =F)

covDir <- paste0( 'C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/LocalPC/r4/Covariates' )
covpaths <- list.files( covDir, pattern = paste0( '_RedRes.tif$'), full.names = T, recursive =F)
stk <- stack(paths)
brk <- brick(stk)
plot(brk)
raster(covpaths[[6]])




covDir <- paste0( 'C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/LocalPC/p1/Covariates' )
covpaths <- list.files( covDir, pattern = paste0( '_RedRes.tif$'), full.names = T, recursive =F)
stk <- stack(covpaths)

nc <- 3
psize = 10
marg = 0.1

par(mai=c(marg,marg,marg,marg))

stk <- getCovariateStack(input$currentProject, UseReducedResRasters)

nlayers(stk)
names(stk) <- str_remove(names(stk), '_RedRes')
nr <- ceiling(nlayers(stk)/nc)


outfile <- 'c:/temp/a.png'
#outfile <- paste0( rootDir, '/', currentUser, '/', input$currentProject, '/tmpData/AllCovsPlot.png')

png(filename = outfile,   width = psize * nc, height = psize * nr, units = "cm", res=150)
mat <- matrix(seq(1,(nc*nr)), nr, nc, byrow = TRUE)
nf <- layout(mat, widths = rep.int(1, ncol(mat)), heights = rep.int(1, nrow(mat)))

for (i in 1:nlayers(stk)) {
  plot(stk[[i]],  main=paste0(names(stk)[i]), cex.main=2)
}

dev.off()  
  
 

  plot(stk[[1]], legend=FALSE, axes=FALSE, box=FALSE)
  
  
  
r <- raster('C:/Users/sea084/Dropbox/RossRCode/Git/AusSoilsDSM/Shiny/DSMTools/Data/LocalPC/p4/GeoTemplate/Template.tif')
nr <- nrow(r)  
nc <- ncol(r)  

ds <- 30
z <- nc/ds

marg<-0.01
par(mai=c(marg,marg,marg,marg))
par(oma=c(marg,marg,marg,marg))
imagefile <- paste0('c:/temp/aTemplate.png' )
png(filename = imagefile,   width = nc/z, height = nr/z, units = "cm", res=150)
plot(r, legend=FALSE, axes=FALSE, box=FALSE)
dev.off()  
  
  
showMethods("plot")
getMethod("plot", c("Raster", "ANY"))
  
df <- read.csv('C:/Projects/Myanmar/Samples/samples.csv')
head(df)
df['x']

v <- summary(df['x'])
str(v)  
str(v[,1])
as.data.frame(v[,1])



