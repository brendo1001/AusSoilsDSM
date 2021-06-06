library(stringr)
library(plyr)
library(ggplot2)
library(dplyr)
library(raster)
library(rasterVis)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/AusSoilsDSM/SLGA/Development/Ross/Scripts/AWC/joinSamplesToHorizons.R')

rasterOptions(progress = 'text')

workDir2 <- 'C:/Projects/TernLandscapes/AWC/DataMassage'
workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'

###  Join the soil Horizon table and the SOC table

# inHorizons <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_Horizons_H_DESIG_MASTER.csv', stringsAsFactors = F)
# inSamples <- read.csv(paste0(workDir, '/Clean_All_SOC.csv'), stringsAsFactors = F)[,-1]


inSamples <- read.csv( paste0(workDir, '/Clean_All_SOC.csv'), stringsAsFactors = F)[,-c(1)]
head(inSamples)
inHorizons <- read.csv( 'c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/MergedHorGroups.csv', stringsAsFactors = F )
head(inHorizons)

#jn <- joinSamplesToHorizonsSerial(inHorizons=rawHor, inSamples=SOCclean)
jdf <- joinSamplesToHorizonsPara(inHorizons, inSamples, tempDir = 'c:/temp')
nrow(jdf)
head(jdf)

idxs <- which(jdf$SoC > 100)
jdf2 <- jdf[-idxs,]
nrow(jdf2)

write.csv(jdf2,'C:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Joined_to_Horizons.csv', row.names=F )

jdf <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Joined_to_Horizons.csv', stringsAsFactors = F )
head(jdf)
nrow(jdf)

unique(jdf$Dataset)




#### Join the soil Classification and SOC Tables

soilgrps <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/MergedRawWithSoilSuperGroups.csv', stringsAsFactors = F )
head(soilgrps)
nrow(soilgrps)

nrow(jdf)
mdf2 <- merge(jdf, soilgrps, all.x=T, all.y=F, by=c('DataStore','Dataset','Provider','Location_ID','Longitude','Latitude'))
nrow(mdf2)
head(mdf2)
colnames(mdf2)

odf3 <- mdf2[, c(1:17, 19:26)]
head(odf3)
colnames(odf3)[9] <- 'SampleDate'
head(odf3)

write.csv(odf3, 'c:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Atts.csv', row.names=F)

odf3 <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Atts.csv', stringsAsFactors = F)
nrow(odf3)
# Couple of little fixes

idxs <- which(odf3$SoilSuperGrp=='Shallow stony soils')
odf3$SoilSuperGrp[idxs] <- 'Shallow Stony'

idxs <- which(odf3$UpperDepth>10)
nrow(odf3)
odf3 <- odf3[-idxs,]

idxs <- which(is.na(odf3$SoilSuperGrp))
idxs <- which(odf3$SoilSuperGrp=='')
odf3$SoilSuperGrp[idxs] <- NA

idxs <- which(odf3$LowerDepth>10)
odf3[idxs,]
odf3 <- odf3[-idxs,]

idxs <- which(odf3$sampUpperDepth>10)
odf3[idxs,]
odf3 <- odf3[-idxs,]
sum(is.na(odf3$SoC))

write.csv(odf3, 'c:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Atts.csv', row.names=F)


######  Have a look at the data

socgrps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Atts.csv', stringsAsFactors = F)
head(socgrps)

summary(socgrps)
nrow(socgrps)
unique(socgrps$Dataset)
count(socgrps, 'SoilSuperGrp')
count(socgrps, 'Dataset')

ddply(socgrps, .(SoilSuperGrp), summarize, SOC=mean(SoC, na.rm=T))
ddply(socgrps[!is.na(socgrps$HorGrp),], .(SoilSuperGrp, HorGrp), summarize, SOC=mean(SoC, na.rm=T), freq=length(SoilSuperGrp))


fil <- socgrps[socgrps$SoC < 20,]
ggplot(fil, aes(x=SoilSuperGrp, y=SoC, fill=SoilSuperGrp)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("SOC Per Soil Type") 


locs <- unique(socgrps[c("Longitude", "Latitude", "Dataset", "Location_ID")])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)

sgrps<- unique(socgrps$SoilSuperGrp)
sgrps <- as.character(na.omit(sgrps))
hgrps <- as.character(na.omit(unique(socgrps$HorGrp)))

for (i in 1:length(sgrps)) {
  for (j in 1:length(hgrps)) {
    idxs <- which(socgrps$SoilSuperGrp == sgrps[i] & socgrps$HorGrp==hgrps[j])
    df <- socgrps[idxs, ]
    if(nrow(df)>3){
      d<-density(df$SoC[df$SoC<10] )
      plot(d, main = paste0(sgrps[i], " - ", hgrps[j], ' Horizons'))
      polygon(d, col="red", border="red")
    }
  }
}


#### Sample the landuse map
luseR <- raster('C:/Projects/TernLandscapes/AWC/SuppData/Landuse/lu10v5ug/hdr.adf', RAT = TRUE)
write.csv(levels(luseR), 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_vat.csv', row.names = F)
head(levels(luseR)[[1]])
unique(levels(luseR)[[1]]$CLASSES_18)
unique(levels(luseR)[[1]]$C18_DESCRIPTION)

rt <- deratify(luseR, att='C18_DESCRIPTION')
levels(rt)
plot(rt)
write.csv(levels(rt), 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_vat_c18.csv', row.names = F)
writeRaster(rt, 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_vat_c18.tif')
remap <- read.csv('C:/Projects/TernLandscapes/AWC/SuppData/Landuse/remap.csv', stringsAsFactors = F)
remap
m <- remap[, c(1,5)]
m
rc <- reclassify(rt, m)
levelplot(rc)
writeRaster(rc, 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_grps.tif', overwrite=T)

rc<-raster('C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_grps.tif')


idxs<-which(is.na(socgrps$Longitude))
socgrps <- socgrps[-idxs,]
pts <- socgrps
coordinates(pts) <- ~Longitude+Latitude
luse <- extract(rc, pts)

socgrps2 <- cbind(socgrps, luse)
head(socgrps2)

socGrpVals <- ddply(socgrps2[!is.na(socgrps2$HorGrp),], .(SoilSuperGrp, HorGrp, luse), summarize, SOC=mean(SoC, na.rm=T), freq=length(SoilSuperGrp))
write.csv(socGrpVals, 'C:/Projects/TernLandscapes/AWC/DataMassage/SoCGrpVals.csv', row.names = F)


### OK we now have our soc group values let apply them to the PSA dataset we will use in the modelling

socGrpVals <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoCGrpVals.csv', stringsAsFactors = F)
tdf <- read.csv(paste0(workDir, '/SDF_All_Props_Clean.csv'), stringsAsFactors = F)
pts <- tdf
head(pts)
which(is.na(pts$Longitude))
coordinates(pts) <- ~Longitude+Latitude
luse <- extract(rc, pts)

tdf2 <- cbind(tdf, luse)
head(socgrps2)

tdf3 <- tdf2

# add in existing soc value
tdf3$FinalSOC <- NA
tdf3$ReplaceLevelSOC <- NA
idxs <- which(!is.na(tdf3$SOC))
tdf3$FinalSOC[idxs] <- tdf3$SOC[idxs]
tdf3$ReplaceLevelSOC[idxs] <- 1
tdf3[-idxs,]
tdf3[idxs,]
nrow(tdf3)-sum(is.na(tdf3$FinalSOC))


grps <- socGrpVals[complete.cases(socGrpVals),]
for (i in 1:nrow(grps)) {
  soil <- grps[i,]$SoilSuperGrp
  horizon <- grps[i,]$HorGrp
  landuse <- grps[i,]$luse
  soc <-grps[i,]$SOC
  idxs <- which(tdf3$SoilSuperGrp==soil & tdf3$HorGrp==horizon & tdf3$luse ==landuse & is.na(tdf3$SOC))
  tdf3[idxs,]
  #print(paste0(soil, ' ',horizon, ' ',landuse, ' ', length(idxs)))
  tdf3$FinalSOC[idxs] <- soc
  tdf3$ReplaceLevelSOC[idxs] <- 2
  tdf3[idxs,]
}
nrow(tdf3)-sum(is.na(tdf3$FinalSOC))
sum(!is.na(tdf3$FinalSOC))
sum(is.na(tdf3$FinalSOC))
sum(is.na(tdf3$luse ))

write.csv(tdf3, paste0(workDir, '/SDF_All_Props_Clean_Fill_1.csv'), row.names = F)

#### Checking out approaches for filling the rest of the blanks

# sum(!is.na(tdf3$SoilSuperGrp) & is.na(tdf3$HorGrp) & is.na(tdf3$SOC))
# sum(is.na(tdf3$SoilSuperGrp) & !is.na(tdf3$HorGrp) & is.na(tdf3$SOC))
# 
# vv <- na.omit(data.frame(upper=tdf$UpperDepth, soc=tdf$SOC))
# vvv <- vv[vv$upper<2 & vv$soc<10,]
# 
# obsVal=vvv[1]
# modelVal=vvv[2]
# plot(vvv)
# 
# cccC <- epi.ccc(obsVal, modelVal, ci = "z-transform",conf.level = 0.95)
# r.sqC <- cor(obsVal, modelVal)^2


### Assign by averages of soil group, by land use less than 30cm
socGrpValsSoilAndLuseTo30cm <- ddply(socgrps2[socgrps2$sampLowerDepth < 0.3,], .(SoilSuperGrp, luse), summarize, SOC=mean(SoC, na.rm=T), freq=length(SoilSuperGrp))
write.csv(socGrpValsSoilAndLuseTo30cm, 'C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsSoilAndLuseTo30cm.csv', row.names = F)

sum(!is.na(tdf3$FinalSOC))
socGrpValsSoilAndLuseTo30cm <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsSoilAndLuseTo30cm.csv', stringsAsFactors = F)
grps <- socGrpValsSoilAndLuseTo30cm[complete.cases(socGrpValsSoilAndLuseTo30cm),]
for (i in 1:nrow(grps)) {
  soil <- grps[i,]$SoilSuperGrp
  landuse <- grps[i,]$luse
  soc <-grps[i,]$SOC
  idxs <- which(tdf3$SoilSuperGrp==soil & tdf3$luse ==landuse & is.na(tdf3$FinalSOC) & tdf3$sampLowerDepth < 0.3)
  tdf3[idxs,]
  #print(paste0(soil, ' ',horizon, ' ',landuse, ' ', length(idxs)))
  tdf3$FinalSOC[idxs] <- soc
  tdf3$ReplaceLevelSOC[idxs] <- 3
  tdf3[idxs,]
}
nrow(tdf3)-sum(is.na(tdf3$FinalSOC))
sum(!is.na(tdf3$FinalSOC))
sum(is.na(tdf3$FinalSOC))
sum(is.na(tdf3$luse ))

write.csv(tdf3, paste0(workDir, '/SDF_All_Props_Clean_Fill_2.csv'), row.names = F)

### Assign by averages of soil group, by land use greater than 30cm
socGrpValsSoilAndLuse30cmtoBottom <- ddply(socgrps2[socgrps2$sampLowerDepth >= 0.3,], .(SoilSuperGrp, luse), summarize, SOC=mean(SoC, na.rm=T), freq=length(SoilSuperGrp))
write.csv(socGrpValsSoilAndLuse30cmtoBottom, 'C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsSoilAndLuse30cmtoBottom.csv', row.names = F)

sum(!is.na(tdf3$FinalSOC))
socGrpValsSoilAndLuse30cmtoBottom <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsSoilAndLuse30cmtoBottom.csv', stringsAsFactors = F)
grps <- socGrpValsSoilAndLuse30cmtoBottom[complete.cases(socGrpValsSoilAndLuse30cmtoBottom),]
for (i in 1:nrow(grps)) {
  soil <- grps[i,]$SoilSuperGrp
  landuse <- grps[i,]$luse
  soc <-grps[i,]$SOC
  idxs <- which(tdf3$SoilSuperGrp==soil & tdf3$luse ==landuse & is.na(tdf3$FinalSOC) & tdf3$sampLowerDepth >= 0.3)
  tdf3[idxs,]
 # print(paste0(soil, ' ',horizon, ' ',landuse, ' ', length(idxs)))
  tdf3$FinalSOC[idxs] <- soc
  tdf3$ReplaceLevelSOC[idxs] <- 3
  tdf3[idxs,]
}
nrow(tdf3)-sum(is.na(tdf3$FinalSOC))
sum(!is.na(tdf3$FinalSOC))
sum(is.na(tdf3$FinalSOC))
sum(is.na(tdf3$luse ))

sum(is.na(tdf3$SoilSuperGrp))
sum(!is.na(tdf3$luse) & is.na(tdf3$FinalSOC))
sum(is.na(tdf3$SoilSuperGrp) & is.na(tdf3$FinalSOC))
idxs <- which(is.na(tdf3$SoilSuperGrp) & is.na(tdf3$FinalSOC))
t <- tdf3[idxs,]

write.csv(tdf3, paste0(workDir, '/SDF_All_Props_Clean_Fill_2.csv'), row.names = F)

tdf3 <- read.csv(paste0(workDir, '/SDF_All_Props_Clean_Fill_2.csv'), stringsAsFactors = F)




### Assign by averages of landuse less than 30cm
socGrpValsLuseTo30cm <- ddply(socgrps2[socgrps2$sampLowerDepth < 0.3,], .(luse), summarize, SOC=mean(SoC, na.rm=T), freq=length(luse))
write.csv(socGrpValsLuseTo30cm, 'C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsLuseTo30cm.csv', row.names = F)

sum(!is.na(tdf3$FinalSOC))
nrow(tdf3)
socGrpValsLuseTo30cm <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsLuseTo30cm.csv', stringsAsFactors = F)
grps <- socGrpValsLuseTo30cm[complete.cases(socGrpValsLuseTo30cm),]
for (i in 1:nrow(grps)) {
  landuse <- grps[i,]$luse
  soc <-grps[i,]$SOC
  idxs <- which(tdf3$luse ==landuse & is.na(tdf3$FinalSOC) & tdf3$sampLowerDepth < 0.3)
  tdf3[idxs,]
  #print(paste0(soil, ' ',horizon, ' ',landuse, ' ', length(idxs)))
  tdf3$FinalSOC[idxs] <- soc
  tdf3$ReplaceLevelSOC[idxs] <- 4
  tdf3[idxs,]
}
nrow(tdf3)-sum(is.na(tdf3$FinalSOC))

### Assign by averages of landuse less than 30cm
socGrpValsLuse30cmToBottom <- ddply(socgrps2[socgrps2$sampLowerDepth >= 0.3,], .(luse), summarize, SOC=mean(SoC, na.rm=T), freq=length(luse))
write.csv(socGrpValsLuse30cmToBottom, 'C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsLuse30cmToBottom.csv', row.names = F)

sum(!is.na(tdf3$FinalSOC))
nrow(tdf3)
socGrpValsLuse30cmToBottom <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/socGrpValsLuse30cmToBottom.csv', stringsAsFactors = F)
grps <- socGrpValsLuse30cmToBottom[complete.cases(socGrpValsLuse30cmToBottom),]
for (i in 1:nrow(grps)) {
  landuse <- grps[i,]$luse
  soc <-grps[i,]$SOC
  idxs <- which(tdf3$luse ==landuse & is.na(tdf3$FinalSOC) & tdf3$sampLowerDepth >= 0.3)
  tdf3[idxs,]
  #print(paste0(soil, ' ',horizon, ' ',landuse, ' ', length(idxs)))
  tdf3$FinalSOC[idxs] <- soc
  tdf3$ReplaceLevelSOC[idxs] <- 4
  tdf3[idxs,]
}

nrow(tdf3)-sum(is.na(tdf3$FinalSOC))

write.csv(tdf3, paste0(workDir, '/SDF_All_Props_Clean_Fill_3.csv'), row.names = F)
