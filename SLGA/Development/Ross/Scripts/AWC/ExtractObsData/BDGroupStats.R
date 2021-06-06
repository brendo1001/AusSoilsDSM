library(stringr)
library(plyr)
library(ggplot2)
library(dplyr)
library(raster)
library(rasterVis)
library(sf)
library(reshape2)


source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/AusSoilsDSM/SLGA/Development/Ross/Scripts/AWC/joinSamplesToHorizons.R')

rasterOptions(progress = 'text')

workDir2 <- 'C:/Projects/TernLandscapes/AWC/DataMassage'
workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'

###  Join the soil Horizon table and the SOC table

# inHorizons <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_Horizons_H_DESIG_MASTER.csv', stringsAsFactors = F)
# inSamples <- read.csv(paste0(workDir, '/Clean_All_SOC.csv'), stringsAsFactors = F)[,-1]


inSamples <- read.csv( paste0(workDir, '/SDF_All_BulkDensity.csv'), stringsAsFactors = F)[,-c(1)]
head(inSamples)
inHorizons <- read.csv( 'c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/MergedHorGroups.csv', stringsAsFactors = F )
head(inHorizons)

#jn <- joinSamplesToHorizonsSerial(inHorizons=rawHor, inSamples=SOCclean)
jdf <- joinSamplesToHorizonsPara(inHorizons, inSamples, tempDir = 'c:/temp')
colnames(jdf)[17] <- 'BlkDen'
str(jdf)
nrow(jdf)
head(jdf)

str(jdf)
jdf$Longitude <- as.numeric(jdf$Longitude)
jdf$Latitude <- as.numeric(jdf$Latitude)
jdf$UpperDepth <- as.numeric(jdf$UpperDepth)
jdf$LowerDepth <- as.numeric(jdf$LowerDepth)

jdf$midpoint <- as.numeric(jdf$midpoint)
jdf$sampUpperDepth<- as.numeric(jdf$sampUpperDepth)
jdf$sampLowerDepth<- as.numeric(jdf$sampLowerDepth)

jdf$BlkDen <- as.numeric(jdf$BlkDen)
str(jdf)

write.csv(jdf, 'C:/Projects/TernLandscapes/AWC/DataMassage/All_BD_Joined_to_Horizons.csv', row.names=F ) 

#####  BD data hasn't been cleaned up to this point so here we go

summary(jdf)
hist(as.numeric(jdf$BlkDen))

idxs <- which(jdf$BlkDen > 2)
jdf[idxs,]
jdf <- jdf[-idxs,]
nrow(jdf)

locs <- unique(jdf[c("Longitude", "Latitude", "Dataset", "Location_ID")])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
head(pts)
length(unique(pts$Dataset))
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)

idxs <- which(jdf$UpperDepth>3)
jdf <- jdf[-idxs,]

idxs <- which(jdf$LowerDepth>10)
jdf <- jdf[-idxs,]
jdf[idxs,]

summary(jdf)


write.csv(jdf, 'C:/Projects/TernLandscapes/AWC/DataMassage/All_BD_Joined_to_Horizons_Clean.csv', row.names=F ) 











write.csv(jdf2,'C:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Joined_to_Horizons.csv', row.names=F )

jdf <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Joined_to_Horizons.csv', stringsAsFactors = F )
head(jdf)
nrow(jdf)

unique(jdf$Dataset)




#### Join the soil Classification and BD Tables

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

write.csv(odf3, 'c:/Projects/TernLandscapes/AWC/DataMassage/All_SOC_Joined_to_Horizons_and_SoilClassifications.csv', row.names=F)





######  Have a look at the data


summary(odf3)
nrow(odf3)
unique(socgrps$Dataset)
count(odf3, 'SoilSuperGrp')
count(odf3, 'Dataset')
str(odf3)

grpStats <- ddply(odf3, .(SoilSuperGrp), summarize, BD=mean(BlkDen, na.rm=T), SD=sd(BlkDen), freq=length(BlkDen))
grpStats2 <- ddply(odf3[!is.na(odf3$HorGrp) & !is.na(odf3$SoilSuperGrp),], .(SoilSuperGrp, HorGrp), summarize, BD=mean(BlkDen, na.rm=T), SD=sd(BlkDen))




ggplot(grpStats) +
  geom_bar( aes(x=SoilSuperGrp, y=BD), stat="identity", fill="skyblue", alpha=0.5) +
  geom_errorbar( aes(x=SoilSuperGrp, y=BD, ymin=BD-SD, ymax=BD+SD), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Bulk Density per SoilType") 

ggplot(odf3, aes(x=c(Dataset), y=BlkDen, fill=Dataset)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Bulk Density per Dataset") 

ggplot(odf3, aes(x=c(SoilSuperGrp), y=BlkDen, fill=SoilSuperGrp)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Bulk Density per SoilType")


d <- dcast(grpStats2, SoilSuperGrp ~ HorGrp, fun.aggregate = mean)
rownames(d) <- d[,1]
d <- as.matrix(d[,-1])
idxs <- which(is.nan(d))
d[idxs] <- 0 
ze_barplot <- barplot(d[-1,] , beside=T , main='BD by Horizon', legend.text=T,col=c("blue" , "skyblue", 'green', 'red', 'yellow', 'pink', 'grey', 'orange', 'purple', 'beige') , ylim=c(0,lim) , ylab="Bulk Density", xlab='Horizon')
error.bar(ze_barplot,bilan, stdev)



locs <- unique(odf3[c("Longitude", "Latitude", "Dataset", "Location_ID")])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)

str(odf3)

sgrps<- unique(odf3$SoilSuperGrp)
sgrps <- as.character(na.omit(sgrps))
hgrps <- as.character(na.omit(unique(odf3$HorGrp)))

for (i in 1:length(sgrps)) {
  for (j in 1:length(hgrps)) {
    idxs <- which(odf3$SoilSuperGrp == sgrps[i] & odf3$HorGrp==hgrps[j])
    df <- odf3[idxs, ]
    if(nrow(df)>3){
      d<-density(df$BlkDen )
      plot(d, main = paste0(sgrps[i], " - ", hgrps[j], ' Horizons'))
      polygon(d, col="red", border="red")
    }
  }
}


# #### Sample the landuse map
# luseR <- raster('C:/Projects/TernLandscapes/AWC/SuppData/Landuse/lu10v5ug/hdr.adf', RAT = TRUE)
# write.csv(levels(luseR), 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_vat.csv', row.names = F)
# head(levels(luseR)[[1]])
# unique(levels(luseR)[[1]]$CLASSES_18)
# unique(levels(luseR)[[1]]$C18_DESCRIPTION)
# 
# rt <- deratify(luseR, att='C18_DESCRIPTION')
# levels(rt)
# plot(rt)
# write.csv(levels(rt), 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_vat_c18.csv', row.names = F)
# writeRaster(rt, 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_vat_c18.tif')
# remap <- read.csv('C:/Projects/TernLandscapes/AWC/SuppData/Landuse/remap.csv', stringsAsFactors = F)
# remap
# m <- remap[, c(1,5)]
# m
# rc <- reclassify(rt, m)
# levelplot(rc)
# writeRaster(rc, 'C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_grps.tif', overwrite=T)
# 
# rc<-raster('C:/Projects/TernLandscapes/AWC/SuppData/Landuse/luse_grps.tif')
# 

# idxs<-which(is.na(socgrps$Longitude))
# socgrps <- socgrps[-idxs,]
# pts <- socgrps
# coordinates(pts) <- ~Longitude+Latitude
# luse <- extract(rc, pts)
# 
# socgrps2 <- cbind(socgrps, luse)
# head(socgrps2)
# 
# socGrpVals <- ddply(socgrps2[!is.na(socgrps2$HorGrp),], .(SoilSuperGrp, HorGrp, luse), summarize, SOC=mean(SoC, na.rm=T), freq=length(SoilSuperGrp))
# write.csv(socGrpVals, 'C:/Projects/TernLandscapes/AWC/DataMassage/SoCGrpVals.csv', row.names = F)




BDGrpVals <- na.omit(ddply(odf3[!is.na(odf3$HorGrp),], .(SoilSuperGrp, HorGrp), summarize, BD=mean(BlkDen, na.rm=T), freq=length(SoilSuperGrp)))
write.csv(BDGrpVals, 'C:/Projects/TernLandscapes/AWC/DataMassage/BDGrpVals.csv', row.names = F)



### OK we now have our BD group values let apply them to the PSA dataset we will use in the modelling

BDGrpVals <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/BDGrpVals.csv', stringsAsFactors = F)

bdf <-  read.csv(paste0(workDir, '/SDF_All_Props_Clean_Fill_3.csv'), stringsAsFactors = F) 
head(bdf)
nrow(bdf)
sum(!is.na(bdf$BlkDen))
sum(is.na(bdf$BlkDen))



# add in existing BD value
bdf$FinalBD <- NA
bdf$ReplaceLevelBD <- NA
idxs <- which(!is.na(bdf$BlkDen))
bdf$FinalBD[idxs] <- bdf$BlkDen[idxs]
bdf$ReplaceLevelBD[idxs] <- 1
bdf[-idxs,]
bdf[idxs,]
nrow(bdf)-sum(is.na(bdf$FinalBD))
head(bdf)


grps <- BDGrpVals[complete.cases(BDGrpVals),]
for (i in 1:nrow(grps)) {
  soil <- grps[i,]$SoilSuperGrp
  horizon <- grps[i,]$HorGrp
  bd <-grps[i,]$BD
  idxs <- which(bdf$SoilSuperGrp==soil & bdf$HorGrp==horizon & is.na(bdf$BlkDen))
  bdf[idxs,]
  #print(paste0(soil, ' ',horizon, ' ',landuse, ' ', length(idxs)))
  bdf$FinalBD[idxs] <- bd
  bdf$ReplaceLevelBD[idxs] <- 2
  bdf[idxs,]
}
nrow(bdf)-sum(is.na(bdf$FinalBD))
sum(!is.na(bdf$FinalBD))
sum(is.na(bdf$FinalBD))


write.csv(bdf, paste0(workDir, '/SDF_All_Props_Clean_Fill_4.csv'), row.names = F)



### Assign by averages of soil group, less than 30cm
BDGrpValsSoilTo30cm <- na.omit(ddply(bdf[bdf$sampLowerDepth < 0.3,], .(SoilSuperGrp), summarize, BD=mean(BlkDen, na.rm=T), freq=length(SoilSuperGrp)))
write.csv(BDGrpValsSoilTo30cm, 'C:/Projects/TernLandscapes/AWC/DataMassage/BDGrpValsSoilTo30cm.csv', row.names = F)

BDGrpValsSoilTo30cm <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/BDGrpValsSoilTo30cm.csv', stringsAsFactors = F)
grps <- BDGrpValsSoilTo30cm[complete.cases(BDGrpValsSoilTo30cm),]
grps <- BDGrpValsSoilTo30cm[BDGrpValsSoilTo30cm$SoilSuperGrp != "",]
for (i in 1:nrow(grps)) {
  soil <- grps[i,]$SoilSuperGrp
  bd <-grps[i,]$BD
  idxs <- which(bdf$SoilSuperGrp==soil & is.na(bdf$FinalBD) & bdf$sampLowerDepth < 0.3)
  bdf[idxs,]
  bdf$FinalBD[idxs] <- bd
  bdf$ReplaceLevelBD[idxs] <- 3
  bdf[idxs,]
}
nrow(bdf)-sum(is.na(bdf$FinalBD))
sum(!is.na(bdf$FinalBD))
sum(is.na(bdf$FinalBD))


### Assign by averages of soil group, greater than or equal to 30cm
BDGrpValsSoil30cmToBottom <- na.omit(ddply(bdf[bdf$sampLowerDepth >= 0.3,], .(SoilSuperGrp), summarize, BD=mean(BlkDen, na.rm=T), freq=length(SoilSuperGrp)))
write.csv(BDGrpValsSoil30cmToBottom, 'C:/Projects/TernLandscapes/AWC/DataMassage/BDGrpValsSoil30cmToBottom.csv', row.names = F)

BDGrpValsSoil30cmToBottom <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/BDGrpValsSoil30cmToBottom.csv', stringsAsFactors = F)
grps <- BDGrpValsSoil30cmToBottom[complete.cases(BDGrpValsSoil30cmToBottom),]
#grps <- BDGrpValsSoil30cmToBottom[BDGrpValsSoil30cmToBottom$SoilSuperGrp != "",]
for (i in 1:nrow(grps)) {
  soil <- grps[i,]$SoilSuperGrp
  bd <-grps[i,]$BD
  idxs <- which(bdf$SoilSuperGrp==soil & is.na(bdf$FinalBD) & bdf$sampLowerDepth >= 0.3)
  bdf[idxs,]
  bdf$FinalBD[idxs] <- bd
  bdf$ReplaceLevelBD[idxs] <- 3
  bdf[idxs,]
}
nrow(bdf)-sum(is.na(bdf$FinalBD))
sum(!is.na(bdf$FinalBD))
sum(is.na(bdf$FinalBD))

write.csv(bdf, paste0(workDir, '/SDF_All_Props_Clean_Fill_5.csv'), row.names = F)


lt30bd <- bdf[bdf$sampLowerDepth < 0.3 & !is.na(bdf$BlkDen),]$BlkDen
lt30DBmean <- mean(lt30bd)
lt30DBmean

idxs <- which(is.na(bdf$FinalBD) & bdf$sampLowerDepth < 0.3)
bdf$FinalBD[idxs] <- lt30DBmean
bdf$ReplaceLevelBD[idxs] <- 4

gt30bd <- bdf[bdf$sampLowerDepth >= 0.3 & !is.na(bdf$BlkDen),]$BlkDen
gt30DBmean <- mean(gt30bd)
gt30DBmean

idxs <- which(is.na(bdf$FinalBD) & bdf$sampLowerDepth >= 0.3)
bdf$FinalBD[idxs] <- gt30DBmean
bdf$ReplaceLevelBD[idxs] <- 4


idxs <- which(is.na(bdf$FinalBD))
idxs <- which(is.na(bdf$FinalSOC))
idxs
bdf[idxs,]
bdf <- bdf[-idxs,]
nrow(bdf)

write.csv(bdf, paste0(workDir, '/SDF_All_Props_Clean_Fill_6.csv'), row.names = F)


