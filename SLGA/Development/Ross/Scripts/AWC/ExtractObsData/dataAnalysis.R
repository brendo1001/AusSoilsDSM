library(sf)
library(ggplot2)
library(plyr)

workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'
workDir2 <- 'C:/Projects/TernLandscapes/AWC/DataMassage'


indf <- read.csv(paste0(workDir, '/SDF_All_Props_Clean.csv'), stringsAsFactors = F)
nrow(indf)
head(indf)


# Fix up ASCs

# unique(indf$ASC)
# ascgrps <- count(indf, 'ASC')
# write.csv(ascgrps, 'c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/rawASC.csv', row.names = F)
# 
# ascgrps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/ASCCodes.csv', stringsAsFactors = F)
# 
# nrow(indf)
# mdf1 <- merge(indf, ascgrps, all.x=T, all.y=F, by=c('ASC'))
# colnames(mdf1)
# odf1 <- mdf1[c(2:20, 1, 22)]
# head(odf1)
# colnames(odf1)[21] <- 'ASCCodeClean'
# unique(odf1$ASCCodeClean)
# nrow(odf1)
# write.csv(odf1, paste0(workDir2, '/obs_With_CleanASCCodes.csv'), row.names = F)
# 
# 




# pts <- st_as_sf(indf, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
# which(!is.na(pts$BlkDen))
# fpts <- pts[!is.na(odf1$BlkDen) & !is.na(odf1$ASCCodeClean),]
# nrow(fpts)
# plot(fpts['BlkDen'],  axes = TRUE, pch=3)
# count(fpts, 'ASC')
# s <- ddply(fpts, .(ASC), summarize,  BD=mean(BlkDen), SOC=mean(SOC, na.rm=T))
# s

# Fix up Horizon Designations

unique(odf1$HorName)
grps <- count(odf1, 'HorName')
write.csv(grps, 'c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/rawHors.csv', row.names = F)


hgrps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/HorGroups.csv', stringsAsFactors = F)
odf1 <- read.csv(paste0(workDir2, '/obs_With_CleanASCCodes.csv'), stringsAsFactors = F)


nrow(odf1)
mdf2 <- merge(odf1, hgrps, all.x=T, all.y=F, by=c('HorName'))
colnames(mdf2)
odf2 <- mdf2[c(2:21, 1, 23)]
head(odf2)
nrow(odf2)
write.csv(odf2, paste0(workDir2, '/obs_With_CleanASCCodes_and_HorGrp.csv'), row.names = F)


####  Make Soil Classifications super grouping
###  Do Manula groupings

odf3<- read.csv(paste0(workDir2, '/obs_With_CleanASCCodes_and_HorGrp.csv'), stringsAsFactors = F)
count(odf3, 'ASCCodeClean')

odf3$ASC <- str_remove(odf1$ASC, ' ')
count(odf3, 'PPF')
#write.csv(odf1, paste0(workDir2, '/obs_With_HorGrp.csv'), row.names = F)

count(odf3, 'PPF')
unique(odf3$PPF)
#write.csv(unique(indf$ASC), 'c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/rawASCCodes.csv', row.names = F)
odf3$PPF<- str_remove(odf3$PPF, ' ')
odf3$PPF<- str_remove(odf3$PPF, '[+]')
odf3$PPF<- str_remove(odf3$PPF, 'KS-')
odf3$PPF<- str_remove(odf3$PPF, '[?]')

mappingsAll <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/PPFtoASClut.csv', stringsAsFactors = F)
mappingsU <- mappingsAll %>% distinct(PPF,ASCcode , .keep_all = TRUE)
mappings <-mappingsU[order(mappingsU$PPF), c(2:3)]
#write.csv(mappings,'c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/PPFGroups.csv')

colnames(mdf2)
odf2 <- mdf2[c(2:21, 1, 23)]
colnames(odf2)[22] <- 'ASCcodeFromPPF'
head(odf2)



#write.csv(unique(odf2$GSG),'c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/GSGGroups.csv')



######  Apply the soil groupings

odf1<- read.csv(paste0(workDir2, '/obs_With_CleanASCCodes_and_HorGrp.csv'), stringsAsFactors = F)

ASCmappings <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/ASCGroups.csv', stringsAsFactors = F)
nrow(odf1)
mdf1 <- merge(odf1, ASCmappings, by=c('ASC'), all.x=T)
nrow(mdf1)
head(mdf1)

PPFmappings <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/PPFGroups.csv', stringsAsFactors = F)
mdf2 <- merge(mdf1, PPFmappings, by=c('PPF'), all.x=T)
nrow(mdf2)
head(mdf2)

gsgmaps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/GSGGroups.csv', stringsAsFactors = F)
mdf3 <- merge(mdf2, gsgmaps, by=c('GSG'), all.x=T)
nrow(mdf3)
head(mdf3)

colnames(mdf3)
odf3 <- mdf3[c(4:21, 1:3, 22:26)]
head(odf3)

#add final soilSupergroup Column
odf3$SoilSuperGrp <- NA
idxs <- which(!is.na(odf3$SuperGrpFromASC))
odf3$SoilSuperGrp[idxs] <- odf3$SuperGrpFromASC[idxs]
head(odf3)

#Work out which group to use next - probably PPF by numbers below but not much in it. I am going with GSG because better fit to supergroups
sum(!is.na(odf3$ASC))
sum(!is.na(odf3$PPF))
sum(!is.na(odf3$GSG))
sum(is.na(odf3$ASC) & !is.na(odf3$PPF))
sum(is.na(odf3$ASC) & !is.na(odf3$GSG))
sum(is.na(odf3$ASC) & !is.na(odf3$PPF) & is.na(odf3$GSG))
sum(is.na(odf3$ASC) & is.na(odf3$PPF) & !is.na(odf3$GSG))

sum(!is.na(odf3$SoilSuperGrp))
idxs <- which(is.na(odf3$SoilSuperGrp) & !is.na(odf3$GSG))
odf3$SoilSuperGrp[idxs] <- odf3$SuperGrpFromGSG[idxs]
sum(!is.na(odf3$SoilSuperGrp))
idxs <- which(is.na(odf3$SoilSuperGrp) & !is.na(odf3$PPF))
odf3$SoilSuperGrp[idxs] <- odf3$SuperGrpFromPPF[idxs]
sum(!is.na(odf3$SoilSuperGrp))

head(odf3)

odf4 <- odf3[with(odf3, order(DataStore, Dataset, Location_ID, Longitude, Latitude, UpperDepth, LowerDepth)), ]
head(odf4)

idxs <- which(odf4$Dataset == 'BM')

write.csv(odf4, paste0(workDir2, '/obs_With_HorGrp_and_SoilGrps.csv'), row.names = F)

