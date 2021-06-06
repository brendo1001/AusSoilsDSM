library(httr)
library(jsonlite)
library(raster)
library(sf)
library(stringr)
library(dplyr)


sdfExtract <- function(att, props, datasets, outDir=NULL, usr='Demo', key='Demo'){
  
  # Iterate Properties
  cnt=0
  for(i in 1:length(props)){
    # Make an empty list to put individual results in for this property
    res <- vector("list", length = length(datasets))
    print(props[i])
    # Iterate Datasets
    for (j in 1:length(datasets)) {
      p <- props[i]
      d <- datasets[j]
      print(paste0(d, ' : ', p))
      url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
      resp <- GET(url, timeout(300))
      response <- content(resp, "text", encoding = 'UTF-8')
      odf <- fromJSON(response)
      
      # If there is data returned slot it into the list
      if(is.data.frame(odf)){
        if(nrow(odf)>0){
          res[[j]] <- odf
          cnt=cnt+1
        }
      }
    }
    # Merge the dataframes in the list into one dataframe
    outDF = as.data.frame(data.table::rbindlist(res, fill=T))
    write.csv(outDF, paste0(outDir, '/SDF_', att, '_', p, '.csv'))
  
  }
  return(paste0(cnt, ' datasets extracted'))
}

usr <- 'ross.searle@csiro.au'
key <- 'a'

workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'

# Get the available datasets
datasetsDF <- fromJSON(paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?usr=', usr, '&key=', key))
datasets<-datasetsDF$DataSet

groups <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups')

props <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties')

str(props)

PSAProps <- props[props$PropertyGroup == 'PSA', ]
#write.csv(PSAProps, paste0(workDir, '/PSA_ALL.csv'))
# Manually edited to add PSA groups
allPropCodes <- read.csv(paste0(workDir, '/PSA_ALL.csv'))



### Extract the PSA data from the SDF 

satts <- c('CL', 'ZS', 'TS', 'CS', 'FS')

for (k in 1:length(satts)) {
  
  att <- satts[k]
  props <- allPropCodes[allPropCodes$PSAComponent==att, ]$Property
  
  # Iterate Properties
  for(i in 1:length(props)){
    # Make an empty list to put individual results in for this property
    res <- vector("list", length = length(datasets))
    # Iterate Datasets
    for (j in 1:length(datasets)) {
      p <- props[i]
      d <- datasets[j]
      print(paste0(d, ' : ', p))
      url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
      resp <- GET(url, timeout(300))
      response <- content(resp, "text", encoding = 'UTF-8')
      odf <- fromJSON(response)
      
      # If there is data returned slot it into the list
      if(is.data.frame(odf)){
        if(nrow(odf)>0){
          res[[j]] <- odf
        }
      }
    }
    # Merge the dataframes in the list into one dataframe
    outDF = as.data.frame(data.table::rbindlist(res, fill=T))
    # Write it to a csv
    write.csv(outDF, paste0(workDir, '/SDF_', att, '_', p, '.csv'))
  }
}


##  merge dataframes for different methods for each PSA component
fls <- list.files(workDir, full.names = T)
attFls <- fls[which(grepl('SDF_TS_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
head(df)
write.csv(df, paste0(workDir, '/SDF_All_Sand.csv'))

attFls <- fls[which(grepl('SDF_FS_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_SandFine.csv'))

attFls <- fls[which(grepl('SDF_CS_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_SandCoarse.csv'))

attFls <- fls[which(grepl('SDF_ZS_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_Silt.csv'))

attFls <- fls[which(grepl('SDF_CL_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_Clay.csv'))



#### tidy up data Clay data
dfc <- read.csv(paste0(workDir, '/SDF_All_Clay.csv'))[,-c(1,2)]
nrow(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
which(is.na(t))
unique(dfc$Value[idxs])
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='-')
dfc <- dfc[-idx2,]
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
dfc <- dfc[-idxs,]
dfc$Value <- as.numeric(dfc$Value)
t <- as.numeric(dfc$Value)
head(dfc)
nrow(dfc)
wa <- dfc[dfc$Dataset=='WAGovernment', ]
mean(wa$Value)
write.csv(dfc, paste0(workDir, '/Clean_Clay.csv'))


#### tidy up data Silt data
dfc <- read.csv(paste0(workDir, '/SDF_All_Silt.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
dfc <- dfc[-idxs,]
dfc$Value <- as.numeric(dfc$Value)
t <- as.numeric(dfc$Value)
which(is.na(t))
head(dfc)
nrow(dfc)
write.csv(dfc, paste0(workDir, '/Clean_Silt.csv'))

#### tidy up data Fine Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_SandFine.csv'))[,-c(1,2)]
head(dfc)
idxs <- which(is.na(dfc$Value))
dfc <- dfc[-idxs]
nrow(dfc)
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
dfc <- dfc[-idxs,]
dfc$Value <- as.numeric(dfc$Value)
t <- as.numeric(dfc$Value)
which(is.na(t))
head(dfc)
write.csv(dfc, paste0(workDir, '/Clean_SandFine.csv'))

# waFS <- dfc[dfc$Dataset=='WAGovernment', ]
# head(waFS)
# nrow(waFS)
# mean(as.numeric(waFS$Value, na.rm=T  ))




#### tidy up data Coarse Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_SandCoarse.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc$Value[idxs]
unique(dfc$Value[idxs])
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
dfc <- dfc[-idxs,]
dfc$Value <- as.numeric(dfc$Value)
t <- as.numeric(dfc$Value)
which(is.na(t))
head(dfc)

# waCS <- dfc[dfc$Dataset=='WAGovernment', ]
# mean(as.numeric(waCS$Value, na.rm=T  ))


write.csv(dfc, paste0(workDir, '/Clean_SandCoarse.csv'))


#### tidy up data Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_Sand.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
# dfc$Value[idxs]
 idx2 <- which(dfc$Value=='<1')
 dfc$Value[idx2] <- 0.5
 idx2 <- which(dfc$Value=='<1.0')
 dfc$Value[idx2] <- 0.5
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc <- dfc[-idxs,]
head(dfc)

waTS <- dfc[dfc$Dataset=='WAGovernment', ]
head(waTS)
mean(as.numeric(waTS$Value, na.rm=T  ))
nrow(waTS)

write.csv(dfc, paste0(workDir, '/Clean_Sand.csv'))


csC <- read.csv( paste0(workDir, '/Clean_SandCoarse.csv'))[,c(-1)]
fsC <- read.csv( paste0(workDir, '/Clean_SandFine.csv'))[,c(-1)]
head(csC)
head(fsC)
nrow(csC)
nrow(fsC)

mdf <- merge(fsC, csC, all=T, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
nrow(mdf)
head(mdf)
#write.csv(mdf, 'c:/temp/test.csv')

idxs <- which(!is.na(mdf$Value.x) & !is.na(mdf$Value.y))
length(idxs)
mdf$Value[idxs] <- as.numeric(mdf$Value.x[idxs]) + as.numeric(mdf$Value.y[idxs])

idxs <- which(!is.na(mdf$Value.x) & is.na(mdf$Value.y))
length(idxs)
mdf$Value[idxs] <- as.numeric(mdf$Value.x[idxs])

idxs <- which(is.na(mdf$Value.x) & !is.na(mdf$Value.y))
length(idxs)
mdf$Value[idxs] <- as.numeric(mdf$Value.y[idxs])

v <- mdf[mdf$Value>0 & mdf$Value <105,]
summary(v$Value)
hist(v$Value)

#mdf$Value <- as.numeric(mdf$Value.x) + as.numeric(mdf$Value.y)
mdf$ObservedProperty <- 'SandTotal'
colnames(mdf)
mdf2 <- mdf[,c(1:12, 30, 29,15:20)]
colnames(mdf2) <- c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType',
                    'ObservedProperty', 'Value','Units', 'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy', 'ExtractTime')
head(mdf2)
write.csv(mdf2, paste0(workDir, '/Clean_SandMergeFineAndCoarse.csv'), row.names = F )



##  rBind the 2 sand dataframes together
dfT <- read.csv(paste0(workDir, '/Clean_Sand.csv'))[,c(-1)]
nrow(dfT)
dfM <- read.csv(paste0(workDir, '/Clean_SandMergeFineAndCoarse.csv'))
nrow(dfM)
head(dfT)
head(dfM)

bs <- rbind(dfT,dfM)
nrow(bs)
write.csv(bs, paste0(workDir, '/Clean_SandAll.csv'))
  




### Next step is to merge all the PSA dataframes

sandDF <- read.csv(paste0(workDir, '/Clean_SandAll.csv'))[ ,c(-1)]
head(sandDF)

idxs <- which(is.na(sandDF$Value))
s <- sandDF[idxs,]
clayDF <-  read.csv(paste0(workDir, '/Clean_Clay.csv'))[ ,c(-1)]
siltDF <-  read.csv(paste0(workDir, '/Clean_Silt.csv'))[ ,c(-1)]
head(sandDF)
head(clayDF)
head(siltDF)

#vSand <- sandDF[sandDF$Dataset=='SAGovernment', ]
#vClay <- clayDF[clayDF$Dataset=='SAGovernment', ]
#write.csv(vSand, 'c:/temp/vsand.csv')
#write.csv(vClay, 'c:/temp/vClay.csv')

nrow(sandDF)
sandDF<-sandDF[which(!duplicated(sandDF)),]
which(duplicated(sandDF))
nrow(clayDF)
clayDF<-clayDF[which(!duplicated(clayDF)),]
which(duplicated(clayDF))


mdf1 <- merge(sandDF, clayDF, all=T, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
nrow(mdf1)
# mdf2 <- merge(sandDF, clayDF, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
# nrow(mdf2)
# wa1 <- mdf1[mdf1$Dataset=='WAGovernment', ]
# wa2 <- mdf2[mdf2$Dataset=='WAGovernment', ]
# mean(wa1$Value.y, na.rm=T)
# mean(wa2$Value.y, na.rm=T)
#vSand <- mdf1[mdf1$Dataset=='VicGovernment', ]

head(mdf1)
mdf2 <- mdf1[,c(1:12, 14, 22,16:19)]
head(mdf2)
colnames(mdf2) <- c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType',
                    'Sand', 'Clay', 'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy')
head(mdf2)

#write.csv(mdf2, 'c:/temp/test.csv')

siltDF<-siltDF[which(!duplicated(siltDF)),]

mdf3 <- merge(mdf2, siltDF,all=T, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
mdf4 <- mdf3[,c(1:14, 20, 15:18)]
head(mdf4)
colnames(mdf4) <- c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType',
                    'Sand', 'Clay', 'Silt', 'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy')
head(mdf4)
nrow(mdf4)

nrow(mdf4[mdf4$Dataset=='NSWGovernment', ])

idxs <- which((mdf4$Sand + mdf4$Silt + mdf4$Clay) > 110 )
mdf4 <- mdf4[-idxs,]
nrow(mdf4)
idxs <- which((mdf4$Sand + mdf4$Silt + mdf4$Clay) < 90 )
# length(idxs)
# mdf4[idxs, ]
# write.csv(mdf4[idxs, ], 'c:/temp/psalow.csv')
# vals <- mdf4$Sand[idxs] + mdf4$Silt[idxs] + mdf4$Clay[idxs]
# hist(vals[vals>0])
# max(vals[vals>0])
mdf4 <- mdf4[-idxs,]
nrow(mdf4)
mdf5 <- mdf4[!duplicated(mdf4),]
nrow(mdf5)
write.csv(mdf5, paste0(workDir, '/Clean_AllPCAComponents.csv'), row.names = F)

mdf4[mdf4$Dataset=='VicGovernment', ]

pSADF <- read.csv( paste0(workDir, '/Clean_AllPCAComponents.csv'), stringsAsFactors = F)
ptsDF <- na.omit(data.frame(Longitude=pSADF$Longitude, Latitude=pSADF$Latitude))
pts = st_as_sf(ptsDF, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(pts))


######  Extract Bulk Density data
att='BulkDensity'

datasetsDF <- fromJSON(paste0('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?usr=', usr, '&key=', key))
datasets<-datasetsDF$DataSet

url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=DENSITY"))
resp <- GET(url, timeout(300))
response <- content(resp, "text", encoding = 'UTF-8')
propRecs <- fromJSON(response)
props <- propRecs$Property

sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


##  merge dataframes for different methods for each Bulk Density component
fls <- list.files(workDir, full.names = T)
attFls <- fls[which(grepl('SDF_BulkDensity_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
df[duplicated(df),]
write.csv(df, paste0(workDir, '/SDF_All_BulkDensity.csv'))

bdf <- read.csv(paste0(workDir, '/SDF_All_BulkDensity.csv'), stringsAsFactors = F)
str(bdf)
t <- as.numeric(bdf$Value)
idxs <- which(is.na(t))
bdf$Value[idxs]
unique(bdf$Value[idxs])
idx2 <- which(bdf$Value=='-')
bdf <- bdf[-idx2,]
nrow(bdf)
str(bdf)
t <- as.numeric(bdf$Value)
bdf$Value <- as.numeric(bdf$Value)
write.csv(bdf, paste0(workDir, '/SDF_All_BulkDensity.csv'), row.names = F)



##### extract the Soil Classification data
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"))
pgs <- fromJSON(url)
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=observations"))
props <- fromJSON(url)
att='Classification'


props <- c('O_GSG', 'O_PPF', 'O_ASC_ORD')
#props <- c('O_ASC_ORD')

sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


gsgDF <- read.csv(paste0(workDir, '/SDF_Classification_O_GSG.csv'), stringsAsFactors = F)
ppfDF <- read.csv(paste0(workDir, '/SDF_Classification_O_PPF.csv'), stringsAsFactors = F)
ASCDF <- read.csv(paste0(workDir, '/SDF_Classification_O_ASC_ORD.csv'), stringsAsFactors = F)

mdf1 <- merge(gsgDF, ppfDF, all=T, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType'))
head(mdf1)
colnames(mdf1)
mdf2 <- mdf1[,c(1:8,15, 28, 17:20)]
head(mdf2)
colnames(mdf2) <- c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType',
                    'GSG', 'PPF',  'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy')
head(mdf2)

mdf3 <- merge(mdf2, ASCDF, all=T, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType'))
head(mdf3)
colnames(mdf3)
mdf4 <- mdf3[,c(1:10,21)]
head(mdf4)
colnames(mdf4) <- c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType',
                    'GSG', 'PPF', 'ASC')

mdf5 <- mdf4[!duplicated(mdf4),]
write.csv(mdf5, paste0(workDir, '/SDF_All_Classification.csv'))
#mdf4[mdf4$Dataset=='VicGovernment', ]
cdf <- read.csv(paste0(workDir, '/SDF_All_Classification.csv'))
count(cdf[!is.na(cdf$ASC), ], 'Dataset')


##### extract the Soil SOC data
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"))
pgs <- fromJSON(url)
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Carbon"))
props <- fromJSON(url)$Property
att='SOC'


sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


##  merge dataframes for different methods for each SOC component
fls <- list.files(workDir, full.names = T)
attFls <- fls[which(grepl(paste0('SDF_', att), fls))]
alldfs = lapply(attFls, read.csv)

df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_SOC.csv'), row.names = F)

#### tidy up data SOC data
dfc <- read.csv(paste0(workDir, '/SDF_All_SOC.csv'), stringsAsFactors = F)[,-c(1)]
head(dfc)

t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])

idx2 <- which(dfc$Value=='-')
#dfc <- dfc[-idx2,]

idxxs <- which(dfc$Value=='<12')
dfc[idxxs,]
dfc<-dfc[-idxxs,]
nrow(dfc)
dfc$Value <- str_replace(dfc$Value, '<0.3', '0.15')
dfc$Value <- str_replace(dfc$Value, '<0.05', '0.025')
dfc$Value <- str_replace(dfc$Value, '<0.10', '0.05')
dfc$Value <- str_replace(dfc$Value, '<0.03', '0.015')
dfc$Value <- str_replace(dfc$Value, '<0.1', '0.05')
dfc$Value <- str_replace(dfc$Value, '< 0.03', '0.015')
dfc$Value <- str_replace(dfc$Value, '<0.050', '0.025')

t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
unique(dfc$Value[idxs])
dfc<-dfc[-idxs,]

t <- as.numeric(dfc$Value)
dfc$Value <- as.numeric(dfc$Value)

spdf <- dfc[dfc$ObservedProperty %in% c('6B4b_SCaRP','6H2a_SCaRP','6H2b_SCaRP','6H2c_SCaRP', '6B3a'), ]
unique(spdf$Dataset)
idxs <- which(dfc$ObservedProperty %in% c('6B4b_SCaRP','6H2a_SCaRP','6H2b_SCaRP','6H2c_SCaRP', '6B3a'))
dfc[idxs,]
mean(dfc$Value[idxs])
dfc$Value[idxs] <- (dfc$Value[idxs] / 10)
mean(dfc$Value[idxs])

unique(dfc$Dataset)

tdf <- dfc[dfc$Dataset=='SCARP',]
nrow(tdf)

# going to leave High SOC value in for now
which(is.na(t))
which(as.numeric(dfc$Value) > 10)

dfc2 <- dfc[!duplicated(dfc),]
write.csv(dfc2, paste0(workDir, '/Clean_All_SOC.csv'))



##### extract the Soil Horizon Name data
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"))
pgs <- fromJSON(url)
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Horizons"))
props <- fromJSON(url)$Property
att='Horizons'

props <- 'h_name'
props <- 'COL_HUE_VAL_CHROM'
props <- 'h_no'

sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)

df <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_Horizons_H_DESIG_MASTER.csv', stringsAsFactors = F)
df2 <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_Horizons_h_name.csv', stringsAsFactors = F)
df3 <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_Horizons_COL_HUE_VAL_CHROM.csv', stringsAsFactors = F)
which(duplicated(df))

nrow(df)
nrow(df2)
nrow(df3)

unique(df$Dataset)
unique(df2$Dataset)

#### Do clean up on depths and locations


psa <- read.csv(paste0(workDir, '/Clean_AllPCAComponents.csv'), stringsAsFactors = F)
nrow(psa)
head(psa)

idxs <- which(is.na(psa$Longitude))
length(idxs)
psa[idxs,]
nrow(psa)
psa <- psa[-idxs,]
nrow(psa)
#idxs <- which(!complete.cases(psa))
#idxs <- which(is.na(psa$Sand))
#s <- psa[idxs,]
t <- as.numeric(psa$UpperDepth)
idxs <- which(is.na(t))
psa[idxs,]
psa <- psa[-idxs,]
nrow(psa)

bd <- read.csv(paste0(workDir, '/SDF_All_BulkDensity.csv'), stringsAsFactors = F)[,-c(1:2)]
nrow(bd)
head(bd)
idxs <- which(is.na(bd$Longitude))
bd[idxs,]
nrow(bd)
bd <- bd[-idxs,]
nrow(bd)
t <- as.numeric(bd$UpperDepth)
idxs <- which(is.na(t))


## sort out the soil classification data
soilClassRaw <- read.csv( paste0(workDir, '/SDF_All_Classification.csv'), stringsAsFactors = F)[,-1]
nrow(soilClassRaw)
head(soilClassRaw)
t <- as.numeric(soilClassRaw$Longitude)
idxs <- which(is.na(t))
idxs
soilClass <- soilClassRaw[-idxs,]
count(soilClassRaw[!is.na(soilClassRaw$ASC), ], 'Dataset')
count(soilClassRaw[!is.na(soilClassRaw$PPF), ], 'Dataset')
count(soilClassRaw[!is.na(soilClassRaw$GSG), ], 'Dataset')
nrow(soilClassRaw)

ascgrps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/ASCCodes.csv', stringsAsFactors = F)
head(ascgrps)
mdf1 <- merge(soilClassRaw, ascgrps, all.x=T, all.y=F, by=c('ASC'))
nrow(mdf1)
head(mdf1)
colnames(mdf1)
soilgrps <- mdf1[,-1]
head(soilgrps)
nrow(soilgrps)

unique(soilgrps$ASCCode)

ASCmappings <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/ASCGroups.csv', stringsAsFactors = F)
head(ASCmappings)
nrow(soilgrps)
mdf1 <- merge(soilgrps, ASCmappings, by.x = 'ASCCode', by.y = 'ASC', all.x=T)
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
odf3 <- mdf3[c(4:10, 1:3, 12:15)]
head(odf3)

#add final soilSupergroup Column and merge in supergrps from soil classifications
odf3$SoilSuperGrp <- NA
idxs <- which(!is.na(odf3$SuperGrpFromASC))
odf3$SoilSuperGrp[idxs] <- odf3$SuperGrpFromASC[idxs]
head(odf3)

idxs <- which(is.na(odf3$SuperGrpFromASC) & !is.na(odf3$SuperGrpFromGSG))
odf3$SoilSuperGrp[idxs] <- odf3$SuperGrpFromGSG[idxs]
head(odf3)

idxs <- which(is.na(odf3$SuperGrpFromASC) & is.na(odf3$SuperGrpFromGSG) & !is.na(odf3$SuperGrpFromPPF))
odf3$SoilSuperGrp[idxs] <- odf3$SuperGrpFromPPF[idxs]
head(odf3)
nrow(odf3)

write.csv(odf3, 'C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/MergedRawWithSoilSuperGroups.csv', row.names=F)




hors <- read.csv( paste0(workDir, '/SDF_Horizons_COL_HUE_VAL_CHROM.csv'), stringsAsFactors = F)[,-1]
unique(hors$Dataset)
nrow(hors)
head(hors)
t <- as.numeric(hors$Longitude)
idxs <- which(is.na(t))
idxs
hors[idxs,]
hors <- hors[-idxs,]
#colnames(hors)[14] <- 'RawHor'


horNames <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_Horizons_h_name.csv', stringsAsFactors = F)[,-1]
head(horNames)
colnames(horNames)[14] <- 'RawHor'

horgrps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/HorGroups.csv', stringsAsFactors = F)
head(horgrps)
nrow(hors)
hm <-  merge(horNames, horgrps, all.x=T, all.y=T, by.x = 'RawHor', by.y='HorName')
nrow(hm)
head(hm)
colnames(hm)
hm2 <- hm[,c(2:12,1,22 )]
head(hm2)

write.csv(hm2, 'c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/MergedHorGroups.csv', row.names = F)

nrow(hors)
nrow(hm2)
head(hors)
head(hm2)
hm <-  merge(hors, hm2, all.x=T, all.y=T, by=c('DataStore','Dataset','Provider','Location_ID','Longitude','Latitude','UpperDepth','LowerDepth'))
nrow(hm)
unique(hm$Dataset)
unique(hors$Dataset)

##### merge all of the required soil property data together with a left join on PSA
### merge psa and BD
nrow(psa)
which(duplicated(psa))
which(duplicated(bd))
bd <- bd[!duplicated(bd),]
mdf1 <- merge(psa, bd, all.x=T, all.y=F, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','Layer_ID','SampleID','UpperDepth','LowerDepth'))
which(duplicated(mdf1))
head(mdf1)
colnames(mdf1)
mdf2 <- mdf1[,c(1:11,13:15, 22)]
head(mdf2)
colnames(mdf2) <- c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','Layer_ID','SampleID','UpperDepth','LowerDepth','Sand', 'clay', 'Silt', 'BlkDen')
head(mdf2)
nrow(mdf2)
#write.csv(mdf2, 'c:/temp/test.csv')

write.csv(mdf2, 'c:/Projects/TernLandscapes/AWC/DataMassage/BD_PSAMerged.csv', row.names = F)

### Merge in SOC

soc <- read.csv( paste0(workDir, '/Clean_All_SOC.csv'), stringsAsFactors = F)[,-c(1)]
nrow(soc)
head(soc)
t <- as.numeric(soc$Longitude)
idxs <- which(is.na(t))
idxs
soc <- soc[-idxs,]
nrow(soc)
t <- as.numeric(soc$UpperDepth)
idxs <- which(is.na(t))
idxs
soc <- soc[-idxs,]
nrow(soc)



which(duplicated(mdf2))
mdf2 <- mdf2[!duplicated(mdf2),]
which(duplicated(soc))

nrow(mdf2)
mdf3 <- merge(mdf2, soc, all.x=T, all.y=F, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','Layer_ID','SampleID','UpperDepth','LowerDepth'))
nrow(mdf3)
mdf3 <- mdf3[!duplicated(mdf3),]
nrow(mdf3)
colnames(mdf3)
mdf4 <- mdf3[,c(1:15,18)]
head(mdf4)
colnames(mdf4) <- c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','Layer_ID','SampleID','UpperDepth','LowerDepth','Sand', 'clay', 'Silt', 'BlkDen', 'SOC')
head(mdf4)
nrow(mdf4)

idxs <- which(!duplicated(mdf4))
length(idxs)
idxs <- which(duplicated(mdf4))
length(idxs)
mdf44 <- mdf4[-idxs,]
nrow(mdf4)
nrow(mdf44)

write.csv(mdf44, 'c:/Projects/TernLandscapes/AWC/DataMassage/BD_PSA_SOC_Merged.csv', row.names = F)

source('C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/AusSoilsDSM/SLGA/Development/Ross/Scripts/AWC/joinSamplesToHorizons.R')

inSamples <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/BD_PSA_SOC_Merged.csv', stringsAsFactors = F)
inHorizons <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/Horizons/MergedHorGroups.csv', stringsAsFactors = F)
head(inHorizons)
nrow(inHorizons)
head(inSamples)
nrow(inSamples)

jdf <- joinSamplesToHorizonsPara(inHorizons, inSamples, tempDir = 'c:/temp' )
head(jdf)
nrow(jdf)
nrow(inHorizons)
nrow(inSamples)
unique(inSamples$Dataset)
unique(inHorizons$Dataset)

str(jdf)
jdf$Longitude <- as.numeric(jdf$Longitude)
jdf$Latitude <- as.numeric(jdf$Latitude)
jdf$UpperDepth <- as.numeric(jdf$UpperDepth)
jdf$LowerDepth <- as.numeric(jdf$LowerDepth)

jdf$midpoint <- as.numeric(jdf$midpoint)
jdf$sampUpperDepth<- as.numeric(jdf$sampUpperDepth)
jdf$sampLowerDepth<- as.numeric(jdf$sampLowerDepth)

jdf$Sand <- as.numeric(jdf$Sand)
jdf$Silt <- as.numeric(jdf$Silt)
jdf$clay <- as.numeric(jdf$clay)
jdf$BlkDen <- as.numeric(jdf$BlkDen)
jdf$SOC <- as.numeric(jdf$SOC)
str(jdf)
write.csv(jdf, 'c:/Projects/TernLandscapes/AWC/DataMassage/BD_PSA_SOC_Merged.csv', row.names = F)


####the datasets below get knocked out of the merge above as they don't have any horizons recorded, so the next section merges them back in

idxs <- which(!unique(inSamples$Dataset) %in% unique(inHorizons$Dataset))
projs <- unique(inSamples$Dataset)[idxs]
tdf <- inSamples[inSamples$Dataset=='SCARP',]
so <- inSamples[inSamples$Dataset %in% projs,]

str(inSamples)

head(jdf)
str(jdf)
so$LowerDepth <- as.numeric(so$LowerDepth)
smdf <- data.frame(DataStore=so$DataStore, Dataset=so$Dataset, Provider=so$Provider, Location_ID=so$Location_ID, Layer_ID=so$Layer_ID, SampleID=so$SampleID,
                   SampleDate=so$SampleDate, Longitude=so$Longitude, Latitude=so$Latitude,UpperDepth=so$UpperDepth, LowerDepth=so$LowerDepth,
                   RawHor=NA, HorGrp=NA, midpoint=so$UpperDepth + ((so$LowerDepth - so$UpperDepth)/2),
                   sampUpperDepth=so$UpperDepth, sampLowerDepth=so$LowerDepth, Sand=so$Sand, clay=so$clay, Silt=so$Silt, BlkDen=so$BlkDen,  SOC=so$SOC
                   )
nrow(jdf)
nrow(smdf)
jdf2 <- rbind(jdf, smdf)
nrow(jdf2)

write.csv(jdf2, 'c:/Projects/TernLandscapes/AWC/DataMassage/sampsWithNoHorsMergedWithSampsWithHors.csv', row.names = F)

### Merge in soil classification


jdf <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/sampsWithNoHorsMergedWithSampsWithHors.csv',stringsAsFactors = F)
nrow(jdf)
soilClass <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/MergedRawWithSoilSuperGroups.csv',stringsAsFactors = F)

head(soilClass)
which(duplicated(soilClass))
which(duplicated(jdf))
jdf <- jdf[!duplicated(jdf),]
nrow(jdf)

mdf5 <- merge(jdf, soilClass, all.x=T, by=c('DataStore','Dataset','Provider','Location_ID','Longitude', 'Latitude'))
nrow(mdf5)
head(mdf5)
colnames(mdf5)
idxs <- which(!duplicated(mdf5))
length(idxs)
mdf55 <- mdf5[idxs,]
nrow(mdf55)
head(mdf55)
#write.csv(mdf5, 'c:/temp/test.csv')
colnames(mdf55)
#mdf6 <- mdf55[,c(1:17,21:23)]
#head(mdf6)
#colnames(mdf6) <- c('DataStore','Dataset','Provider','Location_ID','Longitude','UpperDepth','SampleDate','Latitude','Layer_ID','SampleID','LowerDepth','Sand', 'clay', 'Silt', 'BlkDen', 'SOC', 'HorName', 'GSG',  'PPF',  'ASC')
#head(mdf6)
#nrow(mdf6)
colnames(mdf55)
outdf <- mdf55[, c(1:21, 23:30)]
colnames(outdf)
head(outdf)
colnames(outdf)[9] <- 'SampleDate'
head(outdf)


str(outdf)

bboxExt <- extent(112,154,-44,-10)
idxs <- which(outdf$Longitude >= bboxExt@xmin & outdf$Longitude <= bboxExt@xmax & outdf$Latitude >= bboxExt@ymin & outdf$Latitude <= bboxExt@ymax)
length(idxs)
outdf <- outdf[idxs, ]
nrow(outdf)
str(outdf)
head(outdf)
allpts <- st_as_sf(outdf, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
dsts <- unique(allpts$Dataset)
plot(st_geometry(allpts))

write.csv(outdf, paste0(workDir, '/SDF_All_Props.csv'), row.names = F)



#### Remove outliers

indf <- read.csv(paste0(workDir, '/SDF_All_Props.csv'), stringsAsFactors = F)
nrow(indf)
head(indf)



summary(indf)

# Locations
idxs <- which(is.na(indf$Longitude))
idxs
idxs <- which(is.na(indf$Latitude))
idxs

# Sand
idxs <- which(indf$Sand<0)
indf[idxs,]
#indf <- indf[-idxs,]
nrow(indf)
idxs <- which(indf$Sand>100 & indf$Sand<105)
indf[idxs,]
indf$Sand[idxs] <- 99
idxs <- which(indf$Sand>100)
indf[idxs,]
indf <- indf[-idxs,]
nrow(indf)

# Silt
idxs <- which(indf$Silt<0)
idxs
indf[idxs,]
indf$Silt[idxs] <- 0
indf <- indf[-idxs,]
nrow(indf)
idxs <- which(indf$Silt>100)
indf[idxs,]
#indf <- indf[-idxs,]
#indf$Silt[idxs] <- 15.6
nrow(indf)

# Clay
idxs <- which(indf$Clay<0)
idxs
idxs <- which(indf$Clay>100)
indf[idxs,]

# SOC
idxs <- which(indf$SOC<0)
idxs
idxs <- which(indf$SOC>40)
#idxs <- which(indf$SOC>100)
indf[idxs,]
indf <- indf[-idxs,]
indf[indf$Dataset=='SCARP',]
nrow(indf)
summary(indf)
unique(indf$Dataset)

idxs <- which(indf$UpperDepth>1000)
indf[idxs,]
indf <- indf[-idxs,]
max(indf$UpperDepth)
idxs <- which(is.na(indf$UpperDepth))
indf[idxs,]
#indf <- indf[-idxs,]

t <- as.numeric(indf$LowerDepth)
idxs <- which(is.na(t))
#indf[idxs, ]
#indf <- indf[-idxs,]
#indf$LowerDepth <- as.numeric(indf$LowerDepth)

idxs <- which(indf$UpperDepth>10)
indf[idxs,]
indf$UpperDepth[idxs] <- indf$UpperDepth[idxs]/100


idxs <- which(indf$LowerDepth>10)
indf <- indf[-idxs,]
nrow(indf)


# sites <- unique(indf$Location_ID[idxs])
# 



summary(indf)



idxs <- which(indf$Dataset=='BM')
indf[idxs,]
indf <- indf[-idxs, ]

write.csv(indf, paste0(workDir, '/SDF_All_Props_SomePSA_NAs.csv'), row.names = F)

indf <- read.csv( paste0(workDir, '/SDF_All_Props_SomePSA_NAs.csv'), stringsAsFactors = F)

### just keep PSA data where all have a value
nrow(indf)
idxs <- which(!is.na(indf$clay)  & !is.na(indf$Silt) & !is.na(indf$Sand))
length(idxs)
indf[idxs,]

indf <- indf[idxs,]
nrow(indf)

unique(indf$Dataset)

write.csv(indf, paste0(workDir, '/SDF_All_Props_Clean.csv'), row.names = F)




