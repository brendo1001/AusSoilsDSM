library(httr)
library(jsonlite)
library(raster)
library(sf)
library(stringr)


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
datasetsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
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
write.csv(df, paste0(workDir, '/SDF_All_Sand.csv'))




#### tidy up data Clay data
dfc <- read.csv(paste0(workDir, '/SDF_All_Clay.csv'))[,-c(1,2)]
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc$Value[idxs]
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='-')
dfcC <- dfc[-idx2,]
t <- as.numeric(dfcC$Value)
which(is.na(t))
head(dfcC)
write.csv(dfcC, paste0(workDir, '/Clean_Clay.csv'))


#### tidy up data Silt data
dfc <- read.csv(paste0(workDir, '/SDF_All_Silt.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc$Value[idxs]
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='-')
dfcC <- dfc[-idx2,]
t <- as.numeric(dfcC$Value)
which(is.na(t))
head(dfcC)
write.csv(dfcC, paste0(workDir, '/Clean_Silt.csv'))

#### tidy up data Fine Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_SandFine.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc$Value[idxs]
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='-')
dfcC <- dfc[-idx2,]
t <- as.numeric(dfcC$Value)
which(is.na(t))
head(dfcC)
write.csv(dfcC, paste0(workDir, '/Clean_SandFine.csv'))


#### tidy up data Coarse Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_SandCoarse.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc$Value[idxs]
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='-')
dfcC <- dfc[-idx2,]
t <- as.numeric(dfcC$Value)
which(is.na(t))
head(dfcC)
write.csv(dfcC, paste0(workDir, '/Clean_SandCoarse.csv'))


#### tidy up data Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_Sand.csv'))[,-c(1,2)]
head(dfc)
which(is.na(dfc$Value))
t <- as.numeric(dfc$Value)
idxs <- which(is.na(t))
dfc$Value[idxs]
idx2 <- which(dfc$Value=='<1')
dfc$Value[idx2] <- 0.5
idx2 <- which(dfc$Value=='<1.0')
dfc$Value[idx2] <- 0.5
t <- as.numeric(dfcC$Value)
which(is.na(t))
head(dfc)
write.csv(dfc, paste0(workDir, '/Clean_Sand.csv'))


csC <- read.csv( paste0(workDir, '/Clean_SandCoarse.csv'))[,c(-1)]
fsC <- read.csv( paste0(workDir, '/Clean_SandFine.csv'))[,c(-1)]
head(csC)


mdf <- merge(fsC, csC, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
nrow(mdf)
head(mdf)

mdf$Value <- as.numeric(mdf$Value.x) + as.numeric(mdf$Value.y)
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
clayDF <-  read.csv(paste0(workDir, '/Clean_Clay.csv'))[ ,c(-1)]
siltDF <-  read.csv(paste0(workDir, '/Clean_Silt.csv'))[ ,c(-1)]
head(sandDF)
head(clayDF)
head(siltDF)

mdf1 <- merge(sandDF, clayDF, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
nrow(mdf1)
head(mdf1)
mdf2 <- mdf1[,c(1:12, 14, 22,16:19)]
head(mdf2)
colnames(mdf2) <- c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType',
                    'Sand', 'Clay', 'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy')

mdf3 <- merge(mdf2, siltDF, by=c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType'))
mdf4 <- mdf3[,c(1:14, 20, 15:18)]
head(mdf4)
colnames(mdf4) <- c('DataStore','Dataset','Provider','Location_ID','Layer_ID','SampleID','SampleDate','Longitude','Latitude','UpperDepth','LowerDepth','PropertyType',
                    'Sand', 'Clay', 'Silt', 'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy')
head(mdf4)

write.csv(mdf4, paste0(workDir, '/Clean_AllPCAComponents.csv'), row.names = F)


pSADF <- read.csv( paste0(workDir, '/Clean_AllPCAComponents.csv'), stringsAsFactors = F)
ptsDF <- na.omit(data.frame(Longitude=pSADF$Longitude, Latitude=pSADF$Latitude))
pts = st_as_sf(ptsDF, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(pts))
st_write(pts, 'C:/Projects/Small/soilsites/soilLocs.shp')



######  Extract Bulk Density data
att='BulkDensity'

datasetsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
datasets<-datasetsDF$DataSet

url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=DENSITY"))
resp <- GET(url, timeout(300))
response <- content(resp, "text", encoding = 'UTF-8')
propRecs <- fromJSON(response)
props <- propRecs$Property

sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


##  merge dataframes for different methods for each PSA component
fls <- list.files(workDir, full.names = T)
attFls <- fls[which(grepl('SDF_BulkDensity_', fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_BulkDensity.csv'))




##### extract the Soil Classification data
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"))
pgs <- fromJSON(url)
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=observations"))
props <- fromJSON(url)
att='Classification'


props <- c('O_GSG', 'O_PPF', 'O_ASC_ORD')
props <- c('O_ASC_ORD')

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

write.csv(mdf4, paste0(workDir, '/SDF_All_Classification.csv'))





##### extract the Soil SOC data
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"))
pgs <- fromJSON(url)
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=Carbon"))
props <- fromJSON(url)$Property
att='SOC'


sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


##  merge dataframes for different methods for each PSA component
fls <- list.files(workDir, full.names = T)
attFls <- fls[which(grepl(paste0('SDF_', att), fls))]
alldfs = lapply(attFls, read.csv)
df <- do.call(rbind,alldfs)
write.csv(df, paste0(workDir, '/SDF_All_SOC.csv'))

#### tidy up data Sand data
dfc <- read.csv(paste0(workDir, '/SDF_All_SOC.csv'), stringsAsFactors = F)[,-c(1,2)]
head(dfc)

dfc$Value <- str_replace(dfc$Value, '<', '')
idx2 <- which(dfc$Value=='-')
dfcC <- dfc[-idx2,]
t <- as.numeric(dfcC$Value)

# going to leave High SOC value in for now
which(is.na(t))
which(as.numeric(dfcC$Value) > 10)

write.csv(dfcC, paste0(workDir, '/Clean_SOC.csv'))




###### want to extract horizon codes next


#use ASC map to get assign ASC where not available
#use landuse map to get avg soc where not available

#or extract both from SLGA


bdDF <- read.csv(paste0(workDir, '/SDF_All_BulkDensity.csv'), stringsAsFactors = T)
stDF <- read.csv(paste0(workDir, '/SDF_All_Classification.csv'), stringsAsFactors = T)

mdf1 <- merge(stDF, bdDF, all=T, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude'))

