library(doParallel)
library(doSNOW)
library(fst)



makeNumeric <- function(df, col){
  
    t <- as.numeric(df[,c(col)])
    idxs <- which(is.na(t))
    if(length(idxs) > 0){
      print(paste0(length(idxs), ' records removed from ', col))
      df <- df[-idxs,]
    }
    df[,c(col)] <- as.numeric(df[,c(col)])
  return(df)
}
removeNa <- function(df, col){
  
  idxs <- which(is.na(df[,c(col)]))
  if(length(idxs) > 0){
    print(paste0(length(idxs), ' records removed from ', col))
    df <- df[-idxs,]
  }
  return(df)
}

tidyTable <- function(df, cols = c('UpperDepth', 'LowerDepth')){
  
  for (i in 1:length(cols)) {
    if(cols[i] %in% cols){
      print(i)
      df <- makeNumeric(df, col=cols[i])
    }else{
      df <- removeNa(df, cols)
    }
  }
  return(df)
}

getChunkInfo <- function(chunkLines, tot){
  
  remainder <- tot%%(chunkLines)
  
  chunks <- floor(tot/chunkLines)
  starts <- seq.int(from=1,length=chunkLines,by=chunks)
  nlines <- rep(chunks, length(starts))
  if(remainder > 0){
    starts[length(nlines) + 1] <- starts[length(nlines)]+(chunks-1)
    nlines[length(nlines) + 1] <- tot- (chunks * chunkLines)
  }
  return(list(chunks=length(starts), starts=starts, nlines=nlines))
}


joinSamplesToHorizonsSerial <- function(inHorizons, inSamples){
  
  allhors= tidyTable(df=inHorizons)
  alllabs= tidyTable(df=inSamples)
  
  obs <- unique(allhors[c("DataStore", "Dataset", "Provider",  "Location_ID")]) # "Longitude", "Latitude",
  obs
  
  
  outDF <- data.frame(DataStore=character(),Dataset=character(),Provider=character(),Location_ID=character(),Layer_ID=character()
                      ,SampleID=character(),SampleDate=character(),Longitude=numeric(),Latitude=numeric(),UpperDepth=numeric()
                      ,LowerDepth=numeric(),PropertyType=character(),ObservedProperty=character(),Value=character()
                      ,Units=character(),QualCollection=numeric(),QualSpatialAggregation=numeric(),QualManagement=numeric()
                      ,QualSpatialAccuracy=numeric(),ExtractTime=character(), midpoint = numeric(), sampUpperDepth=numeric(), sampLowerDepth=numeric(), labValue=numeric())
  
  pb <- txtProgressBar(min=0, nrow(obs), style=3)
  for (i in 1:nrow(obs)) {
    #  for (i in 1:1000) {  
    #print(i)
    setTxtProgressBar(pb, i)
    obsR <- obs[i,]
    horizons <- allhors[allhors$DataStore==obsR$DataStore[1] & allhors$Dataset==obsR$Dataset[1] & allhors$Provider==obsR$Provider[1] & allhors$Location_ID==obsR$Location_ID[1],]
    samples <- alllabs[alllabs$DataStore==obsR$DataStore[1] & alllabs$Dataset==obsR$Dataset[1] & alllabs$Provider==obsR$Provider[1] & alllabs$Location_ID==obsR$Location_ID[1],]
    if(nrow(samples) > 0){
      
      for (j in 1:nrow(horizons)) {
        horizon <- horizons[j,] 
        for (k in 1:nrow(samples)) {
          #print(j)
          sample <- samples[k,]
          SampleMidPoint = sample$UpperDepth + ((sample$LowerDepth - sample$UpperDepth)/2)
          
          if(SampleMidPoint > horizon$UpperDepth & SampleMidPoint < horizon$LowerDepth){
            # we have a match
            #print('we have a match')
            # I think midpoint is OK for joining sample to horizon as sample will tend to be inside horizons - could check overlaps but all gets pretty convoluted pretty quickly
            outDF[nrow(outDF)+1,] = c(horizon$DataStore,horizon$Dataset,horizon$Provider,horizon$Location_ID,horizon$Layer_ID
                                      ,horizon$SampleID,horizon$SampleDate,horizon$Longitude,horizon$Latitude,horizon$UpperDepth
                                      ,horizon$LowerDepth,horizon$PropertyType,horizon$ObservedProperty,horizon$Value
                                      ,horizon$Units,horizon$QualCollection,horizon$QualSpatialAggregation,horizon$QualManagement
                                      ,horizon$QualSpatialAccuracy,horizon$ExtractTime, SampleMidPoint, sample$UpperDepth, sample$LowerDepth, sample$Value)
          }
        }
      }
    }
  }
  close(pb)
  return(outDF)
  
}


joinSamplesToHorizonsPara <- function(inHorizons, inSamples, tempDir=tempdir()){

  allhors = tidyTable(df=inHorizons)#[1:10000, ]
  alllabs = tidyTable(df=inSamples)#[1:10000, ]
  
  tempDirec <- paste0(tempDir, '/', as.numeric(Sys.time()))
  print(tempDirec)
  if(!dir.exists(tempDirec)){dir.create(tempDirec)}
  
  htab <- paste0(tempDirec, '/horizons.fst')
  ltab <- paste0(tempDirec, '/labs.fst')
  
  write_fst(allhors, htab, compress = 0)
  write_fst(alllabs, ltab, compress = 0)
  
    numcpus <- detectCores()-2

    chks <- getChunkInfo(chunkLines=numcpus*100, tot=nrow(allhors))
    
    cl <- makeSOCKcluster(numcpus)
    registerDoSNOW(cl)
    pb <- txtProgressBar(initial=1, min=1, max=chks$chunks, style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    result <- foreach(m=1:chks$chunks, .export = c('joinSamplesToHorizonsWorker'), .packages = c('fst'), .options.snow=opts)  %dopar% { joinSamplesToHorizonsWorker(inHorizons=htab, inSamples=ltab, chunks=chks, tempD = tempDirec )  }
    close(pb)
    stopCluster(cl)
    gc()
    
    outDF = as.data.frame(data.table::rbindlist(result, fill=T))
    
    fls <- list.files(tempDirec, full.names = T)
    unlink(fls)
    unlink(tempDirec, recursive = T)

    return(outDF)
}




joinSamplesToHorizonsWorker <- function(inHorizons, inSamples, keepAllHorizons=T, chunks, tempD){

  allhors <- read_fst(inHorizons)
  alllabs <- read_fst(inSamples)
  
  start = chunks$starts[m]
  finish = chunks$starts[m] + (chunks$nlines[m] - 1)
  
  allhors=allhors[start:finish,]
  alllabs=alllabs
  
  
obs <- unique(allhors[c("DataStore", "Dataset", "Provider",  "Location_ID")]) # "Longitude", "Latitude",


# outDF <- data.frame(DataStore=character(),Dataset=character(),Provider=character(),Location_ID=character(),Layer_ID=character()
#                     ,SampleID=character(),SampleDate=character(),Longitude=numeric(),Latitude=numeric(),UpperDepth=numeric()
#                     ,LowerDepth=numeric(),PropertyType=character(),ObservedProperty=character(),Value=character()
#                     ,Units=character(),QualCollection=numeric(),QualSpatialAggregation=numeric(),QualManagement=numeric()
#                     ,QualSpatialAccuracy=numeric(),ExtractTime=character(), midpoint = numeric(), sampUpperDepth=numeric(), sampLowerDepth=numeric(), labValue=numeric())

# outDF <- data.frame(DataStore=character(),Dataset=character(),Provider=character(),Location_ID=character(),Layer_ID=character()
#                     ,SampleID=character(),SampleDate=character(),Longitude=numeric(),Latitude=numeric(),UpperDepth=numeric(),LowerDepth=numeric(), 
#                     RawHor=character(), HorGrp=character(), 
#                     midpoint = numeric(), sampUpperDepth=numeric(), sampLowerDepth=numeric(), 
#                     Sand=numeric(), clay=numeric(), Silt=numeric(), BlkDen=numeric(),  SOC=numeric())

outDF <- data.frame(DataStore=character(),Dataset=character(),Provider=character(),Location_ID=character(),Layer_ID=character()
                    ,SampleID=character(),SampleDate=character(),Longitude=numeric(),Latitude=numeric(),UpperDepth=numeric(),LowerDepth=numeric(), 
                    RawHor=character(), HorGrp=character(), 
                    midpoint = numeric(), sampUpperDepth=numeric(), sampLowerDepth=numeric(), 
                    SoC=numeric())


for (i in 1:nrow(obs)) {

  obsR <- obs[i,]
  horizons <- allhors[allhors$DataStore==obsR$DataStore[1] & allhors$Dataset==obsR$Dataset[1] & allhors$Provider==obsR$Provider[1] & allhors$Location_ID==obsR$Location_ID[1],]
  samples <- alllabs[alllabs$DataStore==obsR$DataStore[1] & alllabs$Dataset==obsR$Dataset[1] & alllabs$Provider==obsR$Provider[1] & alllabs$Location_ID==obsR$Location_ID[1],]
  if(nrow(samples) > 0){
    
    for (j in 1:nrow(horizons)) {
      horizon <- horizons[j,] 
      for (k in 1:nrow(samples)) {

        sample <- samples[k,]
        SampleMidPoint = sample$UpperDepth + ((sample$LowerDepth - sample$UpperDepth)/2)
        
        if(SampleMidPoint >= horizon$UpperDepth & SampleMidPoint <= horizon$LowerDepth){
        # outDF[nrow(outDF)+1,] = c(horizon$DataStore,horizon$Dataset,horizon$Provider,horizon$Location_ID,horizon$Layer_ID
        #                             ,horizon$SampleID,horizon$SampleDate,horizon$Longitude,horizon$Latitude,horizon$UpperDepth
        #                             ,horizon$LowerDepth,horizon$PropertyType,horizon$ObservedProperty,horizon$Value
        #                             ,horizon$Units,horizon$QualCollection,horizon$QualSpatialAggregation,horizon$QualManagement
        #                             ,horizon$QualSpatialAccuracy,horizon$ExtractTime, SampleMidPoint, sample$UpperDepth, sample$LowerDepth, sample$Value)
        # 
          # outDF[nrow(outDF)+1,] = c(horizon$DataStore,horizon$Dataset,horizon$Provider,horizon$Location_ID,horizon$Layer_ID
          #                           ,horizon$SampleID,horizon$SampleDate,horizon$Longitude,horizon$Latitude,horizon$UpperDepth, horizon$LowerDepth,
          #                           horizon$RawHor,horizon$HorGrp,
          #                           SampleMidPoint, sample$UpperDepth, sample$LowerDepth, 
          #                           sample$Sand,sample$clay,sample$Silt,sample$BlkDen,sample$SOC)
          
          outDF[nrow(outDF)+1,] = c(horizon$DataStore,horizon$Dataset,horizon$Provider,horizon$Location_ID,horizon$Layer_ID
                                    ,horizon$SampleID,horizon$SampleDate,horizon$Longitude,horizon$Latitude,horizon$UpperDepth, horizon$LowerDepth,
                                    horizon$RawHor,horizon$HorGrp,
                                    SampleMidPoint, sample$UpperDepth, sample$LowerDepth, 
                                    sample$Value)
        }else{
            
          }
      }
    }
  }
}

if(nrow(outDF)>0){
  write_fst(outDF, paste0(tempD, '/m_', m, '.fst'), compress = 0)
}

return(outDF)

}
