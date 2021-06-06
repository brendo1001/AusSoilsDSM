library(mpspline2)
library(dplyr)

workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'

indf <- read.csv(paste0(workDir, '/SDF_All_Props_Clean_Fill_6.csv'),stringsAsFactors = F)
nrow(indf)
head(indf)

Vol.DUL=48.98-(21.86*indf$FinalBD)+(0.36*indf$clay)-(0.06*indf$Sand)-(0.19*sqrt(indf$Silt))+(2.90*sqrt(indf$FinalSOC))
hist(Vol.DUL)
summary(Vol.DUL)

Vol.DLL=17.40-(10.05*indf$FinalBD)+(0.34*indf$clay)-(0.02*indf$Sand)+(0.18*indf$Silt)
hist(Vol.DLL)
summary(Vol.DLL)

indf2 <- cbind(indf, Vol.DLL, Vol.DUL)
head(indf2)


# Clean up some dodginess due to PTFS
idxs <- which(indf2$Vol.DLL<0 | indf2$Vol.DUL<0)  ##PTF seems to be no good at v high BDs
#write.csv(indf2[idxs,], 'c:/temp/vols.csv')

indf3 <- indf2[-idxs,]

idxs <-  which(indf3$Vol.DLL + 3 > indf3$Vol.DUL)
indf4 <- indf3[-idxs,]

# sort the data frams for splining

odf4 <- indf4[with(indf4, order(DataStore, Dataset, Location_ID, Longitude, Latitude, sampUpperDepth, sampLowerDepth)), ]
head(odf4, 20)

write.csv(odf4, paste0(workDir, '/SDF_All_Props_Clean_With_PTFS_Ordered.csv'), row.names = F)



##### Spline the data

df <- read.csv(paste0(workDir, '/SDF_All_Props_Clean_With_PTFS_Ordered.csv'), stringsAsFactors = F)

sdf <- data.frame(SID=paste0(df$DataStore, '_', df$Dataset, '_', df$Provider, '_', df$Location_ID), Longitude=df$Longitude, Latitude=df$Latitude, 
                  UD=df$sampUpperDepth*100, LD=df$sampLowerDepth*100, DLL=df$Vol.DLL, DUL=df$Vol.DUL)
str(sdf)

which(duplicated(sdf[, c(1:3)]))
idxs<- which(sdf$SID == 'ASRIS_NTGovernment_NTGovernment_704_CPRS59_L.118_1')
idxs<- which(sdf$SID == 'ASRIS_NTGovernment_NTGovernment_704_STURT_126_1')


sdf[idxs,]
sdf[43113, ]$UD <- 24

idxs<- which(sdf$SID == 'ASRIS_WAGovernment_WAGovernment_501_KLC_0069_1')
idxs<- which(sdf$SID == 'ASRIS_WAGovernment_WAGovernment_501_KLC_0069_1')
sdf <- sdf[-43113, ]
sdf <- sdf[-43116, ]

##   get the mean for any duplicated horizons - the spline doesn't deal with duplicates
sdf2 <- ddply(sdf, .(SID, Longitude, Latitude, UD, LD), summarize, DLL=mean(DLL), DUL=mean(DUL))

### Run the splines
locs <- unique(sdf2$SID)
length(locs)

outdf <- data.frame()
ecnt=0

for (i in 1:length(locs)) {
  print(i)
  loc <- locs[i]
  s <- sdf2[sdf2$SID==loc,]
  
  #### Fix up overlapping horizons
  if(nrow(s) > 1){
    for (k in 2:nrow(s)) {
      
      if(s$LD[k-1] > s$UD[k]){
        s$UD[k] = s$LD[k-1]
      }
      if(s$LD[k-1] > s$UD[k]){
        s$UD[k] = s$LD[k-1]
      }
    }
  }
  
  if(nrow(s)==1)
  {
    print(i)
    odf <- data.frame( SID=sDUL[[1]][1],
                Latitude=s$Latitude[1], 
                Longitude=s$Longitude[1],
                nrow=as.numeric(nrow(s)),
                MaxDep <- s$LD[nrow(s)],
                DLL_005=NA,DLL_015=NA, DLL_030=NA, DLL_060=NA,DLL_100=NA,DLL_200=NA ,
                DUL_005=NA,DUL_015=NA,DUL_030=NA,DUL_060=NA,DUL_100=NA,DUL_200=NA
    )
    SampleMidPoint = s$UD[1] + ((s$LD[1] - s$UD[1])/2)
    if( SampleMidPoint > 0 & SampleMidPoint <= 5){odf$DLL_005=s$DLL[1];odf$DUL_005=s$DUL[1]}
    if( SampleMidPoint > 5 & SampleMidPoint <= 15){odf$DLL_015=s$DLL[1];odf$DUL_015=s$DUL[1]}
    if( SampleMidPoint > 15 & SampleMidPoint <= 30){odf$DLL_030=s$DLL[1];odf$DUL_030=s$DUL[1]}
    if( SampleMidPoint > 30 & SampleMidPoint <= 60){odf$DLL_060=s$DLL[1];odf$DUL_060=s$DUL[1]}
    if( SampleMidPoint > 60 & SampleMidPoint <= 100){odf$DLL_100=s$DLL[1];odf$DUL_100=s$DUL[1]}
    if( SampleMidPoint > 100){odf$DLL_200=s$DLL[1];odf$DUL_200=s$DUL[1]}
    colnames(odf)[5] <- 'MaxDepth'
    outdf=rbind(outdf, odf)
  }
  # else if(nrow(s)==2){}
  else{
    
    tryCatch({
    sDLL <- mpspline(obj = s[, c(1,4:7)], var_name = 'DLL')
    sDUL <- mpspline(obj = s[, c(1,4:7)], var_name = 'DUL')
    
    odf <- data.frame( SID=sDUL[[1]][1],
                       Latitude=s$Latitude[1], 
                       Longitude=s$Longitude[1],
                       nrow=nrow(s),
                       maxepth <- s$LD[nrow(s)],
                       DLL_005=sDLL[[1]][4][[1]][1],
                       DLL_015=sDLL[[1]][4][[1]][2],
                       DLL_030=sDLL[[1]][4][[1]][3],
                       DLL_060=sDLL[[1]][4][[1]][4],
                       DLL_100=sDLL[[1]][4][[1]][5],
                       DLL_200=sDLL[[1]][4][[1]][6] ,
                       
                       DUL_005=sDUL[[1]][4][[1]][1],
                       DUL_015=sDUL[[1]][4][[1]][2],
                       DUL_030=sDUL[[1]][4][[1]][3],
                       DUL_060=sDUL[[1]][4][[1]][4],
                       DUL_100=sDUL[[1]][4][[1]][5],
                       DUL_200=sDUL[[1]][4][[1]][6]
                     )
    colnames(odf)[5] <- 'MaxDepth'
    outdf=rbind(outdf, odf)
    
    
  }, error = function(e) {
    # comment out the next print statement for a silent error
    print(e)
    ecnt=ecnt+1
  })
}
}


print(ecnt)

write.csv(outdf, paste0(workDir, '/SDF_All_Props_Clean_With_PTFS_Splined.csv'), row.names = F)
