
#################################
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:20:36 2020                      
###  Project : SLGA TERN Landscapes
###  Purpose : Functions to submit and monitor jobs on the Pearcey HPC
###            At the moment the script that access these functions id "HPC Job Control.R"
#################################

library(raster)
library(raster)
library(stringr)
library(rgdal)
library(stringr)
library(dplyr)


showJobLog <- function(debugPath){
  f <- paste0(debugPath, '/JobLog.csv')
  if(!file.exists(f)){
    return(paste0('Job log does not exist - ', f))
  }else{
    jobLog = read.csv(file=f, stringsAsFactors = F)
    return(jobLog)
  }
}


getJobLog <- function(debugPath){
  f <- paste0(debugPath, '/JobLog.csv')
  if(!file.exists(f)){
    jobLog <- data.frame(jobID=character(), jobName=character(), startTime=character(), startIt=numeric(), endIt=numeric(), stringsAsFactors = F)
  }else{
    jobLog = read.csv(file=f, stringsAsFactors = F)
  }
  return(jobLog)
}

writeJobLog <- function(jobID, jobName, startTime, startIt, endIt, debugPath){
  jobLog <- getJobLog(debugPath)
  jobLog[nrow(jobLog)+1, ] <- c(jobID, jobName, startTime, startIt, endIt )
  f <- paste0(debugPath, '/JobLog.csv')
  write.csv(jobLog, file = f, row.names = F )
}

DeleteDebugFiles <- function(debugFilesPath){
  fls <- list.files(paste0(debugFilesPath), recursive = T, full.names = T)
  unlink(fls)
  print(paste0('Deleted ', length(fls), ' from ', debugFilesPath))
}



sendJob <- function(jobName, att, depth, workingDir, wallTime, memoryGB, jobStartIteration, jobEndIteration, limit='', debugPath, arguments='', deleteDebugFiles=T){
  
  jobFileName <- paste0(workingDir,'/', jobName, '.sh')
  debugFilesPath <- paste0(debugPath,'/', jobName, '_', att, '_', depth)
  makeJobFile(jobName, workingDir, arguments)
  if(deleteDebugFiles){DeleteDebugFiles(debugFilesPath = debugFilesPath)}
  if(!dir.exists(paste0(debugFilesPath))){dir.create(paste0(debugFilesPath), recursive = T)}
  of <- paste0(debugPath, '/', jobName, '_', att, '_', depth, '/', jobName, '_', att, '_', depth, '_out_%a.txt')
  ef <- paste0(debugPath, '/', jobName, '_', att, '_', depth, '/', jobName, '_', att, '_', depth, '_error_%a.txt')
  job <- paste0('sbatch --parsable --job-name=', paste0(att, depth, jobName),' --time=', wallTime, ' --mem=', memoryGB,' -a ', jobStartIteration,'-',jobEndIteration,limit, ' -o ', of,' -e ', ef, ' ', jobFileName)
  jobID <- system(job, intern = T)
  writeJobLog(jobID, paste0(jobName,  '_', att, '_', depth), format(Sys.time(), '%A, %B %d, %Y %H:%M:%S'), jobStartIteration, jobEndIteration, debugPath)
  print(jobID)
  
  return(jobID)
}



sendJob2 <- function(jobName, workingDir, wallTime, memoryGB, jobStartIteration, jobEndIteration, limit='', debugPath, arguments='', deleteDebugFiles=T){
  
  jobFileName <- paste0(workingDir,'/', jobName, '.sh')
  debugFilesPath <- paste0(debugPath,'/', jobName)
  makeJobFile(jobName, workingDir, arguments)
  if(deleteDebugFiles){DeleteDebugFiles(debugFilesPath = debugFilesPath)}
  if(!dir.exists(paste0(debugFilesPath))){dir.create(paste0(debugFilesPath), recursive = T)}
  job <- paste0('sbatch --parsable -N 1 --ntasks 1 --exclusive --job-name=', paste0(jobName),' --time=', wallTime, ' --mem=', memoryGB,' -a ', jobStartIteration,'-',jobEndIteration,limit, ' -o ', debugPath, '/', jobName, '/', jobName, '_out_%a.txt -e ', debugPath, '/', jobName, '/', jobName, '_error_%a.txt ', jobFileName)
  jobID <- system(job, intern = T)
  writeJobLog(jobID, jobName, format(Sys.time(), '%A, %B %d, %Y %H:%M:%S'), jobStartIteration, jobEndIteration, debugPath)
  print(jobID)
  
  return(jobID)
}

makeJobFile <- function( jobName=NULL, workingDir= '', args=''){
  sfile <-  paste0(workingDir ,'/', jobName,'.sh')
  cat('#!/bin/sh\n', file = sfile)
  cat('module load R/3.6.1\n', file = sfile, append = T)
  cat(paste0('/apps/R/3.6.1/lib64/R/bin/Rscript ', workingDir ,'/', jobName,'.R $SLURM_ARRAY_TASK_ID ', args), file=sfile, append = T)
  system(paste0('chmod 755 ', sfile) )
}


showDebugFile<-function(jobName, type, iteration){
  
  f <- paste0(debugPath, '/', jobName, '_', att, '_', depth,  '/', jobName, '_', att, '_', depth, '_', type, '_', iteration, '.txt')
  if(!file.exists(f))
  {
    print(paste0('File does not exist - ', f))
  }else{
    d <- readLines(f)
    d <- paste0(d, '\n')
    cat(d)
  }
}



monitorJob <- function(jobID, debugPath){
  f='go'
  jl <- getJobLog(debugPath)
  jrec <- jl[jl$jobID==jobID,]
  tsks <- (jrec$endIt+1) - jrec$startIt
  while (f != 'stop'){
    #cpus <- system('scc', intern = T)
    d <- system(paste0('sacct -j ', jobID), intern = T)
    d2 <- d[which(grepl('h2', d))]
    completed <- which(grepl('COMPLETED', d2))
    running <- which(grepl('RUNNING', d2))
    msg <- paste0('There are ', length(running),  ' CPUs in use :  ', length(completed), ' of ', tsks, ' tasks completed')
    cat("\r", "")
    cat("\r", msg)
    Sys.sleep(10)
  }
}



showDetails <- function(jobID){
  d <- system(paste0('sacct -j ', jobID, ' -P -X -o jobid%15,jobname,user,start,end,state%30,reqmem,node'), intern = T)
  #d2 <- d[which(grepl('h2', d))]
  d3 <-str_trim( gsub("\\s+", " ", d))
  df <- read.csv(text=d3, sep = '|', header = F)
  #df2 <- df[, c(1,2,4,7,8,9,10,11)]
  colnames(df) <- c('JobID', 'JobName', 'User', 'Time1', 'Time2', 'Status', 'Memory', 'Node')
  
  return(df)
}

showFailedJobNos <- function(jobID){
  d <- system(paste( 'sacct -j ', jobID,  ' -P -X -o jobid%15,start,end,state%30'), intern = T)
  df <- read.csv(text=d, sep = '|', header = T, stringsAsFactors = F)
  df2 <- df[which(grepl(pattern = 'FAILED', df$State)),]
  bits <- str_split( df2$JobID, '_')
  numst <- sapply(bits, function(x) x[2])
  nums <- as.numeric(numst)
  return(nums)
}

showNonSuccessfullJobs <- function(jobName, debugPath){
  
  fls <- list.files(paste0(debugPath, '/', jobName), full.names = T)
  infls <- fls[which(grepl('_out_', fls))]
  
  ol <- vector(mode = 'character', length = length(infls))
  ol[]<-NA
  for (i in 1:length(infls)) {
    f <- infls[i]
    d <- readLines(f)
    cnt <- which(grepl('Finished Successfully', d))
    if(length(cnt)==0){
      ol[i] <- f
    }
  }
  return(as.character(na.omit(ol)))
}



showCPUs <- function(jobID=NULL, ident=NULL){
  if(is.null(jobID)){
    d <- system(paste0('sacct -u ', ident), intern = T)
  }else{
    d <- system(paste0('sacct -j ', jobID), intern = T)
  }
  
  d2 <- d[which(grepl('h2', d))]
  running <- which(grepl('RUNNING', d2))
  paste0('There are ', length(running), ' CPUs in use')
}

showCPUs2<-function(ident){
  d <- system (paste0('squeue -u ', ident), intern = T)
  running <- which(grepl('R', d))
  paste0('There are ', length(running), ' CPUs in use')
}

showQ <- function(ident){
  system (paste0('squeue -u ', ident))
}

showQforJob <- function(jobID){
  system (paste0('squeue -j ', jobID))
}


showNonCompletedJobs <- function(jobID){
  r <- showDetails(jobID)
  r2 <- r[which(!grepl('COMPLETED', r$Status)), ]
  return(r2)
}

showFailedJobs<- function(jobID){
  r <- showDetails(jobID)
  r2 <- r[which(grepl('FAILED', r$Status)), ]
  return(r2)
}

cancelJob <- function(jobID){
  system (paste0('scancel ', jobID))
}






showJobInfo <- function(ident, tnum=10, filter="ALL", fromTime=''){
  
  if(nchar(fromTime)>0 ){fromTime = paste0(' -S ', fromTime, ' ') }
  # limit returned records to date  -S 2020-07-28T10:00:00
  d <- system(paste( 'sacct -u ', ident, fromTime, ' -P -X -o jobid,start,end,state%30'), intern = T)
  df <- read.csv(text=d, sep = '|', header = T, stringsAsFactors = F)
  
  allids <- str_split(df$JobID, '_')
  alljobids <- as.character(sapply(allids, function(x) x[1]))
  alltaskids <- as.character(sapply(allids, function(x) x[2]))
  df4 <- df[, -1]
  df3 <- data.frame(JobID=alljobids, TaskID=alltaskids, stringsAsFactors = F)
  df2 <- cbind(df3, df4)
  
  jobIDs <- unique(df2$JobID)
  jidsToDo <- tail(jobIDs, tnum)
  
  sdf <-  data.frame(JOBID=character(), PENDING=character(), COMPLETED=character(), FAILED=character(), 
                     RUNNING=character(), CANCELLED=character(), TIMEOUT=character(),OUT_OF_MEMORY=character(),stringsAsFactors = F)
  
  
  for (i in 1:length(jidsToDo)) {
    
    jid <- jidsToDo[i]
    jobfil <- df2[which(grepl(jid, df2$JobID)),]
    cnts <- jobfil %>% group_by(State) %>% tally()
    
    #cnts <- as.data.frame(jobfil %>% count(State))
    p <- cnts[cnts$State=='PENDING',2]
    if(nrow(p)==0){p=0}
    c1 <- cnts[cnts$State=='COMPLETED',2]
    if(nrow(c1)==0){c1=0}
    f <- cnts[cnts$State=='FAILED',2]
    if(nrow(f)==0){f=0}
    r <- cnts[cnts$State=='RUNNING',2]
    if(nrow(r)==0){r=0}
    c2 <- cnts[str_detect(cnts$State, 'CANCELLED'),2]
    if(nrow(c2)==0){c2=0}
    t <- cnts[str_detect(cnts$State, 'TIMEOUT'),2]
    if(nrow(t)==0){t=0}
    m <- cnts[str_detect(cnts$State, 'OUT_OF_MEMORY'),2]
    if(nrow(m)==0){m=0}
    sdf <- sdf %>% add_row(JOBID=jid, PENDING=p, COMPLETED=c1, FAILED=f, RUNNING=r, CANCELLED=c2, TIMEOUT=t,OUT_OF_MEMORY=m )
  }
  
  # jl <- showJobLog(tnum)
  # 
  # mdf <- merge(sdf, jl, by.x = 'JOBID', by.y = 'JobID')
  mdf <- sdf
  of <- str_to_upper(filter)
  if(!filter=='ALL'){
    odf3 <- mdf[which(mdf[of] > 0),]
    return(odf3)
  }else{
    return(mdf)
  }
  
  return(mdf)
}

showVerboseJobInfo <- function(ident, debugPath='', tnum=10, filter="ALL", fromTime=''){
  
  if(nchar(fromTime)>0 ){fromTime = paste0(' -S ', fromTime, ' ') }
  # limit returned records to date  -S 2020-07-28T10:00:00
  d <- system(paste( 'sacct -u ', ident, fromTime, ' -P -X -o jobid,start,end,state%30'), intern = T)
  df <- read.csv(text=d, sep = '|', header = T, stringsAsFactors = F)
  
  allids <- str_split(df$JobID, '_')
  alljobids <- as.character(sapply(allids, function(x) x[1]))
  alltaskids <- as.character(sapply(allids, function(x) x[2]))
  df4 <- df[, -1]
  df3 <- data.frame(JobID=alljobids, TaskID=alltaskids, stringsAsFactors = F)
  df2 <- cbind(df3, df4)
  
  jobIDs <- unique(df2$JobID)
  jidsToDo <- tail(jobIDs, tnum)
  
  sdf <-  data.frame(JOBID=character(), PENDING=character(), COMPLETED=character(), FAILED=character(), 
                     RUNNING=character(), CANCELLED=character(), TIMEOUT=character(),OUT_OF_MEMORY=character(),stringsAsFactors = F)
  
  
  for (i in 1:length(jidsToDo)) {
    
    #showJobLog(debugPath)
    
    jid <- jidsToDo[i]
    jobfil <- df2[which(grepl(jid, df2$JobID)),]
    cnts <- jobfil %>% group_by(State) %>% tally()
    
    #cnts <- as.data.frame(jobfil %>% count(State))
    p <- cnts[cnts$State=='PENDING',2]
    if(nrow(p)==0){p=0}
    c1 <- cnts[cnts$State=='COMPLETED',2]
    if(nrow(c1)==0){c1=0}
    f <- cnts[cnts$State=='FAILED',2]
    if(nrow(f)==0){f=0}
    r <- cnts[cnts$State=='RUNNING',2]
    if(nrow(r)==0){r=0}
    c2 <- cnts[str_detect(cnts$State, 'CANCELLED'),2]
    if(nrow(c2)==0){c2=0}
    t <- cnts[str_detect(cnts$State, 'TIMEOUT'),2]
    if(nrow(t)==0){t=0}
    m <- cnts[str_detect(cnts$State, 'OUT_OF_MEMORY'),2]
    if(nrow(m)==0){m=0}
    sdf <- sdf %>% add_row(JOBID=jid, PENDING=p, COMPLETED=c1, FAILED=f, RUNNING=r, CANCELLED=c2, TIMEOUT=t,OUT_OF_MEMORY=m )
  }
  
  # jl <- showJobLog(tnum)
  # 
  # mdf <- merge(sdf, jl, by.x = 'JOBID', by.y = 'JobID')
  mdf <- sdf
  of <- str_to_upper(filter)
  
  jl <- showJobLog(debugPath)
  
  
  
  if(!filter=='ALL'){
    odfv <- merge(jl, mdf, by.x='jobID', by.y='JOBID')
    odf3 <- odfv[which(odfv[of] > 0),]
    return(odf3)
  }else{
    odfv <- merge(jl, mdf, by.x='jobID', by.y='JOBID')
    return(odfv)
  }
  
  
  #return(mdf)
}


# 
# countOutputFiles <- function(outPath, type, att, numUses, numFiles){
# 
#     inDir <- paste0(outPath, '/', type, '/', att)
#     dirs <- list.dirs(inDir, full.names = F, recursive = F)
#     
#     fCount <- vector(mode = 'numeric', numUses)
#     subgrp <- vector(mode = 'character', numUses)
#     complete <- vector(mode = 'logical', numUses)
#    
#       for(j in 1:length(dirs)){
#         cat('=',sep='')
#         d <- dirs[j]
#         subDir <- paste0(inDir, '/', d)
#         fls <- list.files(subDir, '*.fst', recursive = T, full.names = T)
#         #print(paste0(d, ' = ' ,length(fls)))
#         fCount[j] = length(fls)
#         subgrp[j] = d
#         complete[j] = (length(fls)==numFiles)
#       }
#     cat('\n',sep='')
#     df <- data.frame(att, subgrp, fCount, complete, stringsAsFactors = F)
#   return(df)
# }

# taskProcessingTimes <- function(outPath, type, att){
#   
#   inDir <- paste0(outPath, '/', type, '/', att)
#   dirs <- list.dirs(inDir, full.names = F, recursive = F)
#   
#   id <- vector(mode = 'numeric', 608)
#   fCount <- vector(mode = 'numeric', 608)
#   stime <- vector(mode = 'character', 608)
#   etime <- vector(mode = 'character', 608)
#   mins <- vector(mode = 'numeric', 608)
#   
#   fls <- list.files(inDir, '*.fst', recursive = T, full.names = F)
#   
#   for (i in 1:608) {
#     print(i)
#     filt <- paste0('_', i, '.fst')
#     b <- fls[which(grepl(filt, fls))]
#     fi <- file.info(paste0(inDir, '/', fls))
#     dif <- max(fi$mtime) - min(fi$mtime)
#     t <- as.numeric(dif, units='mins')
#     
#     id[i] <- i
#     fCount[i] <- length(fls)
#     stime[i] <- as.character(min(fi$mtime))
#     etime[i] <- as.character(max(fi$mtime))
#     mins[i] <- t
#   }
#   
#   df <- data.frame(att, id, fCount, stime, etime, mins, stringsAsFactors = F)
#   return(df)
# }


# getTimeTaken <- function(outPath, type, att, units){
#   fls <- list.files(paste0(outPath, '/', type, '/', att), '*.fst', recursive = T, full.names = T)
#   fi <- file.info(fls)
#   dif <- max(fi$mtime) - min(fi$mtime)
#   t <- as.numeric(dif, units=units)
#   return(t)
# }

showAllUsers <- function(){
  d <- system(paste0('squeue -o %i,%u'), intern = T)
  
  d3 <-str_trim( gsub("\\s+", " ", d))
  df <- read.csv(text=d3, sep = ',', header = T)
  cnts <- as.data.frame(df %>% count(USER))
  cpus <- sum(cnts$n)
  ocnts <- cnts[order(-cnts$n),]
  return(ocnts)
  
}

HPCLoad <- function(){
  d <- system(paste0('squeue -o %i,%u'), intern = T)
  d3 <-str_trim( gsub("\\s+", " ", d))
  df <- read.csv(text=d3, sep = ',', header = T)
  cnts <- as.data.frame(df %>% count(USER))
  cpus <- sum(cnts$n)
  load <- (cpus/7600) * 100
  p <- sprintf(load, fmt = '%#.2f')
  return(paste0(p, '%'))
  
}
