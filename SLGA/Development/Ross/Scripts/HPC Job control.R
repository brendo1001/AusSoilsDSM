
######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:18:04 2020                      
###  Project : TERN Landscapes
###  Purpose : This script controls and monitors processing on the HPC
###  
###############################################################################################

library(raster)
library(stringr)
library(rgdal)

source(paste0('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCUtils.R'))
ident = 'sea084'
#debugPath <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCout'
debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing'
args=''

jobName='projAusCovTiles'
#args = 'alpsbk_aust_y2009_sd5a2'
#args = 'alpsbk_aust_y2009_se5a2'
#args = 'lztmre_aus_y20002011_dm7a2_d20050630'
#args = 'lztmre_aus_y20002011_dmaa2_d20050630'
args = 'wofs_Log'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='03:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1521, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='renameTiles'
args=''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:30:00', memoryGB='1GB', jobStartIteration=1, jobEndIteration=401, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='maskMosaics'
#102
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='04:30:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=102, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='maskMosaics2'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='04:30:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=102, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='extCropMosaics'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='04:30:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=102, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='resMos'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=102, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


jobName='MaskMos'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=102, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='ausMos4'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='resWofs'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='MaskMos2'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=3, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='projWofs'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='writeRaster'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='2:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='resMos2'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=3, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='setProj'
# 142
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:00:00', memoryGB='7GB', jobStartIteration=41, jobEndIteration=80, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)

jobName='witeRasterChunks'
js <- c(63,64,68,70,75,79)
for (i in 1:6) {
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:30:00', memoryGB='7GB', jobStartIteration=js[i], jobEndIteration=js[i], limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
  Sys.sleep(300)
}


jobName='witeRasterChunks3'

for (i in 1:7) {
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:30:00', memoryGB='7GB', jobStartIteration=i, jobEndIteration=i, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
  Sys.sleep(300)
}


for (i in 15:21) {
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:30:00', memoryGB='7GB', jobStartIteration=i, jobEndIteration=i, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
  Sys.sleep(300)
}

jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='05:30:00', memoryGB='7GB', jobStartIteration=21, jobEndIteration=21, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
#jobID <- sendJob2(jobName=jobName, workingDir=workingDir, wallTime='00:05:00', memoryGB='2GB', jobStartIteration=1, jobEndIteration=10, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
showQ(ident)

jobName='res90m'
#3672
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=13, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='30:00:00', memoryGB='10GB', jobStartIteration=2, jobEndIteration=2, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


jobName='make30mTiles'
#3672
# for (i in 2000 :3000) {
# jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='0:30:00', memoryGB='1GB', jobStartIteration=i, jobEndIteration=i, limit='%25', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
# Sys.sleep(100)
# }

# for (i in 30:31) {
#   jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='0:10:00', memoryGB='63GB', jobStartIteration=((i*100)+1), jobEndIteration=((i+1)*100), limit='%50', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
#   Sys.sleep(300)
# }

#1835#190
# tot = 18359
jobName='make30mTiles2'
#for (i in 10:30) {
for (i in 2:2) {
  print(i)
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='0:25:00', memoryGB='2GB', jobStartIteration=((i*100)+1), jobEndIteration=((i+1)*100), limit='%50', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
  Sys.sleep(150)
}


jobName='make30mTiles4'
for (i in 6:190) {
  print(i)
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='0:10:00', memoryGB='2GB', jobStartIteration=((i*100)+1), jobEndIteration=((i+1)*100), limit='%50', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
  Sys.sleep(180)
}

jobName='longMake30mTiles3'
#for (i in 41:60) {
args<-'10000'
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='63GB', jobStartIteration=1, jobEndIteration=1, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
  #Sys.sleep(600)
#}

# jobName='make30mTiles2'
# for (i in 10:20) {
# jobID <- sendJob2(jobName=jobName, workingDir=workingDir, wallTime='0:25:00', memoryGB='2GB', jobStartIteration=((i*100)+1), jobEndIteration=((i+1)*100), limit='%50', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
# Sys.sleep(600)
# }


# jobID <- '47786598'
# jobID <- '47566098'
#jobID <- '47669788' # processing
  
  
# resample 90m Covariate mosaics 
jobName='res90mCovariates'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:10', memoryGB='10GB', jobStartIteration=1, jobEndIteration=95, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)

showJobInfo('sea084', 10, 'ALL')

monitorJob(jobID, debugPath)
showCPUs2(ident)
showCPUs(ident=ident)
showCPUs(jobID=jobID)
showQ(ident)
showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
nsj <- showNonSuccessfullJobs(jobName, debugPath)

tail(showJobLog(debugPath), 100)
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo('sea084', 30, 'RUNNING')
showJobInfo('sea084', 10, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration =1)
showDebugFile(jobName = jobName, type='out', iteration = 247)


jobID='47738601'
cancelJob('47785301')

 cancelJob(jobID)

jobID<-'47159401'

showAllUsers()
HPCLoad()
