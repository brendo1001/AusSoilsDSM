
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
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='03:00:00', memoryGB='5GB', jobStartIteration=1, jobEndIteration=10, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


jobName='make30mTiles'
#3672
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='05:00:00', memoryGB='5GB', jobStartIteration=1, jobEndIteration=300, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)




# jobID <- '47682289'
# jobID <- '47566098'
#jobID <- '47669788' # processing

monitorJob(jobID, debugPath) 
showCPUs(ident=ident)
showCPUs(jobID=jobID)
showQ(ident)
showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
showNonSuccessfullJobs(jobName, debugPath)

tail(showJobLog(debugPath), 10)
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo('sea084', 30, 'RUNNING')
showJobInfo('sea084', 15, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration = 1)
showDebugFile(jobName = jobName, type='out', iteration = 1)



cancelJob(jobID)
 cancelJob('47501388')

jobID<-'47159401'

showAllUsers()
HPCLoad()
