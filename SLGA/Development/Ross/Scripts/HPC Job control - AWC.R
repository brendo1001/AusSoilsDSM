
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

debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/APSOIL_RF'
rootDir2 <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRF'


#jobEndIteration=2040

att='DUL'
att='LL15'
depth=0
depth=5
depth=15
depth=30
depth=60
depth=100

jobName='doPredictionsBootstrap'
args=paste0(att, ' ', depth, ' 20 F')
print(args)
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='04:00:00', memoryGB='16GB', jobStartIteration=1, jobEndIteration=2040, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

#####  fills in missing files after run above if neeeds be
jobName='doPredictionsBootstrap'
chks<- 2040
args=paste0(att, ' ', depth, ' 20 F')
print(args)
pb <- txtProgressBar(min=0, max=chks, style=3)
for (i in 1:chks) {
  setTxtProgressBar(pb, i)
  f <- paste0(rootDir2, '/Maps/', att, '_', depth, '/Chunks/AllCellVals_', i, '.rds')
  if(!file.exists(f)){
    print(paste0('Missing - ', f))
    jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='04:00:00', memoryGB='16GB', jobStartIteration=i, jobEndIteration=i, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
    }
}
close(pb)

showJobInfo('sea084', 10, 'ALL')
showVerboseJobInfo('sea084',debugPath, 10, 'ALL')
showVerboseJobInfo('sea084',debugPath, 10, 'ALL', fromTime = '2021-05-05T08:38:00')

showVerboseJobInfo('sea084',debugPath, 300, 'RUNNING')

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

showFailedJobs('52188662')

showNonCompletedJobs('52217485')

showJobLog(debugPath)
tail(showJobLog(debugPath), 100)
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo('sea084', 30, 'RUNNING')
showJobInfo('sea084', 10, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration =1)
showDebugFile(jobName = jobName, type='out', iteration = 1018)


jobID='47738601'
cancelJob('47785301')

 cancelJob(jobID)

jobID<-'47159401'

head(showAllUsers(), 10)
HPCLoad()
