
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
#workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts'
workingDir<- '/datasets/work/lw-slga/work/Projects/Roper/Scripts'
args=''

jobName='DemoRscript'
args='ASCDemo asc.mod.2.rds 20'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:10:00', memoryGB='1GB', jobStartIteration=1, jobEndIteration=10, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


jobName='doPredictionsDemo'
args='ASCDemo asc.mod.2.rds 20'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:10:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=218, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)
 
showQ(ident)

monitorJob(jobID, debugPath)
showCPUs2(ident)
showCPUs(ident=ident)
#showCPUs(jobID=jobID)

showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
nsj <- showNonSuccessfullJobs(jobName, debugPath)

tail(showJobLog(debugPath), 100)
showFailedJobs(jobID)
showFailedJobNos(jobID)

showJobInfo('sea084', 100, 'ALL')
showJobInfo('sea084', 30, 'RUNNING')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration =5)
showDebugFile(jobName = jobName, type='out', iteration = 165)

cancelJob(jobID)
cancelJob('47785301')


showAllUsers()
HPCLoad()





