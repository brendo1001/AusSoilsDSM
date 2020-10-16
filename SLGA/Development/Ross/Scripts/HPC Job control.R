
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


workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC'

jobName='mosRFMaps90mTiles'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:00:00', memoryGB='8GB', jobStartIteration=1, jobEndIteration=1, debugPath=debugPath,arguments='', deleteDebugFiles=T)


jobName='mosLinMaps90mTiles'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:00:00', memoryGB='8GB', jobStartIteration=1, jobEndIteration=12, debugPath=debugPath, arguments='', deleteDebugFiles=T)

workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/General'
jobName='drillCovsHPC'
args <- paste0('/datasets/work/af-digiscapesm/work/Ross/SLGAData/TileDrill/ASCs.csv o_longitude_GDA94 o_latitude_GDA94 o_asc_ord SID')
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:10:00', memoryGB='8GB', jobStartIteration=1580, jobEndIteration=1580, debugPath=debugPath, arguments=args, deleteDebugFiles=T)

monitorJob(jobID, debugPath) 
showCPUs(ident=ident)
showCPUs(jobID=jobID)
showQ(ident)
showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
showNonSuccessfullJobs(jobName, debugPath)

showJobLog(debugPath)
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo('sea084', 10, 'RUNNING')
showJobInfo('sea084', 10, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')

showDebugFile(jobName = jobName, type='error', iteration = 1580)
showDebugFile(jobName = jobName, type='out', iteration = 1580)

cancelJob(jobID)
cancelJob('47124435')

jobID<-'47124728'

showAllUsers()
