
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

#scriptsPath <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts'
ident = 'sea084'
debugPath <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCout'




workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/AWC'
jobName='mosaic90mTiles'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:00:00', memoryGB='4GB', jobStartIteration=1, jobEndIteration=12, debugPath=debugPath,jobFileName=jobFileName, deleteDebugFiles=T)

jobID<-'47108136'

jobID<-'47100141'
jobID<-'47099846'

showDebugFile(jobName = jobName, type='error', iteration = 1)
showDebugFile(jobName = jobName, type='out', iteration = 1)

monitorJob(jobID) 
showCPUs()
showQ(ident)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
cancelJob(jobID)
showJobLog()
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo('sea084', 10, 'RUNNING')
showJobInfo('sea084', 10, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')
