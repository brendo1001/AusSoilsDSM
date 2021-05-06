
#################################
###  Author : Ross Searle         
###  Date : Mon Dec 14 10:46:20 2020                      
###  Project :  
###  Purpose : this is just a demo script to demonstrate the HPC control scripts
#################################
library(raster)
rasterOptions(progress = 'text', memfrac = 0.5)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  slgaRoot <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FPAR'
}else{
  slgaRoot <- '/datasets/work/af-digiscapesm/work/Ross/SLGAData/FPAR'
}


args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
arg2 <- as.numeric(args[2])
arg3 <- as.numeric(args[3])

print(paste0("Processing HPC iteraration = ", k))


for (i in 1:100) {
  print(paste0('iteration ', i))
  Sys.sleep(10)
  
}


print(paste0('Finished Successfully'))
