

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
 # basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/90m'
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tiles'
 # mapping <- read.csv('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/90mfls.csv', stringsAsFactors = F)
  mapping <- read.csv('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tile90Names.csv', stringsAsFactors = F)
}else{
 # basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
  basePath <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles'
  mapping <- read.csv('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90mfls.csv', stringsAsFactors = F)
}




args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
#att = args[2]
print(paste0("Processing iteraration = ", k))


# sq <- seq(1, 20041,50)
# ivl <- sq[k]
# 
# root.tiles <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles_25m'
# 
# mapping <- read.csv('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/names.csv', stringsAsFactors = F)
# fols<-read.csv('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/Tilenames.csv')


# for (j in 1:nrow(mapping)) {
#   inf <- paste0( fname = paste0(basePath, '/', mapping[j,1]))
#   outf <- paste0( fname = paste0(basePath, '/', mapping[j,2]))
#   if(inf!=outf){
#     if(file.exists(inf)){
#       res<-file.rename(inf,outf)
#     }
#   }
# }


#### rename 90m tiles

basePath <- 'M:/work/datasets/national/covariates/mosaics/90m'
dirs <- list.files(basePath, recursive = F, full.names = F)
mapping <- read.csv('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tile90Names.csv', stringsAsFactors = F)


  for (j in 1:nrow(mapping)) {
    inf <- paste0( fname = paste0(basePath, '/', mapping[j,1]))
    outf <- paste0( fname = paste0(basePath, '/', mapping[j,2]))
    if(inf!=outf){
      if(file.exists(inf)){
        res<-file.rename(inf,outf)
      }
    }
  }



print(paste0('Finished Successfully'))




#rename 90m mosaics



