

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
 # basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/90m'
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tiles'
  mapping <- read.csv('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/mosaics/90mfls.csv', stringsAsFactors = F)
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


cat('Dir,Cnt\n', file = 'e:/temp/90mCleaning.csv', append = F)

dirs <- list.dirs(basePath, recursive = F, full.names = F)

#####  Clean up unwanted files and count tifs
for(i in 1:length(dirs)){
    
  
    tile <- dirs[i]
    print(paste0(i, ' of ', length(dirs), ' : ', tile))
    it_fls <- list.files(paste0(basePath, '/', tile),recursive = F, full.names = F, pattern = "^it_")
    r <- unlink(paste0(basePath, '/', tile, '/', it_fls), recursive = T)
    
    if(dir.exists(paste0(basePath, '/', tile, '/depth'))){unlink(paste0(basePath, '/', tile, '/depth'), recursive = T)}
    
    if(file.exists(paste0(basePath, '/', tile, '/.Rhistory'))){unlink(paste0(basePath, '/', tile, '/.Rhistory'), recursive = F)}
    
    sq_fls <- list.files(paste0(basePath, '/', tile),recursive = F, full.names = F, pattern = "^t1_square_")
    r <- unlink(paste0(basePath, '/', tile, '/', sq_fls), recursive = F)
    
    
    all_fls <- list.files(paste0(basePath, '/', tile),recursive = F, full.names = F)
    cat(paste0(tile, ',', length(all_fls), '\n'), file = 'e:/temp/90mCleaning.csv', append = T)
}

#   print(dirs[i])
#   
#   sfol <- dirs[i]
#   
#   for (j in 1:nrow(mapping)) {
#     inf <- paste0( fname = paste0(basePath, '/', sfol, '/', mapping[j,1]))
#     outf <- paste0( fname = paste0(basePath, '/', sfol, '/', mapping[j,2]))
#     if(inf!=outf){
#       if(file.exists(inf)){
#         res<-file.rename(inf,outf)
#       }
#     }
#   }
# }


print(paste0('Finished Successfully'))


df90m <- read.csv('e:/temp/90mCleaning.csv', stringsAsFactors = F)
diffs <- df90m[df90m$Cnt != 166,]

tfls <- list.files(paste0(basePath, '/1853'), recursive = F, full.names = F)
cfls <- list.files(paste0(basePath, '/3079'), recursive = F, full.names = F)
setdiff(cfls, tfls)

cat('Dir,Compare\n', file = 'e:/temp/90mCompares.csv', append = F)
for(i in 1:length(dirs)){
  tile <- dirs[i]
  print(paste0(i, ' of ', length(dirs), ' : ', tile))
 
  cfls <- list.files(paste0(basePath, '/', tile), recursive = F, full.names = F)
  notinTemplate <- cfls[!(cfls %in% tfls)]
  notinTile <- tfls[!(tfls %in% cfls)]
  
  if(length(notinTemplate)>0){ cat(paste0(tile, ',Not in template - ',notinTemplate,  '\n'), file = 'e:/temp/90mCompares.csv', append = T)}
  if(length(notinTile)>0){ cat(paste0(tile, ',Not in Tile - ',notinTile,  '\n'), file = 'e:/temp/90mCompares.csv', append = T)}
}




fls <- list.files(paste0('M:/work/datasets/national/covariates/tiles/3250'), recursive = F, full.names = F)
write.csv(fls,paste0('M:/work/datasets/national/covariates/tile90Names.csv' ))








