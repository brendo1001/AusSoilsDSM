

root.tiles<-  paste0('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tiles_25m')

fols<- list.files(root.tiles, full.names = FALSE)

for (i in 1:length(fols)) {
  print(i)
  print(fols[i])
  objs <- file.info(list.files(paste0(root.tiles, '/', fols[i]), full.names=TRUE))
  dfls <- rownames(objs)[objs$size < 100000]
  unlink(dfls)
}

# got to i = 242

i = 300

d <- 'M:/work/datasets/national/covariates/tiles_25m/17496'
d1<-list.files(d)
write.csv(d1,'M:/work/datasets/national/covariates/names.csv')


tilefiles <- list.files(paste0(root.tiles, '/', fols[i]))


for (i in 1:242) {
  
  for (j in 1:length(tilefiles)) {
    
    fname = paste0(root.tiles, '/', fols[i], '/', tilefiles[j])
    
    if(!file.exists(fname)){
      mosR <- raster(paste0('M:/work/datasets/national/covariates/mosaics/30m/', tilefiles[j]))
      print(fname)
      outR <- crop(mosR, tile.raster, file=fname, overwrite=T )
    }
  }
  
  
  efls <- list.files(paste0(root.tiles, '/', fols[i]), full.names=F)
  
  
}

write.csv(fols, 'M:/work/datasets/national/covariates/Tilenames.csv', row.names = F)


fols <- list.dirs(root.tiles, full.names = F, recursive = F)







           