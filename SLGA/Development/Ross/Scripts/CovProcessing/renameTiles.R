
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
#att = args[2]
print(paste0("Processing iteraration = ", k))


sq <- seq(1, 20041,50)
ivl <- sq[k]

root.tiles <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles_25m'

mapping <- read.csv('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/names.csv', stringsAsFactors = F)
fols<-read.csv('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/Tilenames.csv')
for(i in ivl:(ivl+49)){
  print(i)
  print(fols[i,1])
  
  sfol <- fols[i,1]
  for (j in 1:nrow(mapping)) {
    inf <- paste0( fname = paste0(root.tiles, '/', sfol, '/', mapping[j,1]))
    outf <- paste0( fname = paste0(root.tiles, '/', sfol, '/', mapping[j,2]))
    if(inf!=outf){
      if(file.exists(inf)){
        res<-file.rename(inf,outf)
      }
    }
  }
}


print(paste0('Finished Successfully'))



