library(raster)

inDirP <- 'v:/datasets/national/covariates/mosaics/30m/MaskedP'
inDir <- 'v:/datasets/national/covariates/mosaics/30m/Masked'
fls <- list.files(inDir, recursive = FALSE, full.names = F, pattern = ".tif$")
flsP <- list.files(inDirP, recursive = FALSE, full.names = F, pattern = ".tif$")

setdiff(fls, flsP)

fls <- list.files(inDir, recursive = FALSE, full.names = T, pattern = ".tif$")

templateR <- raster('M:/work/datasets/national/covariates/mosaics/30m/Masked/mask30.tif')

ac <- raster('E:/temp/FPAR/Veg_FPAR_Mean.tif')

compareRaster(ac, templateR)


ac <- raster('M:/work/Ross/New/Auscover/FinalMosaics/lztmre_aus_y20002011_dm7a2_d20050630.tif')


###  Check rasters have the same spatial support
for (i in 1:length(fls)) {
  r <- raster(fls[i])
  res <- compareRaster(templateR, r, stopiffalse=F)
  print(paste0(names(r), ' = ', res))
}


re <- extend(inR, templateR )

cr <- crop(ac2, templateR, method='ngb', snap='out')



compareRaster(cr, templateR)

##### Check tile counts

inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tiles30m'
fDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates'

tiles <- list.dirs(inDir, full.names = F, recursive = F)

cat('Tile,Cnt\n', file=paste0(fDir, '/count30Tiles.csv'), append = F )
for (i in 1:length(tiles)) {
  d <- tiles[i]
  t <- list.files(paste0(inDir, '/', d), full.names = F, recursive = F)
  cat(d, ',', length(t), '\n', file=paste0(fDir, '/count30Tiles.csv'), append = T )
  print(i)
}


df <- read.csv(paste0(fDir, '/count30Tiles.csv'))
head(df)
df[df$Cnt!=155,]



fls <- list.files('M:/work/datasets/national/covariates/mosaics/30m/Masked', full.names = F, recursive = F)
ndf <- read.csv('M:/work/datasets/national/covariates/names.csv', stringsAsFactors = F)
str(ndf)

new <- setdiff(fls, ndf$To)



##  90m mosaics
mdir <- 'M:/work/datasets/national/covariates/mosaics/90m'
mfls <- list.files(mdir, full.names = F, recursive = F)
mapping <- read.csv('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tile90Names.csv', stringsAsFactors = F)


setdiff(mfls, mapping$to)
setdiff(mapping$to, mfls)





##  90m mosaics
mdir <- 'M:/work/datasets/national/covariates/mosaics/30m/MaskedP'
mfls <- list.files(mdir, full.names = F, recursive = F)
mapping <- read.csv('//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates/tile90Names.csv', stringsAsFactors = F)


setdiff(mfls, mapping$to)
setdiff(mapping$to, mfls)



fpar <- raster('M:/work/Ross/SLGAData/FPARes/VEG_PERSISTANT_GREEN_VEG.tif')
templateR <- raster('M:/work/datasets/national/covariates/mosaics/30m/MaskedP/mask30.tif')

compareRaster(fpar, templateR)
