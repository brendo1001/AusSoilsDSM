library(DBI)
library(RSQLite)
library(stringr)



df <- read.table('C:/Users/sea084/Dropbox/ProjectAdmin/Covariates/Covariates.csv', header = T, sep = '|')

con <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/ProjectAdmin/Covariates/Covariates.sq3')
dbWriteTable(con, 'Covariates', df)

df <- dbReadTable(con,  'Covariates')
dbWriteTable(con,  'Covariates2', df)

df$OriginalDataset <- str_remove_all(df$OriginalDataset, '#')
df$MetaDataLink<- str_remove_all(df$MetaDataLink, '#')

dbWriteTable(con,  'Covariates', df, overwrite = T )





df <- dbReadTable(con,  'Covariates')
meta <- df$CoVariateName
df2<-df

nCovsPath <- 'V:/datasets/national/covariates/mosaics'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)

# sort out name differences due to Case
ondsklc<-str_to_lower(str_remove(fls, '.tif'))
metalc<-str_to_lower(meta)
inbothlc <- intersect(metalc, ondsklc)
length(inbothlc)
inboth <- str_to_lower(intersect(meta, ondsk))
setdiff(inbothlc, inboth )
#Manually made the edits in the DB and now move on


# Update 90mMosaics field where mosaic tifs are on disk
nCovsPath <- 'V:/datasets/national/covariates/mosaics'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
ondsk <- str_remove(fls, '.tif')
inboth <- intersect(meta, ondsk)
matches <- names(sapply(inboth, function(y) grep(y,ondsk, ignore.case=TRUE)))
df2[df2$CoVariateName%in%matches,]$X90mMosaic <- T

# Update 90mTiles field where tile tifs are on disk
tCovsPath <- 'V:/datasets/national/covariates/tiles/3844'
fls <- list.files(tCovsPath, pattern = '.tif$', full.names = F, recursive = F)
ondsk <- str_remove(fls, '.tif')
inboth <- intersect(meta, ondsk)
matches <- names(sapply(inboth, function(y) grep(y,ondsk)))
df2[df2$CoVariateName%in%matches,]$X90mTiles <- T


new <- setdiff(fls2, df$CoVariateName)

df3<-df2
for (i in 1:length(new)) {
  df3[nrow(df3)+1,1] <- paste0(new[i])
}


dbWriteTable(con,  'CovariatesWorking', df3)
