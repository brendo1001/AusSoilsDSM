library(DBI)
library(RSQLite)
library(stringr)



df <- read.table('C:/Users/sea084/Dropbox/ProjectAdmin/Covariates/Covariates.csv', header = T, sep = '|')

con <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/ProjectAdmin/Covariates/Covariates.sq3')

#dbWriteTable(con, 'Covariates', df)

df <- dbReadTable(con,  'Covariates')
#dbWriteTable(con,  'Covariates2', df)

df$OriginalDataset <- str_remove_all(df$OriginalDataset, '#')
df$MetaDataLink<- str_remove_all(df$MetaDataLink, '#')

dbWriteTable(con,  'Covariates', df, overwrite = T )





df <- dbReadTable(con,  'Covariates')
meta <- df$CoVariateName
df2<-df

nCovsPath <- 'V:/datasets/national/covariates/mosaics/30m/Masked'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
fls 

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


#dbWriteTable(con,  'CovariatesWorking', df3)





df <- dbReadTable(con,  'CovariatesWorking')
meta <- df$CoVariateName

nCovsPath <- 'V:/datasets/national/covariates/mosaics/30m/MaskedP'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
fls <- str_remove(fls, '.tif')
setdiff(fls, df$CoVariateName)


nCovsPath90m <- 'V:/datasets/national/covariates/mosaics/90m'
fls90m <- list.files(nCovsPath90m, pattern = '.tif$', full.names = F, recursive = F)
write.csv(fls90m, 'V:/datasets/national/covariates/mosaics/90mfls.csv')

in30 <- setdiff(str_to_upper(fls), str_to_upper(fls90m))
write.csv(in30, 'c:/temp/in30.csv')

in90 <- setdiff(str_to_upper(fls90m), str_to_upper(fls))
write.csv(in90, 'c:/temp/in90.csv')


#####  update "isin" 30m mosaics
dbWriteTable(con,  'CovariatesWorking_V1', df, overwrite = F )


nCovsPath <- 'V:/datasets/national/covariates/mosaics/30m/MaskedP'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
flsq<-paste0("'",fls,"'" )
ls <- paste(flsq, sep = ' ', collapse = ',')
sql <- paste0('SELECT * from CovariatesWorking where CoVariateName NOT IN (', ls, ')')
qry <- dbSendQuery(con, sql)
res <- dbFetch(qry)
dbClearResult(qry)
nrow(res)

upsql <- paste0('UPDATE CovariatesWorking SET X30mMosaic=1 WHERE CoVariateName IN (', ls, ')')
dbSendQuery(con,upsql)
upsql <- paste0('UPDATE CovariatesWorking SET X30mMosaic=0 WHERE CoVariateName NOT IN (', ls, ')')
dbSendQuery(con,upsql)
             


#####  update "isin" 30m mosaics
dbWriteTable(con,  'CovariatesWorking_V1', df, overwrite = F )


nCovsPath <- 'V:/datasets/national/covariates/mosaics/30m/MaskedP'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
flsq<-paste0("'",fls,"'" )
ls <- paste(flsq, sep = ' ', collapse = ',')
sql <- paste0('SELECT * from CovariatesWorking where CoVariateName NOT IN (', ls, ')')
qry <- dbSendQuery(con, sql)
res <- dbFetch(qry)
dbClearResult(qry)
nrow(res)

upsql <- paste0('UPDATE CovariatesWorking SET X30mMosaic=1 WHERE CoVariateName IN (', ls, ')')
dbSendQuery(con,upsql)
upsql <- paste0('UPDATE CovariatesWorking SET X30mMosaic=0 WHERE CoVariateName NOT IN (', ls, ')')
dbSendQuery(con,upsql)


#####  update "isin" 30m Tiles
upsql <- paste0('UPDATE CovariatesWorking SET X30mTiles=1 WHERE CoVariateName IN (', ls, ')')
dbSendQuery(con,upsql)
upsql <- paste0('UPDATE CovariatesWorking SET X30mTiles=0 WHERE CoVariateName NOT IN (', ls, ')')
dbSendQuery(con,upsql)



df <- dbReadTable(con,  'CovariatesWorking')
#####  update "isin" 90m mosaics

nCovsPath <- 'V:/datasets/national/covariates/mosaics/90m'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
fls <- str_remove(fls, '.tif')
flsq<-paste0("'",fls,"'" )
ls <- paste(flsq, sep = ' ', collapse = ',')
sql <- paste0('SELECT * from CovariatesWorking where CoVariateName IN (', ls, ')')
qry <- dbSendQuery(con, sql)
res <- dbFetch(qry)
dbClearResult(qry)
nrow(res)
length(fls)

d1 <- setdiff(fls, df$CoVariateName)
d1
d2 <- setdiff(fls, df$CoVariateName)
d2

upsql <- paste0('UPDATE CovariatesWorking SET X90mMosaic=1 WHERE CoVariateName IN (', ls, ')')
dbSendQuery(con,upsql)
upsql <- paste0('UPDATE CovariatesWorking SET X90mMosaic=0 WHERE CoVariateName NOT IN (', ls, ')')
dbSendQuery(con,upsql)



#####  update "isin" 90m tiles

nCovsPath <- 'V:/datasets/national/covariates/tiles/208'
fls <- list.files(nCovsPath, pattern = '.tif$', full.names = F, recursive = F)
fls <- str_remove(fls, '.tif')
flsq<-paste0("'",fls,"'" )
ls <- paste(flsq, sep = ' ', collapse = ',')
sql <- paste0('SELECT * from CovariatesWorking where CoVariateName IN (', ls, ')')
qry <- dbSendQuery(con, sql)
res <- dbFetch(qry)
dbClearResult(qry)
nrow(res)
length(fls)

d1 <- setdiff(fls, df$CoVariateName)
d1
d2 <- setdiff(fls, df$CoVariateName)
d2

upsql <- paste0('UPDATE CovariatesWorking SET X90mTiles=1 WHERE CoVariateName IN (', ls, ')')
dbSendQuery(con,upsql)
upsql <- paste0('UPDATE CovariatesWorking SET X90mTiles=0 WHERE CoVariateName NOT IN (', ls, ')')
dbSendQuery(con,upsql)





