library(RCurl)

tl <- tempfile()
tl<- 'c:/temp/app.R'
af <-  download.file('https://github.com/brendo1001/AusSoilsDSM/raw/master/Shiny/DSMTools/app.R', tl, quiet = T)
vt <- readLines(con = tl, n = 1)

