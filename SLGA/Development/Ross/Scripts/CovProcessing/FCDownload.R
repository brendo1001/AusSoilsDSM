library(tictoc)
library(raster)
library(httr)
library(XML)
library(xml2)
library(stringr)
library(htmltidy)
library(ncdf4)
library(RCurl)

#http://dapds00.nci.org.au/thredds/catalog.html
#http://dapds00.nci.org.au/thredds/catalog/fk4/datacube/002/FC/FC-percentile/ANNUAL/catalog.html
#https://cmi.ga.gov.au/sites/default/files/2017-12/DEA%20Tiles_1.pdf
#http://cmi.ga.gov.au/sites/default/files/2017-12/DEA_v2_Albers_tiles_Australia.zip


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FC'
}else{
  outDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGAData/FC'
}

tic()
options(timeout = 300)
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

#k=500


rootDir <- 'http://dapds00.nci.org.au/thredds/catalog/fk4/datacube/002/FC/FC-percentile/ANNUAL'
rootDir2 <- 'http://dapds00.nci.org.au/thredds/fileServer/fk4/datacube/002/FC/FC-percentile/ANNUAL'


linkoURL <- paste0(rootDir, '/catalog.html')
doc <- htmlParse(linkoURL)
links <- xpathSApply(doc, '//a[@href]', xmlValue)
dirs <- links[2:928]
length(dirs)

dir <- dirs[k]

urlFC <- paste0(rootDir, '/', dir, '/catalog.html')
doc2 <- htmlParse(urlFC)
links2 <- xpathSApply(doc2, '//a[@href]', xmlValue)
linksf <- links2[grepl('.nc$',links2)]

downDir <-  paste0(outDir, '/NC/', dir )
if(!dir.exists(downDir)){dir.create(downDir, recursive = T)}
print(paste0('Output Directory - ', downDir))

    for (i in 1:length(linksf)) {
      f <- linksf[i]
      print(f)
      urld <- paste0(rootDir2, '/', dir, f)
      outF <- paste0(downDir, f )
      if(!file.exists(outF)){
        print(paste0('Downloading - ', urld))
        response <- RETRY("GET", url=urld, times = 20, pause_base = 5, pause_cap=30, quiet = F)
        if( response$status_code == 200){
          writeBin(response$content, outF)
          Sys.sleep(3)
        }else{
          stop("Download Error")
        }
        #download.file(urld, destfile = outF, quiet = T, mode = 'wb')
      }else{
        print(paste0('File exists - ', outF))
      }
    }

toc()
print(paste0('Finished Successfully'))


