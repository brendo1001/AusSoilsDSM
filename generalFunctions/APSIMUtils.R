library(RCurl)
library(XML)
library(stringr)
library(rgdal)



APSIMPath = '"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"'
servicePath <- 'http://ternsoils.nexus.csiro.au:8080'

runAPSIMP <- function(prefix, sim, simRoot){
  simNamePath <- paste0(simRoot, '/', prefix, sim, '.apsim')
  apsimcmd = paste0(APSIMPath, ' "', simNamePath, '"')
  system(apsimcmd)
}

readShapeFile <- function(filenameSHP)
{
  src <- dirname(filenameSHP)
  lyr <- str_replace_all(basename(filenameSHP), ".shp", "")
  paddockbdys <- readOGR(src, layer=lyr)
}

readKMLFile <- function(filenameSHP)
{
  paddockbdys <- readOGR(filenameSHP)
}


readAPSIMOutputs  <- function(filenameA, headerRows = 2, NAs = c('?', '-1.#IND00')){

 if(file.exists(filenameA)){
    if(file.size(filenameA) > 0){
        g <- read.table(filenameA, skip = headerRows, header=T, nrows = 1, stringsAsFactors=F, comment.char = "")
        d <- read.table(filenameA, skip = headerRows+2, header=F,  na.strings = '?', comment.char = "")
        colnames(d) <- colnames(g)
        return (d)
    }else{
        return (NULL)
      }
  }  else{
    return (NULL)
  }
}




readAPSIMCSVOutputs  <- function(filenameA, headerRows = 2, NAs = c('?', '-1.#IND00')){
  
  if(file.exists(filenameA)){
    if(file.size(filenameA) > 0){
      g <- read.csv(filenameA, skip = headerRows, header=T, nrows = 1, stringsAsFactors=F, comment.char = "")
      d <- read.csv(filenameA, skip = headerRows+2, header=F,  na.strings = '?', comment.char = "")
      colnames(d) <- colnames(g)
      return (d)
    }else{
      return (NULL)
    }
  }  else{
    return (NULL)
  }
}



saveXMLFile <- function(node, outpath)
{
  
  tempPath = paste(tempdir(),"/simtemp.xml", sep="")
  print(tempPath)
  saveXML(node, file=tempPath)
  doc1 <- readLines(tempPath, warn = FALSE)
  writeLines( '<?xml version="1.0"?>', con=outpath)
  write(doc1,file=outpath,append=TRUE)
  xmlfile = str_replace(outpath, '.apsim', '.xml')
  file.copy(outpath, xmlfile, overwrite=T)
  return (xmlfile)
  
}

saveSoilFile <- function(soilxmltext, outpath)
{ 
  
  print(outpath)
  saveXML(soilxmltext, file=outpath)
  xmlfile = str_replace(outpath, '.soil$', '.xml')
  #print(xmlfile)
  #print(outpath)
  file.copy(outpath, xmlfile, overwrite=T)
  
  return (xmlfile)
  
}

generateLocationstring <- function(locations)
{
  outs = ""
  for(i in 1:nrow(locations)) {
    id <- locations[i,1]
    lon<- locations[i,2]
    lat <- locations[i,3]
    outs = (paste(outs , id,",",lon,",",lat,";",sep="" ))    
  }
  return (outs)
}

# get APSIM Soil parameter files as XML



getSoilParameterFiles <- function(locURL, outPath, viewResults)
{
  soilURL = paste(servicePath, "/actions.svc/getApsoilAgData?",locURL, sep="")
  print(soilURL)
  soildata = getURL(soilURL)
  soilxmltext  <- xmlTreeParse(soildata, asText = TRUE,useInternalNodes=T)
  soilxmlFile = saveSoilFile(soilxmltext, outPath)
  if(viewResults)
  {
    browseURL(soilxmlFile)
  }
  return (soilxmltext)
}


getMetFileLastDate <- function(metFileName){
  
  metDF <- readSiloAPSIMMetFile(metFileName)
  lastRow <- metDF[nrow(metDF),]
  
  lastDate <- strptime(paste(lastRow[1,1], lastRow[1,2]), format="%Y %j")
  return (lastDate)
}


readSiloAPSIMMetFile <- function(metFileName, headerRows = 21){
  g <- read.table(metFileName, skip = headerRows, header=T, nrows = 1, sep = "", strip.white = T)
  d <- read.table(metFileName, skip = headerRows + 3, header=F, sep = "", strip.white = T, na.strings = '?')
  colnames(d) <- colnames(g)
  return (d)
}

