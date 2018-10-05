library(DBI)
library(odbc)
library(stringr)

file_path <-"C:/Projects/TernLandscapes/Regolith/National Bores.accdb"

### THis is a bit of a hack to get rid of records in the locations table without related records in the lith table
### I could work out an easy way to do this in SQL
## using RODBC here because BDI kept falling over 
## this is not needed after this has been done once

con<-odbcConnectAccess2007(file_path)
result <- sqlQuery(con, "SELECT * FROM Bore_Locations2")

pb <- txtProgressBar(min = 0, max = nrow(result), style = 3)

for (i in 1:nrow(result)) {
  
  bid <- result[i,]$HydroID
  res <- sqlQuery(con, paste0("SELECT * from Bore_Lith where BoreID = ", bid))
  if(nrow(res) == 0)
  {
    sql <- paste0("Delete from Bore_Locations2 where HydroID = ", bid)
    resd <- sqlQuery(con, sql)
  }
  res = NULL
  setTxtProgressBar(pb, i)
}
close(pb)







#dbListTables(con)
#dbListFields(con, "Bore_Locations2")

outFileName <- 'c:/temp/ClayDepths.csv'
con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file_path,";"))

result <- dbSendQuery(con, "SELECT * FROM Bore_Locations2")
blocs <- dbFetch(result)

cat(paste('BID, HydroCode, Longitude, Latitude, layerNum, upperDepth, lowerDepth, Description, \n'), file = outFileName)

pb <- txtProgressBar(min = 0, max = nrow(blocs), style = 3)
for (i in 1:nrow(blocs)) {
  
  bid <- blocs[i,]$HydroID
  res <- dbSendQuery(con, paste0("SELECT * from Bore_Lith where BoreID = ", bid, " ORDER BY FromDepth"))
  brecs <- dbFetch(res)
  if(nrow(brecs) > 0)
  {
    #words <- c('clay', 'cly')
    #clayRecs <- brecs[grep(paste(words, collapse='|'), brecs$Descriptio, ignore.case=TRUE),]
    

    clayRecs <- brecs[(str_to_lower(str_trim(brecs$Descriptio)) == 'clay' 
                       | str_to_lower(str_trim(brecs$Descriptio)) == 'brown clay' 
                       | str_to_lower(str_trim(brecs$Descriptio)) == 'yellow clay' 
                       | str_to_lower(str_trim(brecs$Descriptio)) == 'black clay'
                       | str_to_lower(str_trim(brecs$Descriptio)) == 'red clay') 
                       & !is.na(brecs$Descriptio) , ]
    
   lyrNums <- which(str_to_lower(str_trim(brecs$Descriptio)) == 'clay'
          | str_to_lower(str_trim(brecs$Descriptio)) == 'brown clay' 
          | str_to_lower(str_trim(brecs$Descriptio)) == 'yellow clay' 
          | str_to_lower(str_trim(brecs$Descriptio)) == 'black clay'
          | str_to_lower(str_trim(brecs$Descriptio)) == 'red clay')
    
    if(nrow(clayRecs) > 0){
      
      cat(paste0(bid, ', ', blocs[i,]$HydroCode, ', ', blocs[i,]$Longitude, ', ', blocs[i,]$Latitude, ', ', lyrNums[1],
                 ', ', clayRecs[1,]$FromDepth,  ', ', clayRecs[1,]$ToDepth, ', ', clayRecs[1,]$Descriptio,
                 '\n'), file = outFileName, append = T)
    }
  }
  
  dbClearResult(res)
  setTxtProgressBar(pb, i)
  
}

dbDisconnect(con)
close(pb)



