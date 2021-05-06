
#################################
###  Author : Ross Searle         
###  Date : Tue Nov 27 11:08:24 2018                      
###  Project : TERN                
#################################


######   THis recreates the SQLLite database from the local SQLServer version of the NSSC database on Ross Searle Machine
######   This SQLLite DB is the one which is publically distributed


library(RSQLite)
library(DBI)
library(RODBC)
library(stringr)

gitRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git'
dataRoot <- 'G:/Team Drives'

source(paste0(gitRoot, '/AusSoilsDSM/SLGA/Production/NSSC/NSSCHelpers.R'))

SQLcon <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "NSSC2", 
                      Trusted_Connection = "True")


dbPath <- paste0(dataRoot, "/SLGA/Data/Production/NSSC/NSSC_2.0.0NEW.sqlite")
Litecon <- dbConnect(RSQLite::SQLite(), dbPath)


createStatements <- paste0(gitRoot, '/AusSoilsDSM/SLGA/Production/NSSC/NSSC_2.0.0.sqliteVarChar.sql')
dbSendQueries( Litecon, sqlFromFile(createStatements) )

for (i in 1:length(NSSCTables )) {
  print(paste0("Writing ", NSSCTables[i], '....'))
  sql <- paste0("select * from ", NSSCTables[i])
  tbl <- doQuery(SQLcon, sql)
  dbWriteTable(Litecon, NSSCTables[i], tbl, overwrite = F, append = T)
}





