library(googledrive)

drive_auth_config()
drive_api_key()
drive_user()

files <- drive_find(n_max = 500)
files

print(files, n=500)


gd <- as_team_drive("SLGA")
as_team_drive(as_id("0AAXQMKQldI8FUk9PVA"))

drive_find( '.txt', team_drive = gd)
drive_find("ASC", team_drive = 'SLGA')
drive_find( team_drive = as_team_drive(as_id("0AAXQMKQldI8FUk9PVA")))



read.csv(drive_get(path = as_id('10Abppaso8SUM9LVoVkrEUBIULl7xsMNJ'), team_drive = "SLGA"))

drive_ls("Regolith", team_drive = as_team_drive("SLGA"))


drive_download(as_id("1T2-M6sGrQK62ut5_-8RSVRSK-1vqcJoR"), path = 'c:/temp/')



gdd <- drive_get('Regolith/', team_drive = gd)
drive_ls(gdd)
drive_ls(gdd, recursive = T)


gdf <- drive_get('ASC/ASCs.csv', team_drive = gd)
drive_browse(gdf)

drive_download(gdf, path)
getwd()



drive_share(gd, role='writer')



