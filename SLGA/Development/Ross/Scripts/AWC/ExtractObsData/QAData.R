library(sf)
library(ggplot2)
library(dplyr)

workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'


indf <- read.csv(paste0(workDir, '/SDF_All_Props_Clean.csv'), stringsAsFactors = F)
#indf <- read.csv('C:/Projects/TernLandscapes/AWC/ObsDataSDF/SDF_All_Props_Clean_With_PTFS_Splined.csv', stringsAsFactors = F)


unique(indf$Dataset)
count(indf, 'Dataset')
ddply(indf, .(SoilSuperGrp), summarize,  BD=mean(BlkDen, na.rm=T), SOC=mean(SOC, na.rm=T))

summary(indf)


locs <- unique(indf[c("Longitude", "Latitude", "Dataset", "Location_ID")])
nrow(locs)
pts <- st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
head(pts)
length(unique(pts$Dataset))
plot(st_geometry(pts),  col = 'red',  axes = TRUE, pch=3)
aus <- st_read('C:/Projects/GIS/National/Australia.shp')
plot(st_geometry(aus), add=T)

st_write(pts, paste0(workDir, '/SDF_All_Locations.shp'),append=FALSE, overwrite=T)


# ##### some individual check before collation 
# soc <- read.csv(paste0(workDir, '/SDF_All_SOC.csv'), stringsAsFactors = F)[,-c(1)]
# # SOC check per dataset
# ggplot(soc, aes(x=Dataset, y=Value, fill=Dataset)) + 
#   geom_boxplot(alpha=0.3) +
#   theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
#   ggtitle("Soc per Dataset") 
# 
# 
# # Checking WA values
# cldf <- read.csv( paste0(workDir, '/Clean_Clay.csv'), stringsAsFactors = F)
# cldf <- cldf[cldf$Value >=0, ]
# ggplot(cldf, aes(x=Dataset, y=Value, fill=Dataset)) + 
#   geom_boxplot(alpha=0.3) +
#   theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
#   ggtitle("Clay per Dataset") 
# 
# wa <- cldf[cldf$Dataset=='WAGovernment', ]
# mean(wa$Value)
# wasdf <- indf[indf$Dataset=='WAGovernment', ]
# mean(wasdf$clay, na.rm=T)
# 
# idxs <- which(is.na(indf$clay))
# indf[idxs,]
# ####  collated data set checks


# clay per dataset
ggplot(indf, aes(x=Dataset, y=clay, fill=Dataset)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Clay per Dataset") 

# Sand per dataset
ggplot(indf, aes(x=Dataset, y=Sand, fill=Dataset)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Sand per Dataset") 

# Silt per dataset
ggplot(indf, aes(x=Dataset, y=Silt, fill=Dataset)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Silt per Dataset") 

# BD per dataset
ggplot(indf, aes(x=Dataset, y=BlkDen, fill=Dataset)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Bulk Density per Dataset") 

# SOC per dataset
ggplot(indf, aes(x=Dataset, y=SOC, fill=Dataset)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("SOC per Dataset") 



allpts <- st_as_sf(indf, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = F)
dsts <- unique(allpts$Dataset)

for (i in 1:length(dsts)) {
  ds <- dsts[i]
  print(ds)
  plot(st_geometry(aus), main=ds)
  plot(st_geometry(allpts[allpts$Dataset==ds,]),  col = 'red',  axes = TRUE, pch=3, add=T)
}

plot(allpts["clay"],  axes = TRUE, pch=3,)
plot(allpts["Sand"],  axes = TRUE, pch=3,)
plot(allpts["Silt"],  axes = TRUE, pch=3,)
plot(allpts["BlkDen"],  axes = TRUE, pch=3,)
plot(allpts["SOC"],  axes = TRUE, pch=3,)
plot(allpts[allpts$SOC < 5, "SOC"],  axes = TRUE, pch=3,)
plot(allpts["HorName"],  axes = TRUE, pch=3)
plot(allpts["GSG"],  axes = TRUE, pch=3)
plot(allpts["PPF"],  axes = TRUE, pch=3)
plot(allpts["ASC"],  axes = TRUE, pch=3)




### Check the soil grouping stuff 
cdf <- read.csv(paste0(workDir2, '/obs_With_HorGrp_and_SoilGrps.csv'), stringsAsFactors =  F)

count(allpts[!is.na(allpts$ASC), ], 'Dataset')

alldf <- read.csv(paste0(workDir, '/SDF_All_Props.csv'), stringsAsFactors = F)
count(alldf[!is.na(alldf$ASC), ], 'Dataset')


