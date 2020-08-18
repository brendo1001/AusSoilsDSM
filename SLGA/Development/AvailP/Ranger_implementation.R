# Random Forest modeling for digital soil mapping
# Using Ranger within Caret implementation of the orginal Random Forests algorithium 
# Using Ranger for speed and Caret so that the predict raster function works
# Random hold back of 30% of training data for validation of model
# Model fitting using 10 K-folds with cross validation, final model fitted to 70% of training data
# Uncertainity anaylsis by Bootstrapping
#
# Version 0.1 - 22/05/2020 - Orginal script
#
# Required data and format
# Training data in csv file in TrainingData subdirectory with following column label and order: X, Y, ID, VALUE
# Covariate data in GTiff format, mosaic of whole modelling area in Covariate/Mosaics subdirectory
# Covariate tiles in Covariate/Tiles/TitleNumber subdirectory
#
## User inputs
wd <- "E://Temp//TERN2" #set working directory

## Processing starts here #####
###library(httr)
library(raster)
###library(rgdal)
library(sp)
###library(devtools)

## Data prep
# Get training data
td <- paste(wd, "//TrainingData", sep = "")
setwd(td)
data <- read.table("PData.csv", header = TRUE, sep = ",")
depths <- names(data[-(1:3)]) # List of depths in training data

# Get Covariates for whole modelling area
files <- list.files(path = paste(wd, "//Covariates//Mosaics", sep = ""), pattern = "\\.tif", full.names = TRUE) 
###files <- list.files(path = "M://Projects/PMap/Modelling/Stage1/P2/Cubist/Covariates", pattern = "\\.tif$", full.names = TRUE)

# Stack covariate rasters
r1 <- raster(files[1])
for(i in 2:length(files)){
  r1 <- stack(r1, files [i])
}

# Extract covariate values at training data point intersections
data_sp <- data
coordinates(data_sp) <- ~ X + Y
crs(data_sp) <- CRS('+init=EPSG:4326')
# Intersect soil points with covariates.
DSM_data <- raster::extract(r1, data_sp, sp = 1, method = 'simple')
DSM_data<- as.data.frame(DSM_data)
setwd(wd)
write.csv (DSM_data, file ="Site_Covariate_intersect.csv")
DSM_data <- DSM_data[complete.cases(DSM_data),] ## remove any missing values

## Modelling
# Establish calibration and validation data sets
set.seed(123)
training <- sample(nrow(DSM_data), 0.7 * nrow(DSM_data)) # Vector with training data row numbers

library(caret)
library(ranger)
library(dplyr)
library(e1071)
library(gstat)

# Create Bootstrap models directory
dir.create("Bootstrap")
dir.create("Bootstrap/models")
GOOFDat <- data.frame(Depths = NA, RMSE = NA, R2 = NA, MAE = NA, MSE = NA) # Summary stats df

# For each depth fit a model
for (d in 1:length(depths)) {
  cDat <- DSM_data[training, (3+d):(length(files)+3+length(depths))] # Based on number of covariates in ..//mosaic folder 
  vDat <- DSM_data[-training, (3+d):(length(files)+3+length(depths))] # Based on number of covariates in ..//mosaic folder
  
  # Set Ranger (within Caret) control parameters
  fitControl <- trainControl(method = "cv", number = 10, p = 0.7, returnResamp = "final", verboseIter = FALSE, indexFinal = training) #Ten K-folds

  # Uncertainity anaylsis using bootstrapping method
  nbag <- 50 # Number of bootstraps
  dir.create(paste("Bootstrap/models/", depths[d], sep = "")) # Create models subdirectory for each depth
  TrainDat <- DSM_data[,(3+d):(length(files)+3+length(depths))]
  names(TrainDat)[1] <- "VALUE"
  
  # Fit Ranger (RF) model for each bootstrap
  for (i in 1:nbag) {
    trainingREP <- sample.int(nrow(cDat), 1.0 * nrow(cDat), replace = TRUE)
    fit_ranger <- train(VALUE ~ ., data = TrainDat, method = "ranger", trControl = fitControl)
    modelFile <- paste(paste(paste(paste(getwd(), "/Bootstrap/models/", depths[d], "/", sep = ""), "bootMod_", sep = ""), i, sep = ""), ".rds", sep = "")
    saveRDS(object = fit_ranger, file = modelFile)
    }

  # List all files indirectory
  r.models <- list.files(path = paste(getwd(), "/Bootstrap/models/", depths[d], "/", sep = ""), pattern = "\\.rds$", full.names = TRUE)

  # Assess goodness of fit
  # Calibration data
  Mat <- matrix(NA, nrow = nbag, ncol = 4)
  for (i in 1:nbag) {
    fit_ranger <- readRDS(r.models[i])
    Dat <- cDat[1]
    Dat <- setNames( Dat, c("obs")) 
    Dat$pred <- predict(fit_ranger, newdata = cDat)
    Mat[i, 1:3] <- as.matrix(defaultSummary(Dat)) # RMSE, R^2, MAE
    Mat[i, 4] <- mean((Dat$obs -Dat$pred)^2) #MSE
    }

  calDat <- as.data.frame(Mat)
  names(calDat) <- c("RMSE", "R2", "MAE", "MSE")
  colMeans(calDat)

  #Validation data
  Pred.V <- matrix(NA, ncol = nbag, nrow = nrow(vDat))
  cubiMat <- matrix(NA, nrow = nbag, ncol =4)
  for (i in 1:nbag) {
    fit_ranger <- readRDS(r.models[i])
    Pred.V[, i] <- predict(fit_ranger, newdata = vDat)
    Dat <- vDat[1]
    Dat <- setNames( Dat, c("obs")) 
    Dat$pred <- predict(fit_ranger, newdata = vDat)
    cubiMat[i, 1:3] <- as.matrix(defaultSummary(Dat)) # RMSE, R^2, MAE
    cubiMat[i, 4] <- as.matrix(mean((Dat$obs - Dat$pred)^2)) #MSE
    }
  Pred.V_mean <- rowMeans(Pred.V)

  valDat <- as.data.frame(cubiMat)
  names(valDat) <- c("RMSE", "R2", "MAE", "MSE")
  valDatmeans <- colMeans(valDat)
  valDatmeans$Depths <- depths[d]
  GOOFDat <- rbind(GOOFDat, valDatmeans)
  
  avgMSE <- mean(valDat[, 4]) #Average validation MSE
  
  # Validate the quantification of uncertainity
  # SD of variance at each validation sample point due to bootstrapping = sqrt(variance + MSE)
  val.sd <- matrix(NA, ncol = 1, nrow = nrow(Pred.V))
  for (i in 1:nrow(Pred.V)) {
    val.sd[i, 1] <- sqrt(var(Pred.V[i, ]) + avgMSE)
    }

  #Percentile of normal distribution
  qp <- qnorm(c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525))
  # zfactor multiplication
  vMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:length(qp)) {
    vMat[, i] <- val.sd * qp[i]
    }

  # Upper predication limit
  uMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:length(qp)) {
    uMat[, i] <- Pred.V_mean + vMat[, i]
    }

  # Lower predication limit
  lMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:length(qp)) {
    lMat[, i] <- Pred.V_mean - vMat[, i]
    }

  #PI coverage probability (PICP)
  # Determine which predictions are within the PI limits
  bMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:ncol(bMat)) {
    names(vDat)[1] <- "VALUE"
    bMat[, i] <- as.numeric(vDat$VALUE <= uMat[, i] & vDat$VALUE >= lMat[, i])
  }
  # Plot results to file
  jpeg(file = paste(getwd(),"/Bootstrap/", depths[d], "_PICP_plot.jpg", sep = ""))
  cs <- c(99.5, 98.75, 97.5, 95, 90, 80, 70, 60, 55, 52.5)
  plot(cs, ((colSums(bMat)/nrow(bMat)) * 100), ylab = "PICP", xlab = "Confidence level", main = paste("Depth ", depths[d], sep = ""), abline(0,1), xlim = c(0, 100), ylim = c(0, 100))
  dev.off()
}

# Write Goodness of Fit data to file
setwd(paste(wd, "/Bootstrap/", sep = ""))
write.csv(GOOFDat, "GOOFData.csv", row.names = FALSE)
write.csv(avgMSE, "avgMSE.csv", row.names = FALSE) #Save to file for using in mapping part on HPC

### Mapping
## Predict fitted model tile by tile
# Make a list of tiles from tile folder
tile <- list.dirs(path = paste(wd, "//Covariates//Tiles", sep = ""), full.names = FALSE, recursive = FALSE)
dir.create("Bootstrap/map")

# Map tile by tile
for (t in 1:length(tile)) {
  dir.create(paste("Bootstrap/map/", tile[t], sep = ""))  
  files <- list.files(path = paste(wd, "/Covariates/Tiles/", tile[t], "/", sep = ""), pattern = ".tif", full.names = TRUE, recursive = TRUE)
  
  # Stack covariates for this tile
  t1 <- raster(files[1])
  for(i in 2:length(files)){
    t1 <- stack(t1, files [i])
    }
  
  # Map each depth of this tile
  for (d in 1:length(depths)) {
    r.models <- list.files(path = paste(getwd(), "/Bootstrap/models/", depths[d], "/", sep = ""), pattern = "\\.rds$", full.names = TRUE)
    dir.create(paste("Bootstrap/map/", tile[t], "/", depths[d], sep = "")) # output folder
  
    # Variance of each prediction
    for (i in 1:nbag) {
      fit_ranger <- readRDS(r.models[i])
      mapFile <- paste(paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "bootMap_", sep = ""), i, sep = ""), ".tif", sep = "")
      predict(t1, fit_ranger, filename = mapFile, format = "GTiff", overwrite = T)
      }

    # Determine pixel mean
    # 1. Load pathway to rasters
    files <- list.files(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), pattern = "bootMap_", full.names = TRUE)
    # 2. Stack raster
    m1 <- raster(files[1])
    for (i in 2:length(files)) {
      m1 <- stack(m1, files[i])
      }
    # 3. Calculate mean
    meanFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "meanPred_", sep = ""), ".tif", sep = "")
    bootMap.mean <- writeRaster(mean(m1), filename = meanFile, format = "GTiff", overwrite = TRUE)

    # Estimate variance at each pixel
    # Squared variance at each pixel
    for (i in 1:length(files)) {
      r2 <- raster(files[i])
      diffFile <- paste(paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "bootAbsDif_", sep = ""), i, sep = ""), ".tif", sep = "")
      jj <- (r2 - bootMap.mean)^2
      writeRaster(jj, filename = diffFile, format = "GTiff", overwrite = TRUE)
      }

    # Calculate sum of the squared difference
    # 1. Look for files with BootAbsDif in name
    files2 <- list.files(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), pattern = "bootAbsDif", full.names = TRUE)
    # 2. Stack rasters
    r3 <- raster(files2[i])
    for (i in 2:length(files2)) {
      r3 <- stack(r3, files2[i])
      }
    # 3. Calculate sum of square differences for each pixel and write to file
    sqDiffFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sqDiffPred_", sep = ""), ".tif", sep = "")
    bootMap.sqDiff <- writeRaster(sum(r3), filename = sqDiffFile, format = "GTiff", overwrite = TRUE)

    # Calculate variance
    varFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "varPred_", sep = ""), ".tif", sep = "")
    bootMap.var <- writeRaster(((1/(nbag - 1)) * bootMap.sqDiff), filename = varFile, format = "GTiff", overwrite = TRUE)

    # Calculate overall prediction variance
    varFile2 <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "varPredF_", sep = ""), ".tif", sep = "")
    bootMap.varF <- writeRaster((bootMap.var + avgMSE), filename = varFile2, format = "GTiff", overwrite = TRUE)

    # Determine prediction interval (PI)
    # PI = SD*z(90% probablity interval)
    # SD
    sdFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sdPred_", sep = ""), ".tif", sep = "")
    bootMap.sd <- writeRaster(sqrt(bootMap.varF), filename = sdFile, format = "GTiff", overwrite = TRUE)

    # z - Standard error (se)
    seFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sePred_", sep = ""), ".tif", sep = "")
    bootMap.se <- writeRaster((bootMap.sd * qnorm(0.95)), filename = seFile, format = "GTiff", overwrite = TRUE)

    # Map upper prediction limit
    uplFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "uplPred_", sep = ""), ".tif", sep = "")
    bootMap.upl <- writeRaster((bootMap.mean + bootMap.se), filename = uplFile, format = "GTiff", overwrite = TRUE)

    # Map lower prediction limit
    lplFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "lplPred_", sep = ""), ".tif", sep = "")
    bootMap.lpl <- writeRaster((bootMap.mean - bootMap.se), filename = lplFile, format = "GTiff", overwrite = TRUE)

    # Map prediction interval range
    pirFile <- paste(paste(paste(getwd(), "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "pirPred_", sep = ""), ".tif", sep = "")
    bootMap.pir <- writeRaster((bootMap.upl - bootMap.lpl), filename = pirFile, format = "GTiff", overwrite = TRUE)
    }
  }
### End of script Yah!
