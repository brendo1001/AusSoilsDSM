# Trials with various data mining algorithms: GBM (stochastic gradient boost)

## variable selection
## hyperparameter optimisation
## predicting onto raster

## Libraries
#install.packages("devtools") 
#library(devtools)
#install_bitbucket("brendo1001/ithir/pkg") #ithir package
library(ithir);library(caret);library(raster);library(rgdal);library(sp);library(MASS)


##########################################################################################################
## Common dataset 
#point data
data(HV_subsoilpH)

# Start afresh 
#round pH data to 2 decimal places
HV_subsoilpH$pH60_100cm<- round(HV_subsoilpH$pH60_100cm, 2)

#remove already intersected data
HV_subsoilpH<- HV_subsoilpH[,1:3]

#add an id column
HV_subsoilpH$id<- seq(1, nrow(HV_subsoilpH), by = 1)

#re-arrange order of columns
HV_subsoilpH<- HV_subsoilpH[,c(4,1,2,3)]

#Change names of coordinate columns
names(HV_subsoilpH)[2:3]<- c("x", "y")


#grids (covariate raster)
data(hunterCovariates_sub)

## make data a sptatial object
coordinates(HV_subsoilpH)<- ~ x + y

# covariate data extract
DSM_data<- extract(hunterCovariates_sub, HV_subsoilpH, sp= 1, method = "simple")
DSM_data<- as.data.frame(DSM_data)
str(DSM_data)

## remove any missing values
which(!complete.cases(DSM_data))
DSM_data<- DSM_data[complete.cases(DSM_data),]

# Calibrarion and validation datasets
## Random holdback
set.seed(123)
training <- sample(nrow(DSM_data), 0.70 * nrow(DSM_data))
DSM_data_c<- DSM_data[training,]
DSM_data_v<- DSM_data[-training,]
###########################################################################################################################


##########################################################################################################################
## GBM model with defaults
# Bootstrapping and optimisation of: n.trees, interaction.depth, shrinkage, and n.minobsinnode.
gbm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "gbm")  
summary(gbm.model)
gbm.model

# predict on calibration data
gbm.pred_c<-predict(gbm.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = gbm.pred_c, plot.it = T)

# predict on calibration data
gbm.pred_v<-predict(gbm.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = gbm.pred_v, plot.it = T)

# predict onto grid
gbm.grid<- predict(hunterCovariates_sub, gbm.model)
plot(gbm.grid)


## GBM model fit: fiddly around with tuning hyperparameters
#
fitControl <- trainControl(method = "repeatedcv", number=5,
                           repeats = 5)

# repeated cv with 5 repeats
gbm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "gbm",trControl = fitControl)  
gbm.model
summary(gbm.model)


# expand the tuning possibiliites
fitControl <- trainControl(method = "repeatedcv", number=10,
                           repeats = 2)

tgrid <- expand.grid(
  .n.trees = c(50,100,250,500), 
  .interaction.depth = c(1:11),
  .shrinkage = c(0.05, 0.1, 0.2, 0.5, 0.8,0.9),
  .n.minobsinnode = c(10,25,50,100 ))

# Notes this seems to take a while
gbm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "gbm",trControl = fitControl, tuneGrid = tgrid)  
gbm.model
summary(gbm.model)




### PREDICT

# predict on calibration data
gbm.pred_c<-predict(gbm.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = gbm.pred_c, plot.it = T)

# predict on calibration data
gbm.pred_v<-predict(gbm.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = gbm.pred_v, plot.it = T)

# predict onto grid
gbm.grid<- predict(hunterCovariates_sub, gbm.model)
plot(gbm.grid)




