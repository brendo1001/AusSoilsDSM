# Trials with various data mining algorithms: GBM (stochastic gradient boost)

## variable selection
## hyperparameter optimisation
## predicting onto raster

## Libraries
#install.packages("devtools") 
#library(devtools)
#install_bitbucket("brendo1001/ithir/pkg") #ithir package
library(ithir);library(caret);library(raster);library(rgdal);library(sp);library(MASS);library(e1071)


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
## svn model with defaults
# Bootstrapping and optimisation of: 


### Linear support vector
#C: 
svm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "svmLinear")  
summary(svm.model)
svm.model

### Radial support vector
#C:
#sigma: 
svm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "svmRadial")  
summary(svm.model)
svm.model


### Polynomial support vector
#degree:
#scale:
#C: 
svm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "svmPoly")  
summary(svm.model)
svm.model


# predict on calibration data
svm.pred_c<-predict(svm.model, newdata= DSM_data_c[,5:15])
goof(observed = DSM_data_c$pH60_100cm, predicted = svm.pred_c, plot.it = T)

# predict on validation data
svm.pred_v<-predict(svm.model, DSM_data_v[,5:15])
goof(observed = DSM_data_v$pH60_100cm, predicted = svm.pred_v, plot.it = T)

# predict onto grid
svm.grid<- predict(hunterCovariates_sub, svm.model)
plot(svm.grid)


## SVM model fit: fiddly around with tuning hyperparameters
#
fitControl <- trainControl(method = "repeatedcv", number=10,
                           repeats = 2)

# repeated cv with 5 repeats (linear)
svm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "svmLinear",trControl = fitControl)  
svm.model
summary(svm.model)


# expand the tuning possibiliites
fitControl <- trainControl(method = "repeatedcv", number=10,
                           repeats = 2)

tgrid <- expand.grid(
  .C = seq(0,5,0.25))

# Notes this seems to take a while
svm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "svmLinear",trControl = fitControl, tuneGrid = tgrid)  
svm.model
summary(svm.model)




### PREDICT
# predict on calibration data
xg.pred_c<-predict(svm.model, DSM_data_c[,5:15])
goof(observed = DSM_data_c$pH60_100cm, predicted = xg.pred_c, plot.it = T)

# predict on validation data
xg.pred_v<-predict(svm.model, DSM_data_v[,5:15])
goof(observed = DSM_data_v$pH60_100cm, predicted = xg.pred_v, plot.it = T)

# predict onto grid
xg.grid<- predict(hunterCovariates_sub, svm.model)
plot(xg.grid)


### The only hurdle with svmis that in the predict mode, one needs to be explict about the predictor variable ie need to index the relevent columns.
### Appears to work quite well though in terms of generalisation





