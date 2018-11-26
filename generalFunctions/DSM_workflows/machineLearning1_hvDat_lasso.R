# Trials with various data mining algorithms: GBM (stochastic gradient boost)

## variable selection
## hyperparameter optimisation
## predicting onto raster

## Libraries
#install.packages("devtools") 
#library(devtools)
#install_bitbucket("brendo1001/ithir/pkg") #ithir package
library(ithir);library(caret);library(raster);library(rgdal);library(sp);library(MASS);library(glmnet)


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
## glmnet model with defaults
# Bootstrapping and optimisation of: 
# alpha
# lambda

### lasso
glm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "glmnet")  
summary(glm.model)
glm.model
str(glm.model)

## Just using the glmnet package
#fit1=glmnet(x= as.matrix(DSM_data_c[,5:15]), y= DSM_data_c$pH60_100cm, alpha =0.1) 
#print(fit1)
#plot(fit1, label=TRUE)
#names(DSM_data_c[,5:15])
#coef(fit1,s=0.07405264) # extract coefficients at a single value of lambda
#pred1<- predict(fit1,newx=as.matrix(DSM_data_c[,5:15]),s=0.07405264) # make predictions
#goof(observed = DSM_data_c$pH60_100cm, predicted = pred1, plot.it = T)
#cross-validation
#cvfit = cv.glmnet(x= as.matrix(DSM_data_c[,5:15]), y= DSM_data_c$pH60_100cm)
#plot(cvfit)
#cvfit$lambda.min
#coef(cvfit, s = "lambda.min")
#help(predict.glmnet)
#pred1<- predict(cvfit,newx=as.matrix(DSM_data_c[,5:15]),s= cvfit$lambda.min) # make predictions
#goof(observed = DSM_data_c$pH60_100cm, predicted = pred1, plot.it = T)


# predict on calibration data
glm.pred_c<-predict(glm.model, newdata= DSM_data_c[,5:15])
goof(observed = DSM_data_c$pH60_100cm, predicted = glm.pred_c, plot.it = T)

# predict on validation data
glm.pred_v<-predict(glm.model, DSM_data_v[,5:15])
goof(observed = DSM_data_v$pH60_100cm, predicted = glm.pred_v, plot.it = T)

# predict onto grid
glm.grid<- predict(hunterCovariates_sub, glm.model)
plot(glm.grid)


## SVM model fit: fiddly around with tuning hyperparameters
#
fitControl <- trainControl(method = "repeatedcv", number=10,
                           repeats = 20)

# repeated cv with 5 repeats (linear)
glm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "glmnet",trControl = fitControl)  
glm.model
summary(glm.model)


# expand the tuning possibiliites
# The algorithm is quite fast so it might work out to just use the default cross-validation
#fitControl <- trainControl(method = "repeatedcv", number=10,
#                           repeats = 2)

#tgrid <- expand.grid(
#  .alpha = seq(0,1,0.1),
#  .lambda=seq(0,1,0.001))

# Notes this seems to take a while
#glm.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "glmnet",trControl = fitControl, tuneGrid = tgrid)  
#glm.model
#summary(svm.model)




### PREDICT
# predict on calibration data
glm.pred_c<-predict(glm.model, DSM_data_c[,5:15])
goof(observed = DSM_data_c$pH60_100cm, predicted = glm.pred_c, plot.it = T)

# predict on validation data
glm.pred_v<-predict(glm.model, DSM_data_v[,5:15])
goof(observed = DSM_data_v$pH60_100cm, predicted = glm.pred_v, plot.it = T)

# predict onto grid
glm.grid<- predict(hunterCovariates_sub, glm.model)
plot(glm.grid)








