library(readxl)
LINEARKUWAIT <- read_excel("Downloads/LINEARKUWAIT.xlsx")
Secondwave1 <- log(LINEARKUWAIT$Secondwave)
Day  <- LINEARKUWAIT$Day
LINEARKUWAIT1  <-data.frame(Day, Secondwave1)
library(e1071)
modelsvm = svm(Secondwave1 ~ Day, data=LINEARKUWAIT1)
summary(modelsvm)
jarqueberaTest(modelsvm$resid)
predYsvm = predict(modelsvm, data=LINEARKUWAIT1)
plot(LINEARKUWAIT1)
points(LINEARKUWAIT1$Day, predYsvm, col = "red", pch=16)
##Calculate parameters of the SVR model
library(hydroGOF)
#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho
summary(predYsvm)
## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predYsvm,LINEARKUWAIT1$Secondwave1)
## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, Secondwave1 ~ Day, data=LINEARKUWAIT1,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
# YOU CAN ALSO TRY seq(0,0.2,0.01)
#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)
## Select the best model out of 1100 trained models and compute RMSE

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,LINEARKUWAIT1)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,LINEARKUWAIT1$Secondwave1)
##Calculate parameters of the Best SVR model

#Find value of W
W = t(BstModel$coefs) %*% BstModel$SV
#kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html
#Find value of b
b = BstModel$rho
## Plotting SVR Model and Tuned Model in same plot
#Actual data (black), SVR model (blue), tuned SVR model (red).
plot(LINEARKUWAIT1, pch=16)
points(LINEARKUWAIT1$Day, predYsvm, col = "blue", pch=3)
points(LINEARKUWAIT1$Day, PredYBst, col = "red", pch=4)
points(LINEARKUWAIT1$Day, predYsvm, col = "blue", pch=3, type="l")
points(LINEARKUWAIT1$Day, PredYBst, col = "red", pch=4, type="l")

library(caret)
trainingRowIndex <- sample(1:nrow(LINEARKUWAIT1), 0.8*nrow(LINEARKUWAIT1))  # row indices for training data
train <- LINEARKUWAIT1[trainingRowIndex, ]  # model training data
test  <- LINEARKUWAIT1[-trainingRowIndex, ]   # test data
model_reg = svm(Secondwave1 ~ Day, data=train)
jarqueberaTest(model_reg$resid)
print(model_reg)
pred = predict(model_reg, test)

x = 1:length(test$Secondwave1)
plot(x, test$Secondwave1, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")
#https://www.datatechnotes.com/2019/09/support-vector-regression-example-with.html
mae = MAE(test$Secondwave1, pred)
rmse = RMSE(test$Secondwave1, pred)
r2 = R2(test$Secondwave1, pred, form = "traditional")

cat(" MAE:", mae, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)
cm = data.frame(test , pred)
