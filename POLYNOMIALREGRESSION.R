library(readxl)
LINEARKUWAIT <- read_excel("Downloads/LINEARKUWAIT.xlsx")
Secondwave1 <- log(LINEARKUWAIT$Secondwave)
Day  <- LINEARKUWAIT$Day
LINEARKUWAIT1  <-data.frame(Day, Secondwave1)
model <- lm(LINEARKUWAIT1$Secondwave1 ~ poly(LINEARKUWAIT1$Day, 4, raw=TRUE))
summary(model)
M <- model.matrix(model)
head(M)
print(coef(model))
poly1.ortho <- poly(Day, degree=1)
lm1.ortho <- lm(Secondwave1 ~ poly1.ortho)
M1 <- model.matrix(lm1.ortho)
head(M1)
t(M1)%*%M1
coef(lm1.ortho)
poly2.ortho_ <- poly(Day, degree=2)
lm2.ortho <- lm(Secondwave1 ~ poly2.ortho_)
M2 <- model.matrix(lm2.ortho)
head(M2)
t(M2)%*%M2
summary(lm2.ortho)
confint(lm2.ortho)
summary(lm1.ortho)$coefficients
summary(lm2.ortho)$coefficients
#anova(linearMod,  model)
pred = predict(model,data=LINEARKUWAIT1)

plot(x=LINEARKUWAIT1$Day, y=LINEARKUWAIT1$Secondwave1, pch=20, col="grey")

lines(LINEARKUWAIT1$Day, predict(lm(Secondwave1 ~ Day, data=LINEARKUWAIT1)), type="l", col="orange1", lwd=2)
lines(LINEARKUWAIT1$Day, predict(lm(Secondwave1~I(Day^2), data=LINEARKUWAIT1)), type="l", col="pink1", lwd=2)
lines(LINEARKUWAIT1$Day, predict(lm(Secondwave1~I(Day^3), data=LINEARKUWAIT1)), type="l", col="yellow2", lwd=2)
lines(LINEARKUWAIT1$Day, predict(lm(Secondwave1~poly(Day,3)+poly(Day,2), data=LINEARKUWAIT1)), type="l", col="blue", lwd=2)

legend("topleft", 
       legend = c("y~x,  - linear","y~x^2", "y~x^3", "y~x^3+x^2"), 
       col = c("orange","pink","yellow","blue"),
       lty = 1, lwd=3
) 
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(LINEARKUWAIT1), 0.8*nrow(LINEARKUWAIT1))  # row indices for training data
trainingData <- LINEARKUWAIT1[trainingRowIndex, ]  # model training data
testData  <- LINEARKUWAIT1[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(testData$Secondwave1 ~ poly(testData$Day, 4, raw=TRUE))
distPred <- predict(lmMod, testData)  # predict distance

data.frame(testData, distPred)
summary (lmMod)
AIC (lmMod)
actuals_preds <- data.frame(cbind(actuals=testData, predicteds=distPred))  # make actuals_predicteds dataframe.
Secondwave.test <- testData$Secondwave1
par(mfrow=c(1,2))
plot(distPred, Secondwave.test)
abline(a=0, b=1, lty=2)

alpha <- 0.05
conf.Secondwave <- predict(lmMod, data = LINEARKUWAIT1, interval="confidence", level=1-alpha) 
head(conf.Secondwave)
pred.Secondwave <- predict(lmMod, data = LINEARKUWAIT1, interval="prediction", level=1-alpha) 
head(pred.Secondwave)

correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  



confint(lmMod)
# RESIDUAL STANDARD ERROR
sigma(lmMod)*100/mean(LINEARKUWAIT1$Secondwave1)
par(mfrow=c(2,2))
plot(lmMod)
par(mfrow=c(1,1))



