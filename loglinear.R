library(readxl)
#https://otexts.com/fpp2/accuracy.html
#https://www.datatechnotes.com/2018/02/polynomial-regression-curve-fitting-in-r.html
#https://www.learnbymarketing.com/tutorials/linear-regression-in-r/
#https://www.scribbr.com/statistics/linear-regression-in-r/
#https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/
LINEARKUWAIT <- read_excel("Downloads/LINEARKUWAIT.xlsx")
Secondwave1 <- log(LINEARKUWAIT$Secondwave)
Day  <- LINEARKUWAIT$Day
LINEARKUWAIT1  <-data.frame(Day, Secondwave1)
linearMod <- lm(Secondwave1 ~ Day, data=LINEARKUWAIT1)
print(linearMod)
summary(linearMod)
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Day x Residuals Plot
plot(linearMod$resid~LINEARKUWAIT1$Day[order(LINEARKUWAIT1$Day)],
     main="Day x Residuals\nfor Simple Regression",
     xlab="Day", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(linearMod$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(linearMod$resid)
qqline(linearMod$resid)

library(fBasics)
jarqueberaTest(linearMod$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
#Residuals X-squared: 4.1228 p Value: 0.1273
#With a p value of 0.1273, we fail to reject the null hypothesis that the skewness and kurtosis of residuals are statistically equal to zero.
library(lmtest) #dwtest
dwtest(linearMod) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(LINEARKUWAIT1), 0.8*nrow(LINEARKUWAIT1))  # row indices for training data
trainingData <- LINEARKUWAIT1[trainingRowIndex, ]  # model training data
testData  <- LINEARKUWAIT1[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(Secondwave1 ~ Day, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
pred1a.test <- predict(linearMod, newdata=testData )
data.frame(testData, pred1a.test, distPred)
summary (lmMod)
summary(pred1a.test)
AIC (lmMod)
actuals_preds <- data.frame(cbind(actuals=testData, predicteds=distPred))  # make actuals_predicteds dataframe.
Secondwave.test <- testData$Secondwave1
par(mfrow=c(1,2))
plot(distPred, Secondwave.test)
abline(a=0, b=1, lty=2)
plot(distPred, pred1a.test)
abline(a=0, b=1, lty=2)
cor.test <- cor(pred1a.test, Secondwave.test)
R2.test <- cor.test^2
R2.test
alpha <- 0.05
conf.Secondwave <- predict(linearMod, data = LINEARKUWAIT1, interval="confidence", level=1-alpha) 
head(conf.Secondwave)
pred.Secondwave <- predict(linearMod, data = LINEARKUWAIT1, interval="prediction", level=1-alpha) 
head(pred.Secondwave)
library(ggplot2)
theme_set(theme_bw())
pl <- ggplot(LINEARKUWAIT1) + geom_point(aes(x=Day, y=Secondwave1), size=2, colour="#993399") + 
  xlab("Day") + ylab("Secondwave")  
print(pl)
LINEARKUWAIT1[c("fit","lwr.conf", "upr.conf")] <- conf.Secondwave
LINEARKUWAIT1[c("lwr.pred", "upr.pred")] <- pred.Secondwave[,2:3]
pl +
  geom_ribbon(data=LINEARKUWAIT1, aes(x=Day, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=LINEARKUWAIT1, aes(x=Day, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=LINEARKUWAIT1, aes(x=Day, y=fit), colour="#339900", size=1)
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=LINEARKUWAIT1, form.lm=Secondwave1 ~ Day, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=TRUE));  # performs the CV
attr(cvResults, 'ms')   
scatter.smooth(x=LINEARKUWAIT1$Day, y=LINEARKUWAIT1$Secondwave1, main="Secondwave1 ~ Day")  # scatterplot 
plot(linearMod$residuals, pch = 16, col = "red")
ggplot(LINEARKUWAIT1, aes(x = Day, y = Secondwave1)) +
  geom_point() +
  stat_smooth()
cor(LINEARKUWAIT1$Day, LINEARKUWAIT1$Secondwave1)
ggplot(LINEARKUWAIT1, aes(Day, Secondwave1)) +
  geom_point() +
  stat_smooth(method = lm)
confint(linearMod)
# RESIDUAL STANDARD ERROR
sigma(linearMod)*100/mean(LINEARKUWAIT1$Secondwave1)
par(mfrow=c(2,2))
plot(linearMod)
par(mfrow=c(1,1))
