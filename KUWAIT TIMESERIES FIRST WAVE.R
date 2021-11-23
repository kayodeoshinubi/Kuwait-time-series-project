library(readxl)
KUWAITFIRSTWAVE <- read_excel("Downloads/KUWAITFIRSTWAVE.xlsx")
#https://www.geeksforgeeks.org/exponential-smoothing-in-r-programming/
#https://www.statmethods.net/advstats/timeseries.html
First_wave_cases <- ts(KUWAITFIRSTWAVE$Firstwave)
#SIMPLE EXPONENTIAL SMOOTHING
First_wave_cases.train <- window(First_wave_cases, end = 60)
First_wave_cases.test <- window(First_wave_cases, start = 61)
ses.First_wave_cases <- ses(First_wave_cases.train, 
                                        alpha = .2,
                                        h = 100)
autoplot(ses.First_wave_cases)
accuracy(ses.First_wave_cases)
accuracy(ses.First_wave_cases, First_wave_cases.test)
summary(ses.First_wave_cases)
checkresiduals(ses.First_wave_cases)
First_wave_cases.dif <- diff(First_wave_cases.train)
autoplot(First_wave_cases.dif)

# reapplying SES on the filtered data
ses.First_wave_cases.dif <- ses(First_wave_cases.dif,
                                            alpha = .2, 
                                            h = 100)
autoplot(ses.First_wave_cases.dif)
accuracy(ses.First_wave_cases.dif)
summary(ses.First_wave_cases.dif)
checkresiduals(ses.First_wave_cases.dif)
# removing trend from test set
First_wave_cases.dif.test <- diff(First_wave_cases.test)
autoplot(First_wave_cases.dif.test)

summary(First_wave_cases.dif.test)
accuracy(ses.First_wave_cases.dif, First_wave_cases.dif.test)

# comparing our model
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(First_wave_cases.dif, alpha = alpha[i],
             h = 100)
  RMSE[i] <- accuracy(fit, 
                      First_wave_cases.dif.test)[2,2]
}

# convert to a data frame and 
# idenitify min alpha value

alpha.fit <- data.frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, 
                    RMSE == min(RMSE))

plot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min,
             aes(alpha, RMSE), 
             size = 2, color = "red")
# removing trend
First_wave_cases.dif <- diff(First_wave_cases.train)

# refit model with alpha = .05
ses.First_wave_cases.opt <- ses(First_wave_cases.dif, 
                                            alpha = .05,
                                            h = 100)
autoplot(ses.First_wave_cases.opt)

summary(ses.First_wave_cases.opt)
# performance eval
accuracy(ses.First_wave_cases.opt, First_wave_cases.dif.test)
checkresiduals(ses.First_wave_cases.opt)
# plotting results
p1 <- autoplot(ses.First_wave_cases.opt) +
  theme(legend.position = "bottom")
p2 <- autoplot(First_wave_cases.dif.test) +
  autolayer(ses.First_wave_cases.opt, alpha = .5) +
  ggtitle("Predicted vs. actuals for 
                 the test data set")

gridExtra::grid.arrange(p1, p2, 
                        nrow = 1)
#HOLTS METHOD
holt.First_wave_cases <- holt(First_wave_cases.train,
                                          h = 100)
autoplot(holt.First_wave_cases)
accuracy(holt.First_wave_cases)
summary(holt.First_wave_cases)
checkresiduals(holt.First_wave_cases)
# holt's method
holt.First_wave_cases$model

# accuracy of the model
accuracy(holt.First_wave_cases, First_wave_cases.test)
# identify optimal alpha parameter
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(First_wave_cases.train,
              beta = beta[i], 
              h = 100)
  RMSE[i] <- accuracy(fit, 
                      First_wave_cases.test)[2,2]
}

# convert to a data frame and
# idenitify min alpha value
beta.fit <- data.frame(beta, RMSE)
beta.min <- filter(beta.fit, 
                   RMSE == min(RMSE))

# plot RMSE vs. alpha
plot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, 
             aes(beta, RMSE), 
             size = 2, color = "red")

holt.First_wave_cases <- holt(First_wave_cases.train,
                                          h = 100)

# new model with optimal beta
holt.First_wave_cases.opt <- holt(First_wave_cases.train,
                                              h = 100,
                                              beta = 0.0601)

# accuracy of first model
accuracy(holt.First_wave_cases, First_wave_cases.test)

# accuracy of new optimal model
accuracy(holt.First_wave_cases.opt, First_wave_cases.test)

p1 <- autoplot(holt.First_wave_cases) +
  ggtitle("Original Holt's Model") +
  coord_cartesian(ylim = c(50, 1000))

p2 <- autoplot(holt.First_wave_cases.opt) +
  ggtitle("Optimal Holt's Model") +
  coord_cartesian(ylim = c(50, 1000))

gridExtra::grid.arrange(p1, p2, 
                        nrow = 1)
plot(First_wave_cases)
autoplot(First_wave_cases)
library(forecast)
fit <- ets(First_wave_cases)  
forecast(fit, 50)
autoplot(forecast(fit, 50))
accuracy(fit)
summary(fit)
checkresiduals(fit)
First_wave_cases.hw <- ets(First_wave_cases.train,
                                       model = "AAN")
autoplot(forecast(First_wave_cases.hw))
accuracy(First_wave_cases.hw)
summary(First_wave_cases.hw)
checkresiduals(First_wave_cases.hw)

# forecast the next 50 DAYS
First_wave_cases.hw.f1 <- forecast(First_wave_cases.hw,
                                               h = 50)

# check accuracy
accuracy(First_wave_cases.hw.f1, First_wave_cases.test)
summary(First_wave_cases.hw.f1, First_wave_cases.test)
summary(First_wave_cases.hw)
summary(First_wave_cases.hw.f1)
#ARIMA
fit1 <- auto.arima(First_wave_cases)  
checkresiduals(fit1)
accuracy(fit1)
summary(fit1)
forecast(fit1, 50)
autoplot(forecast(fit1, 50))           
First_wave_cases.hw1 <- ets(First_wave_cases.train,
                                        model = "ANN")
autoplot(forecast(First_wave_cases.hw1))
accuracy(First_wave_cases.hw1)
summary(First_wave_cases.hw1)
checkresiduals(First_wave_cases.hw1)
#ARIMA
# forecast the next 50 DAYS
First_wave_cases.hw.f2 <- forecast(First_wave_cases.hw1,
                                               h = 50)

# check accuracy
accuracy(First_wave_cases.hw.f2, First_wave_cases.test)
summary(First_wave_cases.hw.f2, First_wave_cases.test)

First_wave_cases.hw2 <- auto.arima(First_wave_cases.train)

autoplot(forecast(First_wave_cases.hw2))
accuracy(First_wave_cases.hw2)
summary(First_wave_cases.hw2)
checkresiduals(First_wave_cases.hw2)

# forecast the next 50 DAYS 
First_wave_cases.hw.f3 <- forecast(First_wave_cases.hw2,
                                               h = 50)

# check accuracy
accuracy(First_wave_cases.hw.f3, First_wave_cases.test)
summary(First_wave_cases.hw.f3, First_wave_cases.test)

#DAMPING METHOD 

# holt's linear (additive) model
fit4 <- ets(First_wave_cases, model = "ZAN",
            alpha = 0.8, beta = 0.2)
pred1 <- forecast(fit4, h = 50)

# holt's linear (additive) model
fit5 <- ets(First_wave_cases, model = "ZAN", 
            damped = TRUE, alpha = 0.8, 
            beta = 0.2, phi = 0.85)
pred2 <- forecast(fit5, h = 50)

# holt's exponential
# (multiplicative) model
fit6 <- ets(First_wave_cases, model = "ZMN",
            alpha = 0.8, beta = 0.2)
pred3 <- forecast(fit6, h = 50)

# holt's exponential 
# (multiplicative) model damped
fit7 <- ets(First_wave_cases, model = "ZMN", 
            damped = TRUE,
            alpha = 0.8, beta = 0.2,
            phi = 0.85)
pred4 <- forecast(fit7, h = 50)

autoplot(First_wave_cases) +
  autolayer(pred1$mean, 
            color = "blue") +
  autolayer(pred2$mean, 
            color = "blue",
            linetype = "dashed") +
  autolayer(pred3$mean, 
            color = "red") +
  autolayer(pred4$mean, 
            color = "red", 
            linetype = "dashed")
