library(tidyverse)
library(ggthemes)
library(Hmisc)
library(lmtest)
library(forecast)

DavisWeather = read.csv("DavisWeather.csv")[,-1]
head(DavisWeather)

# 1
dw = DavisWeather %>% filter(year == 2017)
seasonal.model = lm(maxtemp ~ as.factor(month), data = dw)
dw$deseas = seasonal.model$residuals
dw$t = 1:nrow(dw)
dw %>% ggplot(aes(x = t, y = deseas)) + geom_line() + theme_bw() + 
  scale_x_continuous("Time (Days)") + scale_y_continuous("Deseasonalized Max Temp") +
  ggtitle("Deseasonalized Max Temp vs. Time", subtitle = "2017")

# 2
par(mfrow = c(1,2))
acf(dw$deseas)
pacf(dw$deseas)
par(mfrow = c(1,1))

"We see a sudden drop in the PACF plot but a gradual decline in the ACF plot, 
this means an AR model should be used for this time series data."

# 3
dw$L1.deseas = Lag(dw$deseas, 1)
ar1.model = lm(deseas ~ L1.deseas, data = dw)
bgtest(ar1.model)

"The p-value from this BG test is less than 0.0, this means we reject H0 and conclude that there is
serial correlation in this model. We should consider adding more lags into this model."

# 4
dw$L2.deseas = Lag(dw$deseas, 2)
ar2.model = lm(deseas ~ L1.deseas + L2.deseas, data = dw)
bgtest(ar2.model)

"The p-value from this BG test is greater than 0.05, this means we failt to reject HO
and conclude that there is no serial correlation in this model. This makes sense as previosuly,
we concluded there is evidence of serial correlation. We fix this by adding more lags, which is 
what we did by using an AR(2) model."

# 5
auto.model = auto.arima(dw$deseas);auto.model

"The R command picks an ARMA(2,1) model"

# 6
f = forecast(auto.model, h = 3)
c(f$x[350:365], f$mean)
rawpred = seasonal.model$coefficients[1] + f$mean

Observed = dw$maxtemp[335:365]
Predicted = rawpred
mtemp = c(Observed, Predicted)
Type = c(rep("Observed", 31), rep("Predicted", 3))
t = 335:368
cbind.data.frame(t, mtemp, Type) %>% 
  ggplot(aes(x = t, y = mtemp)) + geom_point(aes(color = Type)) + 
  geom_line(linetype = "dashed", alpha = I(1/2)) + theme_bw() +
  ggtitle("Maxtemp Observed Values (December 2017)", 
          subtitle = "and Predictions (January 1-3 2018)") + 
  scale_x_continuous("Time") + scale_y_continuous("Maximum Temperature")

"To forecast the raw maximum temperature for Jan 1-3 for 2018, we need to manually add back the seasonality into the forecast of 
deseasonalized maximum temperature. Since the month is Januaray, this means all the dummy variables in our seasonality model = 0, 
meaning we just add the intercept to our deseasonalized values to get the raw maximum temperature prediction of Jan 1-3 for 2018."
