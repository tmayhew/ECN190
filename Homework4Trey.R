library(tidyverse)
library(ggthemes)
library(Hmisc)
library(lmtest)
library(forecast)

# https://zoom.us/j/98496095876?pwd=UkhhR24wMXg0T09MR2YvNGFaWk5pUT09
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

# 3
dw$L1.deseas = Lag(dw$deseas, 1)
ar1.model = lm(deseas ~ L1.deseas, data = dw)
bgtest(ar1.model)
  # Serial correlations exist in error terms

# 4
dw$L2.deseas = Lag(dw$deseas, 2)
ar2.model = lm(deseas ~ L1.deseas + L2.deseas, data = dw)
bgtest(ar2.model)
  # No serial correlation in error terms

# 5
auto.model = auto.arima(dw$deseas);auto.model

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
  
  
  
