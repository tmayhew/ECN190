library(tidyverse)
library(Hmisc)
library(lmtest)

DavisWeather = read.csv("DavisWeather.csv")[,-1]
head(DavisWeather)

#1 
Davis2017 = DavisWeather %>% filter(year == 2017)
seasonal.model = lm(maxtemp ~ as.factor(month), data = Davis2017)
Davis2017$adj = seasonal.model$residuals
Davis2017$t = 1:nrow(Davis2017)
plot(Davis2017$t, Davis2017$adj, type = "l")

#2
acf(Davis2017$adj)
pacf(Davis2017$adj)

"We see a sudden drop in the PACF plot but a gradual decline in the ACF plot, 
this means an AR model should be used for this time series data."

#3

"The p-value from this BG test is less than 0.0, this means we reject H0 and conclude that there is
serial correlation in this model. We should consider adding more lags into this model."

#4
Davis2017$L.adj <- Lag(Davis2017$adj, 1)
Davis2017$L2.adj <- Lag(Davis2017$adj, 2)

AR2 <- lm(adj ~ L.adj + L2.adj, data = Davis2017)
summary(AR2)
bgtest(AR2)

"The p-value from this BG test is greater than 0.05, this means we failt to reject HO
and conclude that there is no serial correlation in this model. This makes sense as previosuly,
we concluded there is evidence of serial correlation. We fix this by adding more lags, which is 
what we did by using an AR(2) model."

#5
library(forecast)
auto.arima(Davis2017$adj)

"The R command picks an ARMA(2,1) model"

#6

results <- auto.arima(Davis2017$adj)
f = forecast(results, h = 3, level = 95)
f$mean + 
  
"To forecast the raw maximum temperature for Jan 1-3 for 2018, we need to manually add back the seasonality into the forecast of 
deseasonalized maximum temperature. Since the month is Januaray, this means all the dummy variables in our seasonality model = 0, 
meaning we just add the intercept to our deseasonalized values to get the raw maximum temperature prediction of Jan 1-3 for 2018."

