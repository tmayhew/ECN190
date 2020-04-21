library(tidyverse)
library(sandwich)
library(ggthemes)
library(knitr)
library(car)
library(Hmisc)

#1
DavisWeather = read.csv("DavisWeather.csv")[,-1]
DavisWeather$t = 1:nrow(DavisWeather)
DavisWeather = DavisWeather %>% select(t, everything())
DavisWeather %>% ggplot(aes(x = t, y = maxtemp)) + geom_line() + theme_bw() +
  scale_x_continuous("Time") + scale_y_continuous("Daily Maximum Temperature") #+ geom_smooth(method = "lm", formula = "y~x", se = F)

"The maxtemp data is not stationary because the expected value of maxtemp changes
over time (t); from time 0 to 200, maxtemp increases over time, from 200-400 maxtemp 
decreases over time, and the pattern repeats from 400-600 and 600-730. The expected
value of temp is not constant for all levels of t, meaning the ZCM is not satisfied."

#2
DavisWeather$Feb = ifelse(DavisWeather$month == 2, 1, 0)
DavisWeather$Mar = ifelse(DavisWeather$month == 3, 1, 0)
DavisWeather$Apr = ifelse(DavisWeather$month == 4, 1, 0)
DavisWeather$May = ifelse(DavisWeather$month == 5, 1, 0)
DavisWeather$Jun = ifelse(DavisWeather$month == 6, 1, 0)
DavisWeather$Jul = ifelse(DavisWeather$month == 7, 1, 0)
DavisWeather$Aug = ifelse(DavisWeather$month == 8, 1, 0)
DavisWeather$Sep = ifelse(DavisWeather$month == 9, 1, 0)
DavisWeather$Oct = ifelse(DavisWeather$month == 10, 1, 0)
DavisWeather$Nov = ifelse(DavisWeather$month == 11, 1, 0)
DavisWeather$Dec = ifelse(DavisWeather$month == 12, 1, 0)

# Regressing MaxTemp on monthly dummies
seasonal.model = lm(maxtemp ~ t + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + 
                      Oct + Nov + Dec, data = DavisWeather)
"Ho = B2 + B3 + B4 + ... + B12 = 0"
"Ha = B2 + B3 + B4 + ... + B12 != 0"

linearHypothesis(seasonal.model, c("Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + 
                                   Oct + Nov + Dec = 0"))

#3
DavisWeather$maxtemphat = predict(seasonal.model)

df1 = cbind.data.frame(X = DavisWeather$t, Y = DavisWeather$maxtemp, Type = "Real")
tobind = cbind.data.frame(X = DavisWeather$t, Y = DavisWeather$maxtemphat, Type = "Predicted")
df2 = rbind.data.frame(df1, tobind)
ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + 
  geom_line(data = subset(df2, Type = "Predicted"), 
            linetype = "dashed", color = "steelblue", 
            alpha = I(3/4)) + geom_line() + 
  theme_bw() + scale_y_continuous("Daily Maximum Temperature") + 
  scale_x_continuous("Time") + ggtitle("Maxtemp vs. Time: Real and Fitted Lines", subtitle = "Seasonal Model")

"The model appears to fit the data well. This result is likely 
because the model has very significant seasonality, so when we 
added time trend and dummy season variables, the predicted values
from the new model fit the data very closely."

#4
DavisSummer = DavisWeather %>% filter(month > 5, month < 10)
DavisSummer$t = 1:nrow(DavisSummer)
DavisSummer %>% ggplot(aes(x = t, y = maxtemp)) + geom_line() + theme_bw() +
  scale_x_continuous("Time") + scale_y_continuous("Maximum Temperature")

DavisSummer$L.maxtemp <- Lag(DavisSummer$maxtemp,1)
DavisSummer$L2.maxtemp <- Lag(DavisSummer$maxtemp,2)

cor(DavisSummer$maxtemp[2:244], DavisSummer$L.maxtemp[2:244])
cor(DavisSummer$maxtemp[3:244], DavisSummer$L2.maxtemp[3:244])
acf(DavisSummer$maxtemp, main = "ACF of Adjusted Maximum Temperature")

"From both the correlation coefficient and the acf plot, it does not appear that the 
daily maxiumum temperature from June to Septmber are weakly dependent. The correlation 
between maximum temperature with lag1 and lag2 are 0.77 and 0.48 respectively, this
tells us there is some correlation between each time period of the maximum temperature.
Also using the ACF plot, we can see that the data is highly correlated and it captures 
seasonlity. As lags increase, the correlation between yt and yt-p does not go to 0."

#5
detrend.model = lm(maxtemp ~ t + Jul + Aug + Sep, data = DavisSummer)
DavisSummer$maxtempdet = detrend.model$residuals
DavisSummer$L.maxtempdet = Lag(DavisSummer$maxtempdet,1)
ar1.model = lm(maxtempdet ~ L.maxtempdet, data = DavisSummer)
summary(ar1.model)

"The slope coefficient of this regression represents beta1 in an AR1 detrended Model. 
This coefficient represents the effect the last time period has on the next time period. 
For every increase in t, daily detrended maximum temperature increases by 0.7348 multiplied
by the detrended maximum temperature from the previous time period."

#6
DavisSummer$maxtemphatdet[2:nrow(DavisSummer)] = ar1.model$fitted.values
DavisSummer1 = DavisSummer %>% filter(year == 2017 & (Jul == 1 | Aug == 1))
df1 = cbind.data.frame(X = DavisSummer1$t, Y = DavisSummer1$maxtempdet, Type = "Real")
tobind = cbind.data.frame(X = DavisSummer1$t, Y = DavisSummer1$maxtemphatdet, Type = "Predicted")
df2 = rbind.data.frame(df1, tobind)
ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + 
  geom_line(data = subset(df2, Type = "Predicted"), 
            linetype = "dashed", color = "steelblue", 
            alpha = I(3/4)) + geom_line() + 
  theme_bw() + scale_y_continuous("Daily Maximum Temperature") + 
  scale_x_continuous("Time") + ggtitle("Maxtemp vs. Time for de-Trended Data: Real and Fitted Lines", subtitle = "AR(1) Model")


#7
"If we would like to carry out an AR(1) model for the whole time series in 2017 and 2018,
we can control for seasonality by adding in monthly dummary variables in the model."


#8
bgtest(ar1.model)

"Reject the null hypothesis of zero serial correlation and conclude (based on the p-value)
that there is serial correlation in the AR(1) model from Q5. The error terms in the AR(1)
model are correlated."



