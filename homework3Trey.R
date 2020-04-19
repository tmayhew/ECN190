library(tidyverse)
library(sandwich)
library(ggthemes)
library(knitr)
library(car)


DavisWeather = read.csv("DavisWeather.csv")[,-1]
DavisWeather$t = 1:nrow(DavisWeather)
DavisWeather = DavisWeather %>% select(t, everything())
DavisWeather %>% ggplot(aes(x = t, y = maxtemp)) + geom_line() + theme_bw() +
  scale_x_continuous("Time") + scale_y_continuous("Daily Maximum Temperature") #+ geom_smooth(method = "lm", formula = "y~x", se = F)

"The maxtemp data is not stationary because the expected value of maxtemp changes
over time (t); from time 0 to 200, maxtemp increases over time, from 200-400 maxtemp 
decreases over time, and the pattern repeats from 400-600 and 600-730. The expected
value of temp is not constant for all levels of t, meaning the ZCM is not satisfied."

head(DavisWeather)

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

DavisWeather$maxtemphat = predict(seasonal.model)

df1 = cbind.data.frame(X = DavisWeather$t, Y = DavisWeather$maxtemp, Type = "Real")
tobind = cbind.data.frame(X = DavisWeather$t, Y = DavisWeather$maxtemphat, Type = "Predicted")
df2 = rbind.data.frame(df1, tobind)

ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + 
  geom_line(data = subset(df2, Type = "Predicted"), 
            linetype = "dashed", color = "steelblue", 
            alpha = I(3/4)) + geom_line() + 
  theme_bw() + scale_y_continuous("Daily Maximum Temperature") + 
  scale_x_continuous("Time") + ggtitle("Maxtemp vs. Time: Real and Fitted Lines") +
  theme(legend.position = "right")

"The model appears to fit the data well. This result is likely 
because the model has very significant seasonality, so when we 
added time trend and dummy season variables, the predicted values
from the new model fit the data very closely."







