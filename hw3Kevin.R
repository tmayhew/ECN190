library(tidyverse)
library(ggthemes)
library(Hmisc)

DavisWeather = read.csv("DavisWeather.csv")[,-1]
DavisWeather$t = 1:nrow(DavisWeather)
DavisWeather %>% ggplot(aes(x = t, y = maxtemp)) + geom_line() + theme_bw() +
  scale_x_continuous("Time") + scale_y_continuous("Maximum Temperature")

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
