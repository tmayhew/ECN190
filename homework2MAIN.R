# https://zoom.us/j/163981126?pwd=TFJtZ05hWGpNOFJ2cXpsQzdwNmZKdz09

library(ggplot2)
library(ggthemes)
library(dplyr)

tableb1 = read.csv("GDPdata.csv")[,-1]
tableb27 = read.csv("Unemployment.csv")[,-1]

#1a
USeconomy = cbind.data.frame(year = tableb1$Yr, gdpgrowth = tableb1$GDP, ue = tableb27$Unemployment.Rate)

#1b 
USeconomy$min = ifelse(USeconomy$gdpgrowth == min(USeconomy$gdpgrowth), "Min", "N")
USeconomy %>% ggplot(aes(x = year, y = gdpgrowth)) + geom_line() + theme_bw() + 
  geom_point(aes(color = min)) + theme(legend.position = "none") + scale_color_manual(values = c("red3", "black"))

"The minimum GDP growth is -2.8% and it happened in 2008"

#1c 
USeconomy$minUE = ifelse(USeconomy$ue == min(USeconomy$ue), "Min", "N")
USeconomy %>% ggplot(aes(x = year, y = ue)) + geom_line() + theme_bw() + 
  geom_point(aes(color = minUE)) + theme(legend.position = "none") + scale_color_manual(values = c("red3", "black"))

"The minimum unemployment rate is 3.7% and it happened in 2019"

#1d 
USeconomy$t <- seq(1, nrow(USeconomy))
USeconomy = USeconomy %>% select(t, year, gdpgrowth, ue)
model <- lm(gdpgrowth ~ t, data = USeconomy)
summary(model)

USeconomy %>% ggplot(aes(x = t, y = gdpgrowth)) + geom_line() + theme_bw() + 
  geom_point() + theme(legend.position = "none") + 
  scale_color_manual(values = c("red3", "black")) + geom_smooth(method = "lm", formula = "y~x", se = F) 

"The intercept coefficient is 3.71980 "

"The slope coefficient is -0.04108. This measures the change in gdp growth from one period to the next. So if t increases by
one unit (one year), gdp growth will decrease by 0.04%" 

#1e
X = USeconomy$gdpgrowth
Xlag = X %>% lag()
lag.df = cbind.data.frame(gdpgrowth = X, FirstDiff = Xlag)
lag.df %>% ggplot(aes(x = gdpgrowth, y = FirstDiff)) + geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = F) + theme_bw() + scale_y_continuous("First Lag of gdpgrowth")

"The association between gdpgrowth and its first lag is positive, but the correlation between the two
appears to be weak."

#1f
par(mfrow = c(1, 1))
gdpgrowth = X
acf(gdpgrowth, lag.max = 16)
acf(gdpgrowth, lag.max = 16, plot = F)

"Not stationary"














