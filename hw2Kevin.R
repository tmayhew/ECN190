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


