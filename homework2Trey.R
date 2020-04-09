library(ggplot2)
library(ggthemes)
library(dplyr)

tableb1 = read.csv("GDPdata.csv")[,-1]
tableb27 = read.csv("Unemployment.csv")[,-1]

#1A
USeconomy = cbind.data.frame(year = tableb1$Yr, gdpgrowth = tableb1$GDP, ue = tableb27$Unemployment.Rate)
print(head(USeconomy, 10))

#1B
USeconomy$min = ifelse(USeconomy$gdpgrowth == min(USeconomy$gdpgrowth), "Min", "N")
USeconomy %>% ggplot(aes(x = year, y = gdpgrowth)) + geom_line() + theme_bw() + 
  geom_point(aes(color = min)) + theme(legend.position = "none") + scale_color_manual(values = c("red3", "black"))
  
"The minimum value for gdpgrowth is: "
min(USeconomy$gdpgrowth)

"This minimum occurred in: "
USeconomy$year[which(USeconomy$gdpgrowth == min(USeconomy$gdpgrowth))]

#1C
USeconomy$min2 = ifelse(USeconomy$ue == min(USeconomy$ue), "Min", "N")
USeconomy %>% ggplot(aes(x = year, y = ue)) + geom_line() + theme_bw() + 
  geom_point(aes(color = min2)) + theme(legend.position = "none") + scale_color_manual(values = c("red3", "black"))

"The minimum value for ue is: "
min(USeconomy$ue)

"This minimum occurred in: "
USeconomy$year[which(USeconomy$ue == min(USeconomy$ue))]

#1D
USeconomy$t = seq(1, nrow(USeconomy))
USeconomy =
  USeconomy %>% select(t, year, gdpgrowth, ue)
trnd.model = lm(gdpgrowth~t, data = USeconomy)

USeconomy %>% ggplot(aes(x = t, y = gdpgrowth)) + geom_line() + theme_bw() + 
  geom_smooth(method = "lm", formula = "y~x", se = F) + scale_x_continuous("Time")
summary(trnd.model)

#1E
X = USeconomy$gdpgrowth
Xlag = X %>% lag()
lag.df = cbind.data.frame(gdpgrowth = X, FirstDiff = Xlag)
lag.df %>% ggplot(aes(x = gdpgrowth, y = FirstDiff)) + geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = F) + theme_bw() + scale_y_continuous("First Lag of gdpgrowth")

"The association between gdpgrowth and its first lag is positive, but the correlation between the two
appears to be weak."

#1F
par(mfrow = c(1, 1))
gdpgrowth = X
acf(gdpgrowth, lag.max = 16)
acf(gdpgrowth, lag.max = 16, plot = F)

"Not stationary"











