library(ggplot2)
library(ggthemes)
library(dplyr)
library(wooldridge)
library(lmtest)
library(sandwich)
library(car)

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

"The mean value is not the same for all periods t, since the slope of the trend-line is negative over time, 
therefore we conclude that the GDP growth in the US between 1975 and 2019 is not stationary."

#2(i)
data(barium)
barium = barium %>% select(t, everything())
lm.model2 = lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 + t, data = barium)
summary(lm.model2)

"Only the linear time trend (t) is a significant variable in the model 
that includes all variables from equation (10.22) and t."

#(ii)
linearHypothesis(lm.model2, c("lchempi=0", "lgas=0", "lrtwex=0",
                              "befile6=0", "affile6=0", "afdec6=0"))

"According to the joint significance hypothesis test on all variables other than
t, the additional variables are not significant because the p-value (0.7767) 
indicates that the F-test is not significant at any reasonable level of alpha."

#(iii)
lm.model3 = lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6 + t +
               feb + mar + apr + may + jun + jul + aug + sep + oct + 
                 nov + dec, data = barium)
summary(lm.model3)
barium %>% ggplot(aes(x = t, y = lchnimp)) + geom_line() + geom_smooth(method = "lm", formula = "y~x", se = F) + theme_bw() + scale_x_continuous("Time")

for (i in 1:nrow(barium)){
  if (barium$feb[i] == 1){
    barium$month[i] = "2"
  }else if (barium$mar[i] == 1){
    barium$month[i] = "3"
  }else if (barium$apr[i] == 1){
    barium$month[i] = "4"
  }else if (barium$may[i] == 1){
    barium$month[i] = "5"
  }else if (barium$jun[i] == 1){
    barium$month[i] = "6"
  }else if (barium$jul[i] == 1){
    barium$month[i] = "7"
  }else if (barium$aug[i] == 1){
    barium$month[i] = "8"
  }else if (barium$sep[i] == 1){
    barium$month[i] = "9"
  }else if (barium$oct[i] == 1){
    barium$month[i] = "10"
  }else if (barium$nov[i] == 1){
    barium$month[i] = "11"
  }else if (barium$dec[i] == 1){
    barium$month[i] = "12"
  }else{
    barium$month[i] = "1"
  }
}

barium$month = factor(barium$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
barium %>% ggplot(aes(x = month, y = lchnimp)) + geom_boxplot() + theme_bw() + ggtitle("lchnimp by Month", subtitle = "Examining possible seasonality")

"There appears to be no seasonality. In examining the estimates and standard errors for the two models, the variables
from the model without months appear unchanged when months are added to the overall model. From these analyses, we 
would conclude that months play no factor in predicting lchnimp, which means there is no significant seasonality
to the time series data."





















