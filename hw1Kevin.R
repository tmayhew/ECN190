library(readstata13)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(sandwich)
library(ggthemes)
Davis2018 <- read.dta13("C:/Users/ykc51/Desktop/Spring 2020/ECN190/Davis2018.dta")
head(Davis2018)

#1a
Davis2018$ClosingYear <- as.numeric(substr(Davis2018$ClosingDate,1,4))
Davis2018$ClosingMonth <- as.numeric(substr(Davis2018$ClosingDate,6,7))

###################################

#1b 
#Davis2018 %>% filter(SingleFamily == 1) %>% filter(ClosingYear == 2018)
Davis2018 = filter(Davis2018, SingleFamily == 1 & ClosingYear == 2018)
head(Davis2018)

###################################

#1c
Davis2018$Bedroom = as.factor(Davis2018$Bedroom)

tab = Davis2018 %>% group_by(Bedroom) %>% summarise(mean(SalePrice))
tab = data.frame(tab)

names(tab) = c("NumberBedrooms", "AvgSale")
print(tab)

pl = ggplot(data = tab, aes(x = NumberBedrooms, y = AvgSale)) + geom_bar(stat = "identity", width = I(1/7)) + 
  theme_bw() + scale_x_discrete("Number of Bedrooms") + 
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000), limits = c(0, 1570000)) + 
  ggtitle("Average Sale Price by Number of Bedrooms")
print(pl)

############################################

Davis2018$FullBath = as.factor(Davis2018$FullBath)

tab3 = Davis2018 %>% group_by(FullBath) %>% summarise(mean(SalePrice))
tab3 = data.frame(tab3)

names(tab3) = c("NumberFullBaths", "AvgSale")
print(tab3)

pl2 = ggplot(data = tab3, aes(x = NumberFullBaths, y = AvgSale)) + geom_bar(stat = "identity", width = I(1/5)) + 
  theme_bw() + scale_x_discrete("Number of FullBaths") + 
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000), limits = c(0, 1570000)) + 
  ggtitle("Average Sale Price by Number of FullBaths")
print(pl2)

############################################

Davis2018$ClosingMonth = as.factor(Davis2018$ClosingMonth)

tab3 = Davis2018 %>% group_by(ClosingMonth) %>% summarise(mean(SalePrice))
tab3 = data.frame(tab3)

names(tab3) = c("NumberClosingMonths", "AvgSale")
print(tab3)

pl3 = ggplot(data = tab3, aes(x = NumberClosingMonths, y = AvgSale)) + geom_bar(stat = "identity", width = I(1/5)) + 
  theme_bw() + scale_x_discrete("Number of ClosingMonths") + 
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000), limits = c(0, 1000000)) + 
  ggtitle("Average Sale Price by Months")
print(pl3)

############################################

Davis2018$Areacode = as.factor(Davis2018$Areacode)

tab4 = Davis2018 %>% group_by(Areacode) %>% summarise(mean(SalePrice))
tab4 = data.frame(tab4)

names(tab4) = c("NumberAreaCodes", "AvgSale")
print(tab4)

pl4 = ggplot(data = tab4, aes(x = NumberAreaCodes, y = AvgSale)) + geom_bar(stat = "identity", width = I(1/7)) + 
  theme_bw() + scale_x_discrete("Number of AreaCodes") + 
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000), limits = c(0, 1000000)) + 
  ggtitle("Average Sale Price by Area Code")
print(pl4)

ggarrange(pl, pl2, pl3, pl4, ncol = 2, nrow =2)

##########################################

#1d 

Davis2018$ClosingMonth = as.double(Davis2018$ClosingMonth)
model <- lm(Davis2018$SalePrice ~ Davis2018$ClosingMonth)
ggplot(data = Davis2018, aes(x = ClosingMonth, y = SalePrice)) + geom_point() + 
  theme_bw() + scale_x_continuous("Closing Month", breaks = seq(1, 12, 1), limits = c(0.75, 12.25)) +
  scale_y_continuous("Sale Price", breaks = seq(0, 1200000, by = 300000), limits = c(0, 1600000)) +
  geom_smooth(method = "lm", se = FALSE)

summary(model)

"From the regression of sale price on month of closing, we can say it is not significant from the pvalue of 0.7423,
with signifcance level 5%"

##########################################

#1e 
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# we used this t test to obtain the heteroskedasticity robust standard erorrs and their t values. 

##########################################

#1f

model2 <- lm(Davis2018$SalePrice ~ Davis2018$ListPrice + Davis2018$DaysOnMarket)
summary(model2)

df = cbind.data.frame(1:length(model2$residuals), model2$residuals)
names(df) = c("index", "residuals")
ggplot(data = df, aes(x = index, y = residuals)) + geom_point() + theme_clean() + geom_hline(yintercept = 0, linetype = "dashed") + scale_x_continuous("") + ggtitle("Residual Plot")

"The slope coefficients represent the change in the outcome variable based on the values of the independent variables.
For example, a one unit increase in list price leads to a 0.9839 increase in sale price. The same applies for days on market,
a one unit increase in days on market leads to a 385.3 decrease in sale price."

"From plotting the residuals, we can see on average that the zero conditonal mean condition is satisfied because the residuals
hover between -50000 and 50000. There are a few outliers but on average, the residuals are 0 if they were to be summed up, concluding that
the zero conditional mean condition is satisfied here"

##########################################

#1g 

model3 <- lm(Davis2018$SalePrice ~ Davis2018$ListPrice + Davis2018$DaysOnMarket + Davis2018$Bedroom + Davis2018$FullBath + Davis2018$Stories)
summary(model3)

"From our original regression, we added 3 house characteristics, number of bedrooms, number of fullbaths, and stories. With p value < 2.2e-16, 
which is close to 0, we can conclude there is joint significance of the newly added housing variables." 

##########################################

#1h

model4 <- lm(Davis2018$SalePrice ~ Davis2018$ListPrice + Davis2018$DaysOnMarket + I(Davis2018$DaysOnMarket^2))
summary(model4)

"Sale Price = 23301.9935 + List Price(0.9889) + Days on Market(-932.6248) + Days on Market^2(4.4529)"

"For houses with the same list prices, what is the predicted difference in sale
price if a house stays on market a week longer than the other?"

# House 1 = 23301.9935 + ListPrice(0.9889) + DaysOnMarket(-932.6248) + DaysOnMarket^2(4.4529)
# If days on market = 0, House 1 = 23301.9935 + ListPrice(0.9889)

# House 2 = 23301.9935 + ListPrice(0.9889) + DaysOnMarket(-932.6248) + DaysOnMarket^2(4.4529)
# If the house stays on market a week later, days on market = 7
# House 2 = 23301.9935 + ListPrice(0.9889) + 7 * (-932.6248) + 7^2 * (4.4529)
# House 2 = 23301.9935 + ListPrice(0.9889) - 6310.182 

"So the predicted difference in sale price if a house stays on market a week longer than the other = -6310.182"

##########################################
