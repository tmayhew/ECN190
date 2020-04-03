# Problem 2
library(readstata13)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(sandwich)
library(ggthemes)

Davis2018 = read.dta13("Davis2018.dta")
Davis2018$ClosingYear <- as.numeric(substr(Davis2018$ClosingDate,1,4))
Davis2018$ClosingMonth <- as.numeric(substr(Davis2018$ClosingDate,6,7))
head(Davis2018)

Davis2018 = filter(Davis2018, ClosingYear == 2018 & SingleFamily == 1)
head(Davis2018)

#########################################################################################################

Davis2018$Bedroom = as.factor(Davis2018$Bedroom)
tab = Davis2018 %>% group_by(Bedroom) %>% summarise(mean(SalePrice))
tab = data.frame(tab)

names(tab) = c("NumberBedrooms", "AvgSale")
print(tab)
pl = ggplot(data = tab, aes(x = NumberBedrooms, y = AvgSale)) + 
  geom_bar(stat = "identity", width = I(1/3), color = "black", fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Number of Bedrooms") +
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000, 
                                                      1250000, 1500000), limits = c(0, 1570000)) + 
  ggtitle("Average Sale Price by Number of Bedrooms")

#########################################################################################################

Davis2018$FullBath = as.factor(Davis2018$FullBath)
tab = Davis2018 %>% group_by(FullBath) %>% summarise(mean(SalePrice))
tab = data.frame(tab)

names(tab) = c("NumberFullBaths", "AvgSale")
print(tab)
pl2 = ggplot(data = tab, aes(x = NumberFullBaths, y = AvgSale)) + 
  geom_bar(stat = "identity", width = I(1/5), color = "black", fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Number of Full Bathrooms") +
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000, 
                                                      1250000, 1500000), limits = c(0, 1570000)) + 
  ggtitle("Average Sale Price by Number of Full Bathrooms")


#########################################################################################################

Davis2018$ClosingMonth = as.factor(Davis2018$ClosingMonth)
tab = Davis2018 %>% group_by(ClosingMonth) %>% summarise(mean(SalePrice))
tab = data.frame(tab)

names(tab) = c("NumberClosingMonths", "AvgSale")
print(tab)
pl3 = ggplot(data = tab, aes(x = NumberClosingMonths, y = AvgSale)) + 
  geom_bar(stat = "identity", width = I(1/5), color = "black", fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Month") +
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000), limits = c(0, 1050000)) + 
  ggtitle("Average Sale Price by Month")


#########################################################################################################

Davis2018$Areacode = as.factor(Davis2018$Areacode)
tab = Davis2018 %>% group_by(Areacode) %>% summarise(mean(SalePrice))
tab = data.frame(tab)

names(tab) = c("NumberAreacodes", "AvgSale")
print(tab)
pl4 = ggplot(data = tab, aes(x = NumberAreacodes, y = AvgSale)) + 
  geom_bar(stat = "identity", width = I(1/7), color = "black", fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Area Code") +
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000), limits = c(0, 1050000)) + 
  ggtitle("Average Sale Price by Area Code")

ggarrange(pl, pl2, pl3, pl4, ncol = 2, nrow = 2)


#########################################################################################################

Davis2018$ClosingMonth = as.double(Davis2018$ClosingMonth)
lm.model = lm(SalePrice ~ ClosingMonth, data = Davis2018)
ggplot(data = Davis2018, aes(x = ClosingMonth, y = SalePrice)) + geom_point() +
  theme_bw() + scale_x_continuous("Closing Month", breaks = seq(1, 12, 1), limits = c(0.75, 12.25)) +
  scale_y_continuous("Sale Price", breaks = seq(0, 1500000, by = 150000), limits = c(0, 1550000)) +
  geom_smooth(method = "lm", se = F) + ggtitle("Closing Month versus Sale Price")
summary(lm.model)

"It appears that regressing Closing Month on Sale Price is not effective; the p-value
is 0.7423, far from significant at the 5% level."

#########################################################################################################

"We would carry out the t-test to obtain heteroskadasticity robust standard errors and their t-values."
coeftest(lm.model, vcov = vcovHC(lm.model, type = "HC0"))

#########################################################################################################

"Run a regression of sale price on list price and days on market. How do you
interpret the slope coefficients of this regression? Do you think the zero
conditional mean condition is satisfied here?"

Davis2018$DaysOnMarket = as.double(Davis2018$DaysOnMarket)
Davis2018$ListPrice = as.double(Davis2018$ListPrice)
lm.model2 = lm(SalePrice ~ ListPrice + DaysOnMarket, data = Davis2018)
print(lm.model2)

"The slope coefficients represent the increase or decrease in the dependent variable
based on values of the independent variables; in this case, the coefficient for
ListPrice (0.9839) is the increase in SalePrice with every 1-unit increase in 
ListPrice, and the coefficient for DaysOnMarket (-385.2674) is the decrease
in SalePrice associated with every 1-unit increase in DaysOnMarket.

In other words, the ListPrice is slightly lower than SalePrice at every level, 
but mirrors its characteristics, while for every day on the market, a house loses
around 385 dollars in value."

df1 = Davis2018 %>% select(ListPrice, DaysOnMarket, SalePrice)
df = cbind.data.frame(df1, lm.model2$residuals, lm.model2$fitted.values)
df = 
  df %>% arrange(`lm.model2$fitted.values`) #%>% select()
df$Index = 1:nrow(df)
df = 
  df %>% select(Index, `lm.model2$residuals`)
names(df) = c("Index", "Residuals")
ggplot(data = df, aes(x = Index, y = Residuals)) + geom_point() + 
  theme_clean() + geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous("") + ggtitle("Residual Plot")

# Zero Conditional Mean:
"From plotting the residuals, we can see on average that the zero conditonal mean 
condition is satisfied because the residuals hover between -50000 and 50000, and the 
expected value is constant at all levels. There are a few outliers but on average, 
the residuals are 0 if they were to be summed up, allowing us to conclude a zero 
conditional mean."


#########################################################################################################

lm.model3 = lm(SalePrice ~ ListPrice + DaysOnMarket + Bedroom + FullBath + Stories, data = Davis2018)
summary(lm.model3)

# House characteristics:
"From our original regression, we added 3 house characteristics, number of bedrooms, number of fullbaths, and stories. With p value < 2.2e-16, 
which is close to 0, we can conclude there is joint significance of the newly added housing variables." 


#########################################################################################################

lm.model4 = lm(SalePrice ~ ListPrice + DaysOnMarket + I(DaysOnMarket^2), data = Davis2018)

#Quadratic Model:
#  23301.9935 + ListPrice(0.9889) + DaysOnMarket(-932.6248) + DaysOnMarket^2(4.4529)

"If two houses have identical List Prices, and one stays on the market 1 week longer:"
# Let C = 23301.9935 + ListPrice(0.9889)

# House 1 = C + 0
# House 2 = C + 7*(-932.6248) + 49*(4.4529)
#         = C - 6310.182

"The house that stays on the market 7 days longer loses $6,310.18 in value."


#########################################################################################################

rentaldata = read.dta13("RENTAL.DTA")
summary(rentaldata)


# 2b ####################################################################################################

rentaldata2b = 
  rentaldata %>% select(city, year, clrent, lrent, rent)
head(rentaldata2b)
for (i in 1:nrow(rentaldata2b)){
  if (i %% 2 == 0){
    rentaldata2b$clrent.calc[i] = rentaldata2b$lrent[i] - rentaldata2b$lrent[i-1]
    
  } else{
    rentaldata2b$clrent.calc[i] = 0
  }
}

rentaldata2b.omit = na.omit(rentaldata2b)
print(head(rentaldata2b.omit, 10))
all(rentaldata2b.omit$clrent == rentaldata2b.omit$clrent.calc) # this line verifies that the clrent variable is equal to the change in the lrent variable in each city.

"For city 1, the clrent is equal to 0.5516071. This means that in city 1, the rent in 1990 
was 55.16% higher than it was in 1980, or there was a 55.16% change in rent from 1980 to 1990."

# 2c ####################################################################################################

head(rentaldata)
rentaldata.omit = na.omit(rentaldata)
rentaldata.omit = 
  rentaldata.omit %>% select(clrent, clpop, clavginc, cpctstu)
lm.modelc = lm(clrent ~ clpop + clavginc + cpctstu, data = rentaldata.omit)

"The intercept (0.385521) is the percent change in rent that would occur without any change 
in population, average income, or percentage of students; even if nothing else in the model
changes, the rent would still increase by around 38.5%."








