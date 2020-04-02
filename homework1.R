# Problem 2
library(readstata13)
library(dplyr)
library(ggplot2)
library(ggpubr)

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
  geom_bar(stat = "identity", width = I(1/3), fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Number of Bedrooms") +
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
  geom_bar(stat = "identity", width = I(1/5), fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Number of Full Bathrooms") +
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
  geom_bar(stat = "identity", width = I(1/5), fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Month") +
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000), limits = c(0, 1050000)) + 
  ggtitle("Average Sale Price by Month")


#########################################################################################################

Davis2018$Areacode = as.factor(Davis2018$Areacode)
tab = Davis2018 %>% group_by(Areacode) %>% summarise(mean(SalePrice))
tab = data.frame(tab)

names(tab) = c("NumberAreacodes", "AvgSale")
print(tab)
pl4 = ggplot(data = tab, aes(x = NumberAreacodes, y = AvgSale)) + 
  geom_bar(stat = "identity", width = I(1/7), fill = "#F7CAC9") + theme_bw() + scale_x_discrete("Area Code") +
  scale_y_continuous("Average Sale Price", breaks = c(0, 250000, 500000, 750000, 1000000), limits = c(0, 1050000)) + 
  ggtitle("Average Sale Price by Area Code")

ggarrange(pl, pl2, pl3, pl4, ncol = 2, nrow = 2)



