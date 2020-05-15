library("quantmod")
library(Hmisc)
library(forecast)
library(fGarch)
library(anytime)

HW5CP <- new.env()
getSymbols("EUR=X",env = HW5CP, src="yahoo",from = "2014-01-01",to = "2016-12-31")

EUR<-HW5CP[["EUR=X"]]
close<-EUR$`EUR=X.Close`
close$t = 1:nrow(close);close = close[,c(2,1)]
close = as.data.frame(close)
close$date = rownames(close)
colnames(close) = c("t", "exr", "date")
close$date = anydate(close$date)

#1 
close[1,1]

"The exchange rate in the first trading day of 2014 is the exchange rate between Euros and US dollars. 
On Janurary 1, 2014, 1 US dollar was worth 0.72754 euros."

#2
plot(close$EUR.X.Close, type = "l", main = "")
title(main = "Daily Closing Exchange Rate between Euro and US Dollars", ylab = "Exchange Rate")

close$pchange <- (close - Lag(close,1))/Lag(close,1)
head(pchange)
head(close)

plot(close$pchange, type = "l", main = "")
title(main = "Percent Change in Exchange Rate between Euro and US Dollars", ylab = "Percent Change")


"The exchange rate in the second half of 2014 and first half of 2015 increased. You can see from both 
plots that the exchange rate and the percent change of the exchange rate increased during that period.
In the percent change plot, the perent change was between -10% and 10% prior to July 1st, 2014, then 
the percent changed increased to over -20% and 20% change in days after the second half of 2014 "

#3
auto.arima(close$pchange)

"The suggest model from the output is MA(1)"

auto.arima(close$pchange, ic = "bic")

"Using the BIC criterion, the output suggested an ARMA(0,0) model"

#4 
results <- garchFit(formula = ~ garch(1,1), data = close$pchange[-1], trace = F)
mu = coef(results)[1]
omega = coef(results)[2]
alpha1 = coef(results)[3]
beta1 = coef(results)[4]

"sigma^2t = omega + alpha1(u^2t-1) + beta1(sigma^2t-1)" 

#5
volatility_longterm<-rep(sd(close$pchange), length(close$pchange))
plot(dates, close$pchange, type = "l")
lines()

