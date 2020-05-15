library(quantmod)
library(tidyverse)
library(forecast)
library(anytime)
library(fGarch)

HW5CP = new.env()
getSymbols("EUR=X", env = HW5CP, src="yahoo", from = "2014-01-01", to = "2016-12-31")
EUR <- HW5CP[["EUR=X"]]
close <- EUR$`EUR=X.Close`

# 1 ########################################################################

"The exchange rate in the first trading day of 2014 is 0.72754. This means
that the closing exchange rate for January 1, 2014 was 1 U.S. dollar to 0.7275
euros."

# 2 ########################################################################
close <- EUR$`EUR=X.Close`
close$t = 1:nrow(close);close = close[,c(2,1)]
close = as.data.frame(close)
close$date = rownames(close)
colnames(close) = c("t", "exr", "date")
close$date = anydate(close$date)
close %>% ggplot(aes(x = date, y = exr)) + geom_line() + theme_bw() + ggtitle("Daily Exchange Rate", subtitle = " EUR to USD") + scale_x_date("Time") + scale_y_continuous("Exchange Rate")

close$percentchange = (close$exr - Lag(close$exr, 1))/Lag(close$exr, 1)
close[2:nrow(close),] %>% ggplot(aes(x = t, y = percentchange)) + geom_line() + theme_bw() + ggtitle("Daily Percent Change", subtitle = " EUR to USD")+ scale_x_continuous("Time") + scale_y_continuous("Percent Change")

# 3 ########################################################################

auto.arima(close$percentchange)

"The auto.arima function is predicting that the series is MA(1)."

auto.arima(close$percentchange, ic = "bic")

"If the information criteria is changed to BIC, then an ARMA(0, 0) model
is selected by the auto.arima function."

# 4 ########################################################################

results <- garchFit(formula = ~ garch(1, 1), data = close$percentchange[-1], trace = F)
results

# sigma-sq-t = 1.618e-07 + 4.858e-02*(ut-1) + 9.497e-01*(sigma-sq-(t-1))

# 5 ########################################################################
return = close$percentchange[-1]
dates = close$date[-1]
volatility_longterm<-rep(sd(return),length(return))
plot(dates,return,type="l")
lines(dates,mean(return)+2*volatility_longterm,col="red")
lines(dates,mean(return)-2*volatility_longterm,col="red")










