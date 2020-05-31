rm(list=ls())
library("quantmod")
library(tidyverse)
library("YieldCurve")

"1. Obtain the monthly data of interest rate (aka. annual yield) of different U.S. treasury
bonds with maturities 1-month, 3-month, 6-month, 1-year, 2-year, 3-year, 5-year, 7-year,
10-year, 20-year, and 30-years. (See codes in the R example file.)"

getSymbols(c("GS1M","GS3M","GS6M","GS1","GS2","GS3","GS5","GS7","GS10","GS20","GS30"), src = "FRED")


"2. Find the (annual) yield of a 3-month T-bill in March 2007. Then find the (annual) yield
of 20-year T-bond in the same month. (Find those numbers from the time series you
                                      loaded into R in the last question. Note that the unit of the yield data stored in R is
                                      percentage point. That is if you get a 4 from your data it means a 4% or 0.04 annual
                                      yield.) What would you comment on those numbers?
"

date = "2007-03-01"
Trates<- cbind(GS3M[index(GS3M)==date],GS20[index(GS20)==date])
Trates
"There is a slightly higher yield for a 3 month T-bill in March 2007 than a 20 year T-bond.
This means the return on the 3 month T-bill is higher than the reteun on the 20 year T-bond"



"3. Now, draw a scatter plot of the yield data of all bonds in March 2007 against their
maturity dates (you will have 11 data points in the scatter plot)."

Trates2 <-cbind(GS1M[index(GS1M)==date],GS3M[index(GS3M)==date],GS6M[index(GS6M)==date],
                GS1[index(GS1)==date],GS2[index(GS2)==date],GS3[index(GS3)==date],
                GS5[index(GS5)==date], GS7[index(GS7)==date],GS10[index(GS10)==date],
                GS20[index(GS20)==date],GS30[index(GS30)==date])
maturity<-c(1/12,3/12,6/12,1,2,3,5,7,10,20,30)
plot(maturity,Trates2, main=paste("Yield Curve in ", date, sep=""))


"4. Fit a yield curve using the Nelson-Siegel model for March 2007. 
Interpret your B0 and B1 estimates. Also, find the sign of your B2 estimate. What does the sign tell you?"

NSresults <- Nelson.Siegel(rate = Trates2, maturity = maturity)
NSresults
beta0 <- NSresults[,1]
beta1 <- NSresults[,2]
beta2 <- NSresults[,3]
"Beta0 is the long-term interest rate, which is `r beta0`%. Beta1 is the long-to-short-term spread, which means the 
difference between the long term interest rate and the short term interest rate, which is `r beta1`%. Beta2 is the curvature
parameter and since beta2 is less than 0, the curve produces a trough, which means it is inverted. Beta2 = `r beta2`%."


"Beta0 represents the long-term interest rate estimated by the Nelson-Siegel model; in this case,
the long-term interest rate is estimated to be 4.92%. Beta1 represents the estimated difference
between long-term interest rate and short-term interest rate, in this case, the estimate is
0.33995. The sign of Beta2 is negative, which produces a trough, indicating that the yield curve 
is inverted."


"5. What does your Nelson-Siegel model predict about the annual yield of U.S. treasury
bonds in the secondary market in March 2007 that has 2.5 years left to maturity?"

predict <- NSrates(NSresults, 2.5)
predict

"Our Nelson-Siegel model predicts `r predict`% as the annual yield of a US treasury bond in the secondary market in March 2007
with 2.5 years left to maturity"

'6. Suppose there is a zero-coupon bond with face value $1000 that in March 2007 has 2.5
years left to maturity. Calculate the predicted price of this zero-coupon bond using your
predicted yield in the last question. You could use either one of the following
compounding formulae (for a zero-coupon bound). (Note gain that the unit of the yield
                                                 data stored in R is percentage point.)'
1000/((1+(predict[1,1]/100))^2.5)
1000/(exp((predict[1,1]/100)*2.5))

