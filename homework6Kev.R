#1 
rm(list=ls())
library("quantmod")
getSymbols(c("GS1M","GS3M","GS6M","GS1","GS2","GS3","GS5","GS7","GS10","GS20","GS30"), src = "FRED")

#2 
date = "2007-03-01"
Trates<- cbind(GS3M[index(GS3M)==date],GS20[index(GS20)==date])
Trates
"There is a slightly higher yield for a 3 month T-bill in March 2007 than a 20 year T-bond.
This means the return on the 3 month T-bill is higher than the reteun on the 20 year T-bond"

#3
Trates2 <-cbind(GS1M[index(GS1M)==date],GS3M[index(GS3M)==date],GS6M[index(GS6M)==date],
              GS1[index(GS1)==date],GS2[index(GS2)==date],GS3[index(GS3)==date],
              GS5[index(GS5)==date], GS7[index(GS7)==date],GS10[index(GS10)==date],
              GS20[index(GS20)==date],GS30[index(GS30)==date])
maturity<-c(1/12,3/12,6/12,1,2,3,5,7,10,20,30)
plot(maturity,Trates2, main=paste("Yield Curve in ", date, sep=""))

#4
library("YieldCurve")
NSresults <- Nelson.Siegel(rate = Trates2, maturity = maturity)
NSresults
beta0 <- NSresults[,1]
beta1 <- NSresults[,2]
beta2 <- NSresults[,3]
"Beta0 is the long-term interest rate, which is `r beta0`%. Beta1 is the long-to-short-term spread, which means the 
difference between the long term interest rate and the short term interest rate, which is `r beta1`%. Beta2 is the curvature
parameter and since beta2 is less than 0, the curve produces a trough, which means it is inverted. Beta2 = `r beta2`%."

#5
predict <- NSrates(NSresults, 2.5)

"Our Nelson-Siegel model predicts `r predict`% as the annual yield of a US treasury bond in the secondary market in March 2007
with 2.5 years left to maturity"

#6 