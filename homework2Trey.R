tableb1 = read.csv("tableb1.csv")[,-1]
tableb27 = read.csv("tableb27.csv")[,-1]
USeconomy = cbind.data.frame(year = tableb1$Yr, gdpgrowth = tableb1$GDP, ue = tableb27$Unemployment.Rate)
tail(USeconomy)

