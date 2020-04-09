tableb1 = read.csv("GDPdata.csv")[,-1]
tableb27 = read.csv("Unemployment.csv")[,-1]

USeconomy = cbind.data.frame(year = tableb1$Yr, gdpgrowth = tableb1$GDP, ue = tableb27$Unemployment.Rate)
tail(USeconomy)

