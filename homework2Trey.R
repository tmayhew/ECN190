tableb1 = read.csv("tableB1.csv")
head(tableb1)
tableb1$Yr = as.character(tableb1$Yr)
tableb1 = tableb1[7:51,]
tableb1$Yr = seq(1975, 2019)
tableb27 = read.csv("TableB27.csv")
names(tableb27)[1] = "Yr"
tableb27 = tableb27[1:45,]

write.csv(tableb1, "tableb1.csv")
write.csv(tableb27, "tableb27.csv")








USeconomy = cbind.data.frame(tableb1$Yr, tableb1$GDP, tableb27$Unemployment.Rate)
