library(tidyverse)
library(ggthemes)

DavisWeather = read.csv("DavisWeather.csv")[,-1]
DavisWeather$t = 1:nrow(DavisWeather)
DavisWeather %>% ggplot(aes(x = t, y = maxtemp)) + geom_line() + theme_bw() +
  scale_x_continuous("Time") + scale_y_continuous("Maximum Temperature")
