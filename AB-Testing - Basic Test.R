########## MARKETING - AB-TEST
library(tidyverse)

#### Daten einlesen
file <- "~/Documents/Data/ABtestClickThrough.csv"
data <- read_csv(file) 

#### Daten vorbereiten
str(data) 
names(data) <- c("group", "time", "clickedTrue") 
data$group <- as_factor(data$group) 
data$clickedTrue <- as_factor(data$clickedTrue) 
levels(data$clickedTrue) <- c("0", "1")
glimpse(data)

#### Häufigkeitstabelle
(freqTable <- table(data$group, data$clickedTrue))
##       1   0
##   A  20 480
##   B  40 460

#### Signifikanztest
prop.test(freqTable, conf.level = .95)
# => p-value = 0.01141
## sample estimates:
## prop 1 prop 2 
##   0.04   0.08

