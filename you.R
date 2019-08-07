library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
setwd("~/youtube")

datax <- read.csv("videos.csv")
glimpse(datax)

dataxx <- datax[c(-1,-4,-5,-6,-7,-12,-13,-14,-15,-16)]
glimpse(dataxx)
sum(is.na(dataxx))
write.csv(dataxx, file = "dataxx.csv")

