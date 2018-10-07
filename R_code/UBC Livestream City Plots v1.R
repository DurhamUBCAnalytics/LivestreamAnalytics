rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")

sub_city <- function(x, min_users=10)
{
  
  for(i in c(1:length(x)))
  {
    x[[i]] <- subset(x[[i]], Users >= min_users & City!="")
  }
  x
}

load("Sunday Livestream City.RData")
city.data <- rbindlist(list.city)

