rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")



load("Sunday Livestream Device.RData")
device.data <- rbindlist(list.device)

device.data$Date <- as.Date(device.data$Date, "%Y%m%d")
device.data <- subset(device.data, Device.Category !="")

device.nocity <- device.data[,c("Device.Category","Users","Date")]

device.agg <- aggregate(device.nocity[,-c(1,3)], by=list(device.nocity$Device.Category, device.nocity$Date), FUN=sum, na.rm=TRUE)
names(device.agg)[c(1,2)] <- c("Device","Date")

device.nodate <- device.data[,c("Device.Category","Users","City")]
device.city.agg <- aggregate(device.nodate[,-c(1,3)], by=list(device.nodate$Device.Category, device.nodate$City), FUN=sum, na.rm=TRUE)
names(device.city.agg)[c(1,2)] <- c("Device","City")
device.city.agg <- subset(device.city.agg, City!="(not set)")

#### just for extracting the top cities for plotting devices by cities
load("Sunday Livestream City.RData")
city.data <- rbindlist(list.city)
city.data$City[city.data$City==""] <- "Total"
topcity <- subset(city.data, Users >=10 & City!="Total")
###########################

topcity.agg <- subset(device.city.agg, City%in%topcity$City)

p <- ggplot(data=device.agg, aes(x=Date, y=Users,group=Device)) +
  geom_line(aes(color=Device)) +
  geom_point(aes(color=Device)) +
  ggtitle("Users by Device") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggsave(p, file="../Livestream Summary Plots/UBC Users by Device Livestream Line Plot.png")

bp <- ggplot(data=device.agg, aes(x=Date, y=Users, fill=Device)) +
  geom_bar(stat="identity") +
  ggtitle("Users by Device") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(bp, file="../Livestream Summary Plots/UBC Users by Device Livestream Bar Chart.png")

city <- ggplot(data=topcity.agg, aes(x=City, y=Users, fill=Device)) +
  geom_bar(stat="identity") +
  ggtitle("Users by Device All Sundays") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(city, file="../Livestream Summary Plots/UBC Users by Device Top Cities Livestream Bar Chart.png")

#### now without durham but for all cities with > 5 users
topcity <- subset(city.data, Users >=5 & City!="Total")
minorcity.agg <- subset(device.city.agg, City%in%topcity$City & City!="Durham")

minor.city <- ggplot(data=minorcity.agg, aes(x=City, y=Users, fill=Device)) +
  geom_bar(stat="identity") +
  ggtitle("Users by Device All Sundays") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(minor.city, file="../Livestream Summary Plots/UBC Users by Device Minor Cities Livestream Bar Chart.png")

ggsave(grid.arrange(p, bp, city, minor.city, nrow=2, ncol=2), file="../Livestream Summary Plots/UBC Users by City Livestream Combined.png", 
       width=10, height = 10, units="in", dpi=600)

