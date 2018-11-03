rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")
source("../R_code/Livestream functions v1.R")

load("Sunday Livestream Hourly.RData")
time.data <- rbindlist(list.time)
time.data$Date <- as.Date(time.data$Date, "%Y%m%d")

time.data$Hour <- as.integer(substring(as.character(time.data$Hour.of.Day),9,10))
time.data$Service <- NA
time.data$Service[time.data$Hour%in%c(7,8)] <- "7:15"
time.data$Service[time.data$Hour%in%c(9,10)] <- "9:30"
time.data$Service[time.data$Hour%in%c(11,12)] <- "11:15"


time.nocity <- time.data[,c("Hour","Users","Date")]

time.agg <- aggregate(time.nocity[,-c(1,3)], by=list(time.nocity$Hour, time.nocity$Date), FUN=sum, na.rm=TRUE)
names(time.agg)[c(1,2)] <- c("Hour","Date")

time.agg$Service <- NA
time.agg$Service[time.agg$Hour%in%c(7,8)] <- "7:15"
time.agg$Service[time.agg$Hour%in%c(9,10)] <- "9:30"
time.agg$Service[time.agg$Hour%in%c(11,12)] <- "11:15"

services <- subset(time.agg, !is.na(Service))
services <- services[,-1] ### remoes hour for aggregating by service

services.agg <- aggregate(services[,-c(1,3)], by=list(services$Service, services$Date), FUN=max, na.rm=TRUE)
names(services.agg) <- c("Service","Date","Viewers")

##### now aggregate by city for all Sundays
service.nodate <- time.data[,c("Service","Users","City")]
service.city.agg <- aggregate(service.nodate[,-c(1,3)], by=list(service.nodate$Service, service.nodate$City), FUN=max, na.rm=TRUE)
names(service.city.agg) <- c("Service","City","Viewers")
service.city.agg <- subset(service.city.agg, !City%in%c("(not set)"))


#### this just so things plot in reasonable order
services.agg$Order <- 1
services.agg$Order[services.agg$Service=="9:30"] <- 2
services.agg$Order[services.agg$Service=="11:15"] <- 3
services.agg$Service <- as.factor(services.agg$Service)
services.agg$Service  <- with(services.agg, reorder(Service, Order))

services.agg$Week <- which_week(services.agg$Date)
services.agg$Month <- which_month(services.agg$Date)

services_by_week <- services.agg %>% 
  group_by_(.dots=c("Service","Order","Week")) %>% 
  summarise(Viewers=sum(Viewers)) %>% 
  as.data.frame()
services_by_week$Service <- as.factor(services_by_week$Service)
services_by_week$Service  <- with(services_by_week, reorder(Service, Order))

service.city.agg$Order <- 1
service.city.agg$Order[service.city.agg$Service=="9:30"] <- 2
service.city.agg$Order[service.city.agg$Service=="11:15"] <- 3
service.city.agg$Service <- as.factor(service.city.agg$Service)
service.city.agg$Service  <- with(service.city.agg, reorder(Service, Order))
#### just for extracting the top cities for plotting devices by cities
load("Sunday Livestream City.RData")
city.data <- rbindlist(list.city)
topcity <- get_top_cities(data=city.data, n=10)
###########################

topcity.agg <- subset(service.city.agg, City%in%topcity$City)
minorcity <- subset(city.data, Users >=5 & !City%in%c("Total","Durham"))
minorcity.agg <- subset(service.city.agg, City%in%minorcity$City)

p <- ggplot(data=services.agg, aes(x=Date, y=Viewers,group=Service)) +
  geom_line(aes(color=Service), size=1) +
  geom_point(aes(color=Service), size=1.75) +
  ggtitle("Viewers by Service") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(services.agg$Date[services.agg$Week%in%c(1,3)]), date_labels = "%b %d")
ggsave(p, file="../Livestream Summary Plots/UBC Viewers by Service Livestream Line Plot.png")

bp <- ggplot(data=services.agg, aes(x=Date, y=Viewers, fill=Service)) +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Service") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(services.agg$Date[services.agg$Week%in%c(1,3)]), date_labels = "%b %d")
ggsave(bp, file="../Livestream Summary Plots/UBC Viewers by Service Livestream Bar Chart.png")

bp.week <- ggplot(data=services_by_week, aes(x=Week, y=Viewers, fill=Service)) +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Service by Week") + xlab("Week of Month")
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(bp.week, file="../Livestream Summary Plots/UBC Viewers by Service by Week Livestream Bar Chart.png")

city <- ggplot(data=topcity.agg, aes(x=City, y=Viewers, fill=Service)) +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Service: All Sundays") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(city, file="../Livestream Summary Plots/UBC Viewers by Service Top Cities Livestream Bar Chart.png")

minor.city <- ggplot(data=minorcity.agg, aes(x=City, y=Viewers, fill=Service)) +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Service: All Sundays") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(minor.city, file="../Livestream Summary Plots/UBC Viewers by Service Minor Cities Livestream Bar Chart.png")

### plots aggregated all into one
ggsave(grid.arrange(p, bp, city, bp.week, nrow=2, ncol=2), file="../Livestream Summary Plots/Combined UBC Viewers by Service Livestream.png", 
       width=10, height = 10, units="in", dpi=600)

