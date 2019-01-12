rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)
library(scales)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")
source("../R_code/Livestream functions v1.R")


load("Sunday Livestream Device.RData")
device.data <- rbindlist(list.device)

device.data$Date <- as.Date(device.data$Date, "%Y%m%d")
device.data$Week <- which_week(device.data$Date)
device.data$Month <- which_month(device.data$Date)
device.data <- subset(device.data, Device.Category !="")

device.agg <- device.data %>%
  group_by_(.dots=c("Device.Category","Date")) %>%
  summarise(Users=sum(Users)) %>%
  as.data.frame()
names(device.agg)[1] <- "Device"

device.agg.wide <- device.agg[,c("Device","Date","Users")]
device.agg.wide <- spread(device.agg.wide, "Device","Users")

device.agg$Week <- which_week(device.agg$Date)
device.agg$Month <- which_month(device.agg$Date)

device.city.agg <- device.data %>%
  group_by_(.dots=c("Device.Category","City")) %>%
  summarise(Users=sum(Users)) %>%
  as.data.frame()
names(device.city.agg)[1] <- "Device"
device.city.agg <- subset(device.city.agg, City!="(not set)")
device.city.wide <- device.city.agg[,c("Device","City","Users")]
device.city.wide <- spread(device.city.wide, "Device","Users")
device.city.wide$Total <- rowSums(device.city.wide[,c(2:4)], na.rm=TRUE)
device.city.wide <- device.city.wide[order(device.city.wide$Total, decreasing = TRUE),]
device.city.wide[is.na(device.city.wide)] <- 0
device.city.wide$Per_Desk <- device.city.wide$desktop/device.city.wide$Total
device.city.wide$Per_Mobile <- device.city.wide$mobile/device.city.wide$Total
device.city.wide$Per_Tablet <- device.city.wide$tablet/device.city.wide$Total
write.table(device.city.wide, file="../Livestream Summary Tables/Device by City.csv",
            col.names=TRUE, row.names=FALSE, quote=FALSE, sep=",")

device.city.agg$Users_percent <- 0
device.city.agg <- device_per(device.city.agg)

vv <- subset(device.city.agg, Device=="mobile")
device.city.agg$City_Factor <- factor(device.city.agg$City, levels=vv$City[order(vv$Users, decreasing=TRUE)])
device.city.agg$City_Factor_Per <- factor(device.city.agg$City, levels=vv$City[order(vv$Users_percent, decreasing=TRUE)])


device_by_week <- device.data %>% 
  group_by_(.dots=c("Device.Category","Week")) %>% 
  summarise(Users=sum(Users)) %>% 
  as.data.frame()
names(device_by_week)[1] <- "Device"

device_by_week_avg <- device.data %>% 
  group_by_(.dots=c("Device.Category","Week","Month")) %>% 
  summarise(Users=sum(Users)) %>% 
  as.data.frame() %>%
  group_by_(.dots=c("Device.Category","Week")) %>% 
  summarise(Users=mean(Users)) %>% 
  as.data.frame()
names(device_by_week_avg)[1] <- "Device"


#### just for extracting the top cities for plotting devices by cities
load("Sunday Livestream City.RData")
city.data <- rbindlist(list.city)
topcity <- get_top_cities(data=city.data, n=10)
###########################

topcity.agg <- subset(device.city.agg, City%in%topcity$City)
tmpcity <- subset(city.data, Users >=5 & City!="Total")
minorcity.agg <- subset(device.city.agg, City%in%tmpcity$City & City!="Durham")

### Line plot for devices by sundays
p <- ggplot(data=device.agg, aes(x=Date, y=Users,group=Device)) +
  ylab("Viewers") + 
  geom_line(aes(color=Device), size=1) +
  geom_point(aes(color=Device), size=1.75) +
  ggtitle("Viewers by Device") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(device.agg$Date[device.agg$Week%in%c(1,3)]), date_labels = "%b %d")
  
ggsave(p, file="../Livestream Summary Plots/UBC Viewers by Device Livestream Line Plot.png")

### Bar plot for devices by sundays
bp <- ggplot(data=device.agg, aes(x=Date, y=Users, fill=Device)) +
  ylab("Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Viewers by Device") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(device.agg$Date[device.agg$Week%in%c(1,3)]), date_labels = "%b %d")

ggsave(bp, file="../Livestream Summary Plots/UBC Viewers by Device Livestream Bar Chart.png")

bp.week <- ggplot(data=device_by_week, aes(x=Week, y=Users, fill=Device)) +
  ylab("Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Viewers by Device by Week of Month") + xlab("Week of Month") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(bp.week, file="../Livestream Summary Plots/UBC Viewers by Device by Week Livestream Bar Chart.png")

bp.week_avg <- ggplot(data=device_by_week_avg, aes(x=Week, y=Users, fill=Device)) +
  ylab("Average Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Viewers by Device by Week of Month") + xlab("Week of Month") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(bp.week_avg, file="../Livestream Summary Plots/UBC Viewers by Device by Week Avg Livestream Bar Chart.png")

#### Aggregation of all Sundays by the top cities
city <- ggplot(data=topcity.agg, aes(x=City_Factor, y=Users, fill=Device)) +
  ylab("Viewers") + xlab("Cities ordered by # mobile users") +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Device All Sundays") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(city, file="../Livestream Summary Plots/UBC Viewers by Device Top Cities Livestream Bar Chart.png")

#### Aggregation of all Sundays by the top cities, by percent
city.per <- ggplot(data=topcity.agg, aes(x=City_Factor_Per, y=Users, fill=Device)) +
  ylab("Viewers") + xlab("Cities ordered by % mobile viewers") +
  geom_bar(position ="fill", stat="identity") +
  ggtitle("Percent Devices for Viewers: All Sundays") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_y_continuous(labels = scales::percent)
ggsave(city.per, file="../Livestream Summary Plots/UBC Viewers by Device Percent Top Cities Livestream.png")

#### now without durham but for all cities with > 5 users

### aggregation of "minor cities" aka cities minus durham which dominates the plot
minor.city <- ggplot(data=minorcity.agg, aes(x=City_Factor, y=Users, fill=Device)) +
  ylab("Viewers") + xlab("Cities ordered by # mobile viewers") +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Device All Sundays: Minor Cities") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
ggsave(minor.city, file="../Livestream Summary Plots/UBC Viewers by Device Minor Cities Livestream Bar Chart.png")

minor.city.per <- ggplot(data=minorcity.agg, aes(x=City_Factor_Per, y=Users, fill=Device)) +
  ylab("Viewers") + xlab("Cities ordered by % mobile viewers") +
  geom_bar(position="fill", stat="identity") +
  ggtitle("Percent Viewers by Device All Sundays: Minor Cities") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_y_continuous(labels = percent_format())
ggsave(minor.city.per, file="../Livestream Summary Plots/UBC Viewers by Device Percent Minor Cities Livestream.png")

### plots aggregated all into one
ggsave(grid.arrange(p, bp.week, city.per, minor.city.per, nrow=2, ncol=2), file="../Livestream Summary Plots/Combined UBC Viewers by Device Livestream.png", 
       width=10, height = 10, units="in", dpi=600)

