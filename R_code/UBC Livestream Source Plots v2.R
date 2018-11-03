rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")
source("../R_code/Livestream functions v1.R")

load("Sunday Livestream Source.RData")
source.data <- rbindlist(list.source)

source.data$Date <- as.Date(source.data$Date, "%Y%m%d")
source.data <- subset(source.data, Source !="")
source.data$OriSource <- source.data$Source
### some recoding
source.data$Source <- "Other"
source.data$Source[source.data$OriSource=="(direct)"] <- "Direct"
#source.data$Source[grepl("facebook",source.data$OriSource)] <- "Facebook"
source.data$Source[grepl("google|bing|yahoo|search|lookup",source.data$OriSource)] <- "Search Engine"



source.nocity <- source.data[,c("Source","Users","Date")]

source.agg <- aggregate(source.nocity[,-c(1,3)], by=list(source.nocity$Source, source.nocity$Date), FUN=sum, na.rm=TRUE)
names(source.agg)[c(1,2)] <- c("Source","Date")

source.agg$Week <- which_week(source.agg$Date)
source.agg$Month <- which_month(source.agg$Date)

source_by_week <- source.agg %>% 
  group_by_(.dots=c("Source","Week")) %>% 
  summarise(Users=sum(Users)) %>% 
  as.data.frame()

source.nodate <- source.data[,c("Source","Users","City")]
source.city.agg <- aggregate(source.nodate[,-c(1,3)], by=list(source.nodate$Source, source.nodate$City), FUN=sum, na.rm=TRUE)
names(source.city.agg)[c(1,2)] <- c("Source","City")
source.city.agg <- subset(source.city.agg, City!="(not set)")

#### just for extracting the top cities for plotting devices by cities
load("Sunday Livestream City.RData")
city.data <- rbindlist(list.city)
topcity <- get_top_cities(data=city.data, n=10)
###########################

topcity.agg <- subset(source.city.agg, City%in%topcity$City)

## do some subbing just fo asthetics

### Line plot for devices by sundays
p <- ggplot(data=source.agg, aes(x=Date, y=Users,group=Source)) +
  ylab("Viewers") +
  geom_line(aes(color=Source), size=1) +
  geom_point(aes(color=Source), size=1.75) +
  ggtitle("Viewers by Source") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(source.agg$Date[source.agg$Week%in%c(1,3)]), date_labels = "%b %d")
ggsave(p, file="../Livestream Summary Plots/UBC Viewers by Source Livestream Line Plot.png")

### Bar plot for Sources by sundays
bp <- ggplot(data=source.agg, aes(x=Date, y=Users, fill=Source)) +
  ylab("Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Users by Source") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(source.agg$Date[source.agg$Week%in%c(1,3)]), date_labels = "%b %d")
ggsave(bp, file="../Livestream Summary Plots/UBC Viewers by Source Livestream Bar Chart.png")

bp.week <- ggplot(data=source_by_week, aes(x=Week, y=Users, fill=Source)) +
  geom_bar(stat="identity") +
  ggtitle("Viewers by Source by Week of Month") + xlab("Week of Month") +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(bp.week, file="../Livestream Summary Plots/UBC Viewers by Source by Week Livestream Bar Chart.png")

#### Aggregation of all Sundays by the top cities
city <- ggplot(data=topcity.agg, aes(x=City, y=Users, fill=Source)) +
  ylab("Viewers") +
  geom_bar(stat="identity") +
  ggtitle("Users by Source All Sundays") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(city, file="../Livestream Summary Plots/UBC Viewers by Source Top Cities Livestream Bar Chart.png")

#### now without durham but for all cities with > 5 users
topcity <- subset(city.data, Users >=5 & City!="Total")
minorcity.agg <- subset(source.city.agg, City%in%topcity$City & City!="Durham")

### aggregation of "minor cities" aka cities minus durham which dominates the plot
minor.city <- ggplot(data=minorcity.agg, aes(x=City, y=Users, fill=Source)) +
  ylab("Viewers") +
  geom_bar(stat="identity") +
  ggtitle("Users by Source All Sundays: Minor Cities") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(minor.city, file="../Livestream Summary Plots/UBC Viewers by Source Minor Cities Livestream Bar Chart.png")

### plots aggregated all into one
ggsave(grid.arrange(p, bp, city, bp.week, nrow=2, ncol=2), file="../Livestream Summary Plots/Combined UBC Viewers by Source Livestream.png", 
       width=10, height = 10, units="in", dpi=600)

