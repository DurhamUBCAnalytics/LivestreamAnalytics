rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")

load("Sunday Livestream Source.RData")
source.data <- rbindlist(list.source)

### some recoding
source.data$Source[source.data$Source=="(direct)"] <- "Direct"
source.data$Source[source.data$Source%in%c("com.google.android.googlequicksearchbox","myactivity.google.com","google")] <- "Google"
source.data$Source[source.data$Source=="ask"] <- "Ask.com"
source.data$Source[source.data$Source=="bing"] <- "Bing"
source.data$Source[source.data$Source%in%c("l.facebook.com","m.facebook.com","facebook.com")] <- "Facebook"
source.data$Source[source.data$Source=="myubc.org"] <- "UBC site"
source.data$Source[source.data$Source%in%c("searchencrypt.com","cvriskcalculator.com","dnserrorassist.att.net","dnsrsearch.com",
                                           "frontpage.pch.com","gospelmusicfever.blogspot.com","lookup.t-mobile.com")] <- "Other"
source.data$Source[source.data$Source%in%c("us.search.yahoo.com","yahoo")] <- "Yahoo"
source.data$Source[source.data$Source=="outlook.live.com"] <- "Outlook"
source.data$Source[source.data$Source=="r.search.aol.com"] <- "AOL"





source.data$Date <- as.Date(source.data$Date, "%Y%m%d")
source.data <- subset(source.data, Source !="")

source.nocity <- source.data[,c("Source","Users","Date")]

source.agg <- aggregate(source.nocity[,-c(1,3)], by=list(source.nocity$Source, source.nocity$Date), FUN=sum, na.rm=TRUE)
names(source.agg)[c(1,2)] <- c("Source","Date")

source.nodate <- source.data[,c("Source","Users","City")]
source.city.agg <- aggregate(source.nodate[,-c(1,3)], by=list(source.nodate$Source, source.nodate$City), FUN=sum, na.rm=TRUE)
names(source.city.agg)[c(1,2)] <- c("Source","City")
source.city.agg <- subset(source.city.agg, City!="(not set)")

#### just for extracting the top cities for plotting devices by cities
load("Sunday Livestream City.RData")
city.data <- rbindlist(list.city)
city.data$City[city.data$City==""] <- "Total"
topcity <- subset(city.data, Users >=10 & City!="Total")
###########################

topcity.agg <- subset(source.city.agg, City%in%topcity$City)

## do some subbing just fo asthetics

### Line plot for devices by sundays
p <- ggplot(data=source.agg, aes(x=Date, y=Users,group=Source)) +
  geom_line(aes(color=Source)) +
  geom_point(aes(color=Source)) +
  ggtitle("Users by Source") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggsave(p, file="../Livestream Summary Plots/UBC Users by Source Livestream Line Plot.png")

### Bar plot for Sources by sundays
bp <- ggplot(data=source.agg, aes(x=Date, y=Users, fill=Source)) +
  geom_bar(stat="identity") +
  ggtitle("Users by Source") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(bp, file="../Livestream Summary Plots/UBC Users by Source Livestream Bar Chart.png")

#### Aggregation of all Sundays by the top cities
city <- ggplot(data=topcity.agg, aes(x=City, y=Users, fill=Source)) +
  geom_bar(stat="identity") +
  ggtitle("Users by Source All Sundays") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(city, file="../Livestream Summary Plots/UBC Users by Source Top Cities Livestream Bar Chart.png")

#### now without durham but for all cities with > 5 users
topcity <- subset(city.data, Users >=5 & City!="Total")
minorcity.agg <- subset(source.city.agg, City%in%topcity$City & City!="Durham")

### aggregation of "minor cities" aka cities minus durham which dominates the plot
minor.city <- ggplot(data=minorcity.agg, aes(x=City, y=Users, fill=Source)) +
  geom_bar(stat="identity") +
  ggtitle("Users by Source All Sundays") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(minor.city, file="../Livestream Summary Plots/UBC Users by Source Minor Cities Livestream Bar Chart.png")

### plots aggregated all into one
ggsave(grid.arrange(p, bp, city, minor.city, nrow=2, ncol=2), file="../Livestream Summary Plots/UBC Users by Source Livestream Combined.png", 
       width=10, height = 10, units="in", dpi=600)

