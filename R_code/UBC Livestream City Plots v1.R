rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

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
city.data$City[city.data$City==""] <- "Total"

city.data$Date <- as.Date(city.data$Date, "%Y%m%d")

topcity <- subset(city.data, Users >=10 & City!="Total")

p <- ggplot(data=topcity, aes(x=Date, y=Users,group=City)) +
  geom_line(aes(color=City)) +
  geom_point(aes(color=City)) +
  ggtitle("Users by City") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggsave(p, file="../Livestream Summary Plots/UBC Users by City Livestream Line Plot.png")


bp <- ggplot(data=topcity, aes(x=Date, y=Users, fill=City)) +
  geom_bar(stat="identity") +
  ggtitle("Users by City") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(bp, file="../Livestream Summary Plots/UBC Users by City Livestream Bar Chart.png")



ggsave(grid.arrange(p, bp, nrow=1), file="../Livestream Summary Plots/UBC Users by City Livestream Combined.png", 
       
       width=10, height = 5, units="in", dpi=600)

