rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/Processed Data")
source("../R_code/Livestream functions v1.R")

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
city.data$Week <- which_week(city.data$Date)
city.data$Month <- which_month(city.data$Date)

city_by_week <- city.data %>% 
  group_by_(.dots=c("City","Week")) %>% 
  summarise(Users=sum(Users)) %>% 
  as.data.frame()

city_by_month <- city.data %>% 
  group_by_(.dots=c("City","Month")) %>% 
  summarise(Users=sum(Users)) %>% 
  as.data.frame()


topcity <- get_top_cities(data=city.data, n=10)
topcity_by_week <- subset(city_by_week, City%in%topcity$City)
topcity_by_month <- subset(city_by_month, City%in%topcity$City)

city_cols <- c("red","darkgreen","lightblue","purple","blue","black","lightgreen","pink","brown","orange")
p <- ggplot(data=topcity, aes(x=Date, y=Users,group=City)) +
  ylab("Viewers") +
  geom_line(aes(color=City), size=1) +
  geom_point(aes(color=City), size=1.75) +
  ggtitle("Viewers by City") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_date(breaks=unique(topcity$Date[topcity$Week%in%c(1,3)]), date_labels = "%b %d") + 
  scale_color_manual(name="City", values=city_cols) 

ggsave(p, file="../Livestream Summary Plots/UBC Viewers by City Livestream Line Plot.png")


bp <- ggplot(data=topcity, aes(x=Date, y=Users, fill=City)) +
  ylab("Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Viewers by City") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_date(breaks=unique(topcity$Date[topcity$Week%in%c(1,3)]), date_labels = "%b %d") + 
  scale_fill_manual(name="City", values=city_cols) 
ggsave(bp, file="../Livestream Summary Plots/UBC Viewers by City Livestream Bar Chart.png")

bp.week <- ggplot(data=topcity_by_week, aes(x=Week, y=Users, fill=City)) +
  ylab("Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Viewers by City by Week") + xlab("Week of Month") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_manual(name="City", values=city_cols) 
ggsave(bp.week, file="../Livestream Summary Plots/UBC Viewers by City by Week Index Livestream Bar Chart.png")

bp.month <- ggplot(data=topcity_by_month, aes(x=Month, y=Users, fill=City)) +
  ylab("Viewers") + 
  geom_bar(stat="identity") +
  ggtitle("Viewers by City by Month") + xlab("Month of Year") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_manual(name="City", values=city_cols) 
ggsave(bp.month, file="../Livestream Summary Plots/UBC Viewers by City by Month Index Livestream Bar Chart.png")

ggsave(grid.arrange(p, bp.week, bp.month, layout_matrix=matrix(c(1,1,2,3), byrow = TRUE, ncol=2, nrow=2), nrow=2, ncol=2),
       file="../Livestream Summary Plots/Combined UBC Viewers by City Livestream.png", 
       width=10, height = 10, units="in", dpi=600)

