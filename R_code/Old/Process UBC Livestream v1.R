#### processing UBC data

rm(list=ls())

library(plotly)
library(processx)
library(magrittr)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/SundayCounts")

files <- list.files(path=".")

load.analytics <- function(x)
{
  tmp.list <- vector("list", length(x)) ## initialize list
  vv <- strsplit(x, " ") ## split on spaces
  for(l in c(1:length(x))) 
  {
    n <- length(vv[[l]])
    names(tmp.list)[[l]] <- gsub("\\.csv","",vv[[l]][n]) ### extract date based on naming convention and assign to list name
    tmp.list[[l]] <- read.table(x[l], header=TRUE, sep=",", stringsAsFactors=FALSE, skip=6 ) ### read in data and assign to list
  }
 
  tmp.list
}

sub_city <- function(x, min_users=10)
{
  
  for(i in c(1:length(x)))
  {
    x[[i]] <- subset(x[[i]], Users >= min_users & City!="")
  }
  x
}


list.device <- load.analytics(files[grep("Device",files)])
list.source <- load.analytics(files[grep("Source",files)])
list.time <- load.analytics(files[grep("Time",files)])
list.city <- load.analytics(files[grep("City",files)])
list.city_top <- sub_city(list.city)


city_pie <- function(data, do_plot=TRUE)
{
  n <- length(data)
  tmp.list <- vector("list", length(n)) ## initialize list
  for(i in c(1:n))
  {
    tmp <- data[[i]]
    tmp.list[[i]] <- plot_ly(data[[i]], labels = ~City, values = ~Users, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+value',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(Users, ' Viewers'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(title = paste('Livestream viewers on ', format( as.Date(names(data)[i],"%Y%m%d"), "%b %d %Y") ),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    if(do_plot)
    {
      orca(tmp.list[[i]], file= paste0('../Livestream Summary Plots/Livestream viewers on ', format( as.Date(names(data)[i],"%Y%m%d"), "%b %d %Y"), '.png'))
    }
    
  }
  
  tmp.list
}


pie.plots <- city_pie(list.city_top)


#### make pie chart of viewers by service

#### make map of viewersy by city

#### make bar chart of viewers by city by service
