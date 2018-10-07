#### processing UBC data

rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)

setwd("C:/Users/cavin/Desktop/UBC Data Analytics/UBC_Livestream_Analytics/SundayCounts")

files <- list.files(path=".")

load.analytics <- function(x)
{
  tmp.list <- vector("list", length(x)) ## initialize list
  vv <- strsplit(x, " ") ## split on spaces
  for(l in c(1:length(x))) 
  {
    n <- length(vv[[l]])
    l.name <- gsub("\\.csv","",vv[[l]][n]) ### extract date based on naming convention and assign to list name
    names(tmp.list)[[l]] <- l.name
    
    l.data <- read.table(x[l], header=TRUE, sep=",", stringsAsFactors=FALSE, skip=6 ) ### read in data 
    l.data$Date <- l.name ### add date identifier
    tmp.list[[l]] <- l.data ### assign to list
  }
 
  tmp.list
}


list.device <- load.analytics(files[grep("Device",files)])
save(list.device, file="../Processed Data/Sunday Livestream Device.RData")
write.table(rbindlist(list.device), file="../Processed Data/Sunday Livestream Device.csv", col.names=TRUE, row.names=FALSE, quote=FALSE, sep=",")

list.source <- load.analytics(files[grep("Source",files)])
save(list.source, file="../Processed Data/Sunday Livestream Source.RData")
write.table(rbindlist(list.source), file="../Processed Data/Sunday Livestream Source.csv", col.names=TRUE, row.names=FALSE, quote=FALSE, sep=",")

list.time <- load.analytics(files[grep("Time",files)])
save(list.time, file="../Processed Data/Sunday Livestream Hourly.RData")
write.table(rbindlist(list.time), file="../Processed Data/Sunday Livestream Hourly.csv", col.names=TRUE, row.names=FALSE, quote=FALSE, sep=",")


list.city <- load.analytics(files[grep("City",files)])
save(list.city, file="../Processed Data/Sunday Livestream City.RData")
write.table(rbindlist(list.city), file="../Processed Data/Sunday Livestream City.csv", col.names=TRUE, row.names=FALSE, quote=FALSE, sep=",")

