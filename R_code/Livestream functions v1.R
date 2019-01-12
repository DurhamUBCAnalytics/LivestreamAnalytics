#### functions useful for date plotting

library(data.table)


#### gets which week of month x occurs in
which_week <- function(x)
{
  ceiling(as.numeric(format(x,"%d"))/7)
}
which_month <- function(x)
{
  as.numeric(format(x,"%m"))
}

get_top_cities <- function(data, n=10)
{
  topcity <- subset(data, City!="Total")
  topcity <- topcity %>% group_by_(.dots=c("City")) %>%
    summarise(Users=sum(Users)) %>%
    as.data.frame()
  
  topcity <- topcity[order(topcity$Users, decreasing=TRUE),]
  topcity <- topcity[c(1:n),]
  
  top.data <- subset(data, City%in%topcity$City)
  top.data
}


##### function for adding the percentge of users for a device for each city
##### written specifically for UBC Livestream Devide Plots v1.R (1/2/2019)
device_per <- function(data)
{
  data.city <- unique(data$City)
  for(i in data.city)
  {
    vv <- subset(data, City==i)
    
    for(d in c("mobile","desktop","tablet"))
    {
      data$Users_percent[data$City==i & data$Device==d] <- data$Users[data$City==i & data$Device==d]/sum(data$Users[data$City==i])
    }
  }
  
  data
}

service_per <- function(data)
{
  service.city <- unique(data$City)
  serv.order <- c(3,2,1)
  names(serv.order) <- c("11:15","9:30","7:15")
  
  for(i in service.city)
  {
    vv <- subset(data, City==i)
    
    for(d in c("11:15","9:30","7:15"))
    {
      data$Service_Per[data$City==i & data$Service==d] <- data$Viewers[data$City==i & data$Service==d]/sum(data$Viewers[data$City==i])
    }
  }
  
  data
}
