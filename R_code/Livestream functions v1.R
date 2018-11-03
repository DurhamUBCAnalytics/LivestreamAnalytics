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