#Adjust the record length of the data
adjust.record.length <- function(start.range, end.range, input.data){

  dates = as.POSIXlt(paste(input.data$Year,
                        formatC(input.data$Month, width = 2, flag = "0"),
                        formatC(input.data$Day, width = 2, flag = "0"),
                        sep = "-"))
  attributes(dates)$tzone <- "Australia/Sydney"

  start.date = dates[1]
  end.date = dates[length(dates)]

  #If date is not available for the date range
  if(end.date < start.range){
    return(NA)
  }else if(end.range < start.date){
    return(NA)
  }else{}

  #want to adjust start of the data to be the same as the start.ragne
  data.adj = input.data
  if(start.date <= start.range){
    start.ind = which(dates == start.range)
    #adjust the start
    len = length(dates)
    dates = dates[start.ind:len]
    data.adj = data.adj[start.ind:len,]
  }else{
    dates.add = as.POSIXlt(seq(as.Date(start.range), as.Date(start.date), "days"))
    attributes(dates.add)$tzone <- "Australia/Sydney"
    dates.add = dates.add[-length(dates.add)]
    df.add = as.data.frame(matrix(NA, length(dates.add), dim(data.adj)[2]))
    names(df.add) = names(data.adj)
    df.add$Year = dates.add$year + 1900
    df.add$Month = dates.add$mon + 1
    df.add$Day = dates.add$mday
    #adjust the start
    dates = c(dates.add, dates)
    data.adj = rbind(df.add, data.adj)
  }

  if(end.date >= end.range){
    end.ind = which(dates == end.range)
    #adjust the end
    dates = dates[1:end.ind]
    data.adj = data.adj[1:end.ind,]
  }else{
    dates.add = as.POSIXlt(seq(as.Date(end.date), as.Date(end.range), "days"))
    attributes(dates.add)$tzone = "Australia/Sydney"
    dates.add = dates.add[-1]
    df.add = as.data.frame(matrix(NA, length(dates.add), dim(data.adj)[2]))
    names(df.add) = names(data.adj)
    df.add$Year = dates.add$year + 1900
    df.add$Month = dates.add$mon + 1
    df.add$Day = dates.add$mday
    #adjust the end
    dates = c(dates, dates.add)
    data.adj = rbind(data.adj, df.add)
  }
  return(data.adj)
}
