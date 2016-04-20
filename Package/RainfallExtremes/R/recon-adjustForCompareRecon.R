#Adjust the neighbour data to be the same size as the daily data
adjust.for.compare <- function(dailyData, data){

  #get daily start and end dates
  startDaily = dailyData$Year[1]*10000 + dailyData$Month[1]*100 + dailyData$Day[1]
  len.dates = dim(dailyData)[1]
  endDaily = dailyData$Year[len.dates]*10000 + dailyData$Month[len.dates]*100 + dailyData$Day[len.dates]

  #get nbr start and end dates
  startNbr = data$Year[1]*10000 + data$Month[1]*100 + data$Day[1]
  len.nbr = dim(data)[1]
  endNbr = data$Year[len.nbr]*10000 + data$Month[len.nbr]*100 + data$Day[len.nbr]

  #If records do not overlap
  if(endDaily < startNbr){
    return(NA)
  }else if(endNbr < startDaily){
    return(NA)
  }

  #want to adjust the neighbour data to be the same size as the daily data
  nbrAdjust = data
  if(startNbr < startDaily){
    start.ind = which((nbrAdjust$Year*10000 + nbrAdjust$Month*100 + nbrAdjust$Day) == startDaily)
    nbrAdjust = nbrAdjust[start.ind:len.nbr,]
    #     quantiles = quantiles[start.ind:len.nbr]
  }else{
    shift = which((dailyData$Year*10000 + dailyData$Month*100 + dailyData$Day) == startNbr)
    matrix.na = as.data.frame(matrix(NA, shift - 1, dim(nbrAdjust)[2]))
    names(matrix.na) = names(nbrAdjust)
    nbrAdjust = rbind(matrix.na, nbrAdjust)
    #     quantiles = c(rep(NA, shift - 1), quantiles)
  }
  if(endNbr > endDaily){
    end.ind = which((nbrAdjust$Year*10000 + nbrAdjust$Month*100 + nbrAdjust$Day) == endDaily)
    nbrAdjust = nbrAdjust[1:end.ind,]
    #     quantiles = quantiles[1:end.ind]
  }else{
    shift = dim(dailyData)[1] - dim(nbrAdjust)[1]
    matrix.na = as.data.frame(matrix(NA, shift, dim(nbrAdjust)[2]))
    names(matrix.na) = names(nbrAdjust)
    nbrAdjust = rbind(nbrAdjust, matrix.na)
  }
  return(nbrAdjust)
}
