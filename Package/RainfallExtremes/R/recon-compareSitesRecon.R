print("Dependencies - adjust.for.compare(), congruence()")

#compare site observational records to check suitability
#pearson correlation & congruence statistics
compare.sites <- function(data, nbr.ind, metaData, dataDir,
                          start.range, end.range){

  if(length(data$Rain)==0){
    #     print("Error: Length of daily data is zero")
    return(NA)
  }

  if(missing(start.range)){
    start.range = as.POSIXlt(paste(data$Year[1],
                    formatC(data$Month[1], width = 2, flag = "0"),
                    formatC(data$Day[1], width = 2, flag = "0"),
                    sep = "-"))
    attributes(start.range)$tzone <- "Australia/Sydney"
  }
  if(missing(end.range)){
    end.ind = dim(data)[1]
    end.range = as.POSIXlt(paste(data$Year[end.ind],
                        formatC(data$Month[end.ind], width = 2, flag = "0"),
                        formatC(data$Day[end.ind], width = 2, flag = "0"),
                        sep = "-"))
    attributes(end.range)$tzone <- "Australia/Sydney"
  }

  #read in the adjacent site data
  nbr.site.num = formatC(metaData$SiteNum[nbr.ind], width = 6, flag = "0")
  zip.file = paste(dataDir, "IDCJAC0009_", nbr.site.num,
                   "_1800.zip", sep = "")
  nbrData = getDataFromZip(zip.file)

  #restrict the neighbour data to the same date range as the data
  nbrAdjust = adjust.record.length(start.range, end.range, nbrData)
  if(is.null(dim(nbrAdjust))){
    #     print("Neighbour site record is not compatible")
    return(NA)
  }

  if(length(data$Rain)!=length(nbrAdjust$Rain)){
    #    print("Error: Length of daily data and neighbour data did not match")
    return(NA)
  }

  if(sum(nbrAdjust$Rain, na.rm = T) == 0){
    #     print("No non-zero observations")
    return(NA)
  }

  #remove any missing observations from both sites
  #this is so the test statistic compares the same time periods and
  #we check we have sufficient data for a comparison so we don't through warnings/errors
  rm = union(which(is.na(nbrAdjust$Rain)), which(is.na(data$Rain)))
  if(length(rm) > 0){
    data = data[-rm,]
    nbrAdjust = nbrAdjust[-rm,]
  }

  if(length(data$Rain) < 100 | length(nbrAdjust$Rain) < 100){
    #     print("Neighbour has too few data points for comparison")
    return(NA)
  }

  #check neighbour pearson
  pearson = cor.test(data$Rain, nbrAdjust$Rain, method = "pearson")
  if(pearson$estimate < 0.6){
    #     print("Pearson fail")
    return(-1)
  }

  #check congruence statistics
  congr.stat = congruence(data$Rain, nbrAdjust$Rain)
  if(congr.stat < 0.8){
    #     print("Congruence fail")
    return(-2)
  }

  return(congr.stat)
}
