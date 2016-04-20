print("do i need to also search over Quality constraint here too for fill data - yes")
print("want to be careful not to fill anything bigger than a month gap!")
print("i removed the startYear and endYear restrictions")
print("dependency - adjust.for.compare")
print("what to do with tagged accums? - fill or not to fill? ")

#fill missing data using nearest neighbour and return a reconstructed series
reconstruct <- function(dailyData, valid.nbrs, metaData){
  
  #create data set for reconstruction
  reconData = dailyData
  
  #check if dailyData needs reconstruction
  na.ind = which(is.na(dailyData$Rain))
  if(length(na.ind) == 0){
    print("No data to reconstruct")
    return(dailyData)
  }
  
  #shouldn't edit na.ind like this - why??? i can't remember
  i = 1
  num.nbrs = length(valid.nbrs)
  while(length(na.ind) > 0 & num.nbrs > 0){
    
    #read in the nbrData
    nbr.siteNum = formatC(metaData$SiteNum[valid.nbrs[i]], width = 6, flag = "0")
    nbrData = read.data(nbr.siteNum)
    
    #get the na.ind that can be filled
    nbrData = adjust.for.compare(dailyData, nbrData)
    nbrFill = nbrData[na.ind,]
    fill.ind = which(!is.na(nbrFill$Rain) & (nbrFill$Period == 1 | is.na(nbrFill$Period)))
      print("Warning: I think this is an issue because quality was set to NA :(" )
    
    #fill the data if appropriate 
    #if data is filled remove from na.ind
    if(length(fill.ind) > 0){
      reconData[na.ind[fill.ind],] = nbrFill[fill.ind,]
      reconData$Quality[na.ind[fill.ind]] = nbr.siteNum
      na.ind = na.ind[-fill.ind]
    }
    
    #get index for next valid neighbour
    i = i + 1
    
    #decrease number of available sites for filling by 1
    num.nbrs = num.nbrs - 1
    
  }
  
  return(reconData)
}
