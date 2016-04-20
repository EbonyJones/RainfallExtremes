check.obs.prior <-function(data, block.ind, block.obs, rmax){
  
  if(missing(rmax)){rmax = 1}
  
  #get block maxima / indices
  possible.max.ind = which(block.obs$Period == 1)
  max.ind.block = possible.max.ind[order(block.obs$Rain[possible.max.ind], 
                                         decreasing = T, na.last = T)[1:rmax]]
  max.ind.data = block.ind[max.ind.block]
  
  #get observations prior to the maxima
  obs.prior = data[max.ind.data - 1, ]
  #have to do this using data as we might require an observation prior to the start of block.obs
  suspect.ind = which( (is.na(obs.prior$Rain) | obs.prior$Rain == 0) & obs.prior$Quality != "U")
  if(length(suspect.ind) > 0){
    #suspect untagged accumulations amongst the extremes
    
    #set suspected indices having untagged accumulations to NA
    block.obs$Rain[max.ind.block[suspect.ind]] = NA
    data$Rain[max.ind.data[suspect.ind]] = NA
    levels(data$Quality) = c("", "N", "Y", "U")
    data$Quality[max.ind.data[suspect.ind]] = "U" #ERROR HERE FACTOR ASSIGNMENT INCORRECT
    
    #check still got enough data that we could reasonably class the next observation as extreme
    numNA = numNAPerBlock(block.obs, rmax)
    if(numNA > allowable.numNA){
      if(length(suspect.ind) == rmax){
        max.date = rep(year*10000, rmax)
        max.rain = rep(NA, rmax)
      }else{
        #Too litte data to keep searching.
        #Violate our condition on number of missing maxima per block
        #Keep the maxima that was not suspected of being an untagged accumulation
        keep.ind = (1:rmax)[-suspect.ind]
        max.date = rep(year*10000, rmax)
        max.rain = rep(NA, rmax)
        max.rain[keep.ind] = block.obs$Date[max.ind.block[keep.ind]]
        max.rain[keep.ind] = block.obs$Rain[max.ind.block[keep.ind]]
      }
      return(cbind(max.date, max.rain))
    }else{
      #can search for a new maxima, after discarding the suspected old maxima
      output = check.obs.prior(data, block.ind, block.obs, rmax)
      return(output)
    }
    
  }else{
    #maxima observations are fine!
    max.date = block.obs$Date[max.ind.block]
    max.rain = block.obs$Rain[max.ind.block]
  }
  
  return(cbind(max.date, max.rain))
  
}

#30/03/2016
#Needed to add a new level for "U", but now the code appears to work. 
#Yet to write a test function, but initial testing appears ok.
