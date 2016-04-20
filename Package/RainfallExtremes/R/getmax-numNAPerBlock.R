#get block maximum
#for tagged accumulations, check if the accum is
  #greater than the max
#if accum is less than the max then the missing obs are not max
#set observations in accum to -1
#don't count these observations in the no data observations

#if accum is greater than max ??? Haven't decided what to do.
#currently set to tagged accums that could be possible maxima to NA

#return number of missing observations

numNAPerBlock <- function(block.obs, rmax){
  if(missing(rmax)){rmax = 1}
  
  #check there are observations in the block
  if(sum(is.na(block.obs$Rain)) == length(block.obs$Rain)){
    numNA = length(block.obs$Rain)
    return(numNA)
  } 
  
  #get the block maximum
  possible.max.ind = which(block.obs$Period == 1)
  if(length(possible.max.ind) >= rmax){
    block.max = sort(block.obs$Rain[possible.max.ind],decreasing = T, na.last = T)[rmax]
  }else{
    #No possible maxima
    numNA = length(block.obs$Rain)
    return(numNA)
  }
  
  #don't worry about tagged accumulations that were not greater than maxima
  tagged.nonmax.ind = which(block.obs$Period > 1 & block.obs$Rain <= block.max)
  if(length(tagged.nonmax.ind) > 0){
    block.obs$Rain[tagged.nonmax.ind] = -1
    num.not.max = sum(block.obs$Period[tagged.nonmax.ind])
  }else{
    num.not.max = 0
  }
  
  #set tagged accumulations greater than the maxima to NA
  tagged.possmax.ind = which(block.obs$Period > 1 & block.obs$Rain > block.max)
  if(length(tagged.possmax.ind) > 0){
    block.obs$Rain[tagged.possmax.ind] = NA
  }
  
  #The remaing NA values could have been possible maxima
  numNA = sum(is.na(block.obs$Rain)) - num.not.max
  
#   #optional output for visual check
#   all.tagged.ind = which(block.obs$Period > 1)
#   num.tagged = sum(block.obs$Period[all.tagged.ind])
#   print(paste("Checked tagged accums:" , num.not.max, "of", num.tagged, 
#         "not possible maxima"))
  
  return(numNA)
  
}
