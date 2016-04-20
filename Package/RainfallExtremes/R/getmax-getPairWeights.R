#' Assign binary weights to pairs using a percCutOff
#' 
#' @param pairs A dataframe with names S1, S2, and percObsPerPair (see getPairs())
#' @param percCutOff The weight of a pair is set to 0 if the percentge of common observations 
#' is less than the percCutOff. If no pair satisfies the percentage cut off, all weights are set to 1.
#' @return Returns a binary vector of the same length as the number of pairs
#' 
getPairWeights <- function(pairs, percCutOff){
  weights = rep(1, length(pairs$percObsPerPair))
  zero.wghts = which(pairs$percObsPerPair < percCutOff)
  if(length(zero.wghts) > 0){
    weights[zero.wghts] = 0
  }
  return(weights)
}  

