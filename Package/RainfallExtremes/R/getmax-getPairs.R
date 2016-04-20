#' Gets all distinct pairs of stations and the number of common observations between them
#' @param maximaObs - matrix of block maxima observations, 
#' rows correspond to observation block and columns the station
#' @return A dataframe with names of S1, S2, and percObsPerPair.
#' S1 and S2 are integers that index a row in the meta data
#' percObsPerPair is a double that is the percentage of common observations for that pair

getPairs <- function(maximaObs){
  numSites = dim(maximaObs)[2]
  pairs = as.data.frame(expand.grid(S1 = 1:numSites, S2 = 1:numSites))
  pairs = pairs[which(pairs$S1 < pairs$S2),]
  getObsPerPair <- function(pair, maximaObs){
    numObs = dim(maximaObs)[1]
    percObsPerPair = round(sum(!is.na(maximaObs[,pair[1]] + maximaObs[,pair[2]]))/numObs,2)
    return(percObsPerPair)
  }
  pairs$percObsPerPair = apply(X = pairs, MARGIN = 1, FUN = getObsPerPair, maximaObs)
  return(pairs)  
}