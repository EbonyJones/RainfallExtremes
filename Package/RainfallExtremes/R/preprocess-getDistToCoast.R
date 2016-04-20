#Function to find minimum distance from a set of points in long lat coords
# to the coastline.
getDistToCoast <- function(long, lat, coast){
  
  library(sp)
  library(geosphere)
  
  min.dist <- matrix(0, ncol = 1, nrow = length(long))
  closest.points <- matrix(0, ncol = 2, nrow = length(long))
  for (i in 1:length(long)) {
    pnt = c(long[i], lat[i])
    closest.points[i,] <- coast[which.min(spDistsN1(coast, pnt, longlat = TRUE)), ] #spherical appoximation for distance
    min.dist[i] = distHaversine(pnt, closest.points[i,]) #ellipsoidal distance
  }
  
  return(min.dist)
}
