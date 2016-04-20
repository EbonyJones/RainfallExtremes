#Function to get nearest neighbours

#needs library(geosphere) #this needs to be loaded elsewhere
print("Warning - load library geosphere")
print("Reduced this function to one get.nbrs, will run twice once for ZSeries and then BSeries")
#
get.nbrs <- function(site.locn, metaData, num.km){
  km.dist = distHaversine(cbind(metaData$Long, metaData$Lat), site.locn)
  nearest.nbrs = which(km.dist/1000 < num.km & km.dist/1000 > 0)
  #order the neighbours by which is closest
  output.nbrs = nearest.nbrs[order(km.dist[nearest.nbrs])]
  return(output.nbrs)
}
