#Function to get Eastern Australian coastline
getCoast <- function(){
  #Get coords of eastern australian state coastline
  library(oz)
  outline = ozRegion(sections = c(3:5)) #qld outline
  xRange = outline$rangex
  yRange = outline$rangey
  xPnts <- c(outline$lines[[1]]$x, outline$lines[[2]]$x, outline$lines[[3]]$x)
  yPnts <- c(outline$lines[[1]]$y, outline$lines[[2]]$y, outline$lines[[3]]$y)
  return(cbind(xPnts,yPnts))
}
