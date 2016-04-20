
#?normalise with respect to area each box ??
cut_long = seq(111.975, 885*0.05 + 111.975, 0.05)
cut_lat = seq(-44.525, 690*0.05 - 44.525, 0.05)
contour2D(z = as.matrix(map), x = cut_long, y = cut_lat, levels = c(0, 1, 5, 10, 25, 50, 100, 200, 300, 400, 600, 800))
contour2D(z = testGrid, x = cut_long, y = cut_lat, levels = c(0, 1, 5, 10, 25, 50, 100, 200, 300, 400, 600, 800))

#Get AWAP data
AWAP = url("http://www.bom.gov.au/jsp/awap/rain/archive.jsp?colour=colour&map=totals&year=2014&month=6&period=month&area=nat")
map = readGDAL("C:/Users/saundersk1/Documents/Papers/SOI_Asymmetry/Data/AWAP/1900070119000731.grid")
meta = GDALinfo("C:/Users/saundersk1/Documents/Papers/SOI_Asymmetry/Data/2014060120140630.grid")
filled.contour(seq(111.975, 885*0.05 + 111.975, 0.05),seq(-44.525, 690*0.05 - 44.525, 0.05), as.matrix(map))

#Read in AWAP data
totalYr = matrix(0, 886, 691) #Need to automate this step.
total <- rep(0, length(years))
print("Warning: WHAT TO DO WITH NO DATA!!! NEED TO THINK ABOUT THIS!")
for(y in years){
  for(m in months){
    gridStartDate = y*10000 + m*100 + 01
    gridEndDate = y*10000 + m*100 + ((y%%4 == 0)&(m==2))*29 + ((y%%4 != 0)&(m==2))*28 + (sum(m==c(9,4,6,11)) > 0)*30 + (sum(m==c(1,3,5,7,8,10,12)) > 0)*31
    inputFile = paste("C:/Users/saundersk1/Documents/Papers/SOI_Asymmetry/Data/AWAP/", gridStartDate, gridEndDate, ".grid" , sep = "")
    map = readGDAL(inputFile)
    totalYr = totalYr + as.matrix(map) #WHAT TO DO WITH NO DATA!!! NEED TO THINK ABOUT THIS
  }
  total[y - years[1] + 1] = sum(totalYr)
  gridStartDate = y*10000 + months[1]*100 + 01
  gridEndDate = y*10000 + months[length(months)]*100 + ((y%%4 == 0)&(m==2))*29 + ((y%%4 != 0)&(m==2))*28 + (sum(m==c(9,4,6,11)) > 0)*30 + (sum(m==c(1,3,5,7,8,10,12)) > 0)*31
  outputFile = paste("C:/Users/saundersk1/Documents/Papers/SOI_Asymmetry/Data/AWAP/Total/", gridStartDate, gridEndDate, ".txt" , sep = "")
  write.table(totalYr, outputFile, sep = " ")
}

filled.contour(seq(111.975, 885*0.05 + 111.975, 0.05),seq(-44.525, 690*0.05 - 44.525, 0.05), tot)
filled.contour(seq(111.975, 885*0.05 + 111.975, 0.05),seq(-44.525, 690*0.05 - 44.525, 0.05), as.matrix(map))

map = as.matrix(map)
x = seq(111.975, 885*0.05 + 111.975, 0.05)
y = seq(-44.525, 690*0.05 - 44.525, 0.05)  
for(i in 1:dim(map)[1]){
  for(j in 1:dim(map)[2]){
    points(x[j],y[i], col = "red", pch = 4)
  }
}
# urls = url("http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/month/grid/0.05/history/nat/2014060120140630.grid.Z", open = "r")

#Get coords of qld state outline
library(oz)
outline = ozRegion(section = c(3,11:13)) #qld outline
xRange = outline$rangex
yRange = outline$rangey
numLineSegs = length(outline$lines)
xPnts <- c()
yPnts <- c()
for(i in 1:numLineSegs){
  if(i == 3){#For qld - need to reverse the line segment for polygon class later 
    xPnts <- c(xPnts, outline$lines[[i]]$x[3:1])
    yPnts <- c(yPnts, outline$lines[[i]]$y[3:1])
  }else{
    xPnts <- c(xPnts, outline$lines[[i]]$x)
    yPnts <- c(yPnts, outline$lines[[i]]$y) 
  }
}

#Read in grid data and flip
grid = readGDAL("C:/Users/saundersk1/Documents/Papers/SOI_Asymmetry/Data/AWAP/1900070119000731.grid")
gridDim = grid@grid@cells.dim
gridSize = grid@grid@cellsize
gridBox = map@bbox
grid = as.matrix(grid) #grid data is recorded as a vector, create a matrix
grid = grid[, seq(gridDim[2],1,-1)] #invert matrix so lower left corner is indexed correctly

#Cut grid region
xPnts1 = round((xPnts - gridBox[1,1])/gridSize[1])
yPnts1 = round((yPnts - gridBox[2,1])/gridSize[2])
cutBox = matrix(c(gridBox[1,1] + min(xPnts1)*gridSize[1], gridBox[2,1] + min(yPnts1)*gridSize[2], gridBox[1,1] + max(xPnts1)*gridSize[1], 
                  gridBox[2,1] + max(yPnts1)*gridSize[1]), 2,2)
cutGrid = grid[min(xPnts1):max(xPnts1), min(yPnts1):max(yPnts1)]
cutDim = dim(cutGrid)

#Plot reduced size grid
contour2D(z = cutGrid, x = seq(cutBox[1,1], cutBox[1,2], 0.05), 
          y = seq(cutBox[2,1], cutBox[2,2], 0.05), 
          levels = c(0, 1, 5, 10, 25, 50, 100, 200, 300, 400, 600, 800))

#Delete unessary data
library(sp)
pnts = which(cutGrid > -10^6, arr.in = T)
spPnts = SpatialPoints(pnts)
xPnts2 = xPnts1 - min(xPnts1) + 1
yPnts2 = yPnts1 - min(yPnts1) + 1
spPoly = SpatialPolygons(list(Polygons(list(Polygon(cbind(xPnts2, yPnts2))), ID = "Qld")))
keepPnts = over(spPnts, spPoly)
plot(pnts[!is.na(keepPnts),])
