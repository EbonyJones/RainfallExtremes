#Download Monthly AWAP data
downloadAWAPGrids <- function(months, years){
  #eg. months = seq(7,12) #JASOND
  #eg. years = seq(1900, 2014)
  for(y in years){
    for(m in months){
        numDaysPerMnth = ((y%%4 == 0)&(m==2))*29 + ((y%%4 != 0)&(m==2))*28 + (sum(m==c(9,4,6,11)) > 0)*30 + (sum(m==c(1,3,5,7,8,10,12)) > 0)*31
        days = 1:numDaysPerMnth
#         #For Monthly Grid Extraction
#         gridStartDate = y*10000 + m*100 + 01
#         gridEndDate = y*10000 + m*100 + numDaysPerMnth
#         #The below line needs to change if type change from totals or monthly
#         urlAddress = paste("http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/month/grid/0.05/history/nat/", gridStartDate, gridEndDate, ".grid.Z", sep ="") 
#         outputFile = paste("C:/Users/saundersk1/Documents/Data/AWAP/", gridStartDate, gridEndDate, ".grid.Z" , sep = "")
#         download.file(urlAddress, outputFile, mode = "wb")
        for(d in days){
          gridStartDate = y*10000 + m*100 + d
          gridEndDate = gridStartDate          
          urlAddress = paste("http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/", gridStartDate, gridEndDate, ".grid.Z", sep ="") 
          outputFile = paste("C:/Users/saundersk1/Documents/Data/AWAP/", gridStartDate, gridEndDate, ".grid.Z" , sep = "")
          download.file(urlAddress, outputFile, mode = "wb")
        }
    }
  }
}

#WARNING - When the grids are read they are referenced with respect to the lower left corner. 
#However for plotting applications or matrix indexing, we always start with the 1,1 
#   upper left corner.
#As such the grids need to be inverted in the y direction (rows) in order to be used with std R
#   functions.