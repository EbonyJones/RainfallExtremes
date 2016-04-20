#Download AWAP data
downloadAWAPGrids <- function(gridStartDate, gridEndDate, output.file){
  #Dates are of the form YYYYMMDD.
  #If the Date is invalid the grid will not download
  urlAddress = paste("http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/", gridStartDate, gridEndDate, ".grid.Z", sep ="") 
  outputFile = paste(gridStartDate, gridEndDate, ".grid.Z" , sep = "")
  download.file(urlAddress, outputFile, mode = "wb")
}
