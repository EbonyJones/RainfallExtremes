#' Reads in the meta.data from a file
#' Makes sure the station numbers are width 6 and padded with zeroes
#' @param meta.data.file string that contains the location of the meta data file
#' @return Returns a data frame containing the meta data
#'
readMetaDataFile <- function(meta.data.file){
  meta.data = read.table(meta.data.file, head = T, sep = " ", stringsAsFactors = F)
  meta.data$SiteNum = formatC(meta.data$SiteNum, width = 6, flag = "0")
  return(meta.data)
}
