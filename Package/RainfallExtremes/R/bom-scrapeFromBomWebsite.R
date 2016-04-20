#' This function will go to the Bureau of Meteorology Weather Data Portal
#' and scrape the daily rainfall observational record for the input station number.
#' The output is saved in a zip folder under the default destination name that is used 
#' by the Bureau of Meteorology.
#' 
#' @param siteNum A string input for the station number. This is of width 6 padded with zeroes.
#' @return The sum of \code{0} if there is an error, \code{1} if scrape was successful
#' @examples
#' scrapeBoMData("001000") #succeed in downloading a zip folder 
#' scrapeBoMData("000000") #fail in downloading a zip folder

scrapeBoMData <- function(siteNum){
  #library(stringr)

  searchstr = "dailyZippedDataFile"
  bom.site = "http://www.bom.gov.au"
  url.part1 = "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num="
  destFile = paste("IDCJAC0009_", siteNum, "_1800.zip", sep = "")

  #scrape the bureau website for hyperlinks
  url = paste(url.part1, siteNum, sep = "")
  html <- try(paste(readLines(url), collapse="\n"))
  if(typeof(html)!= "character"){
    #Error site not found, no data scraped
    return(0)
  }

  #search for all hyperlinks
  #(this step could be simplified if I knew about regexp)
  hyper.links <- str_match_all(html, "<a href=\"(.*?)\"")[[1]][,2]
  if(length(hyper.links) == 0){
    #Error for no hyperlinks found
    return(0)
  }

  #search for download link
  matched = lapply(X = hyper.links, FUN = str_match, pattern = searchstr)
  index = which(!is.na(matched) == 1)
  if(length(index) == 0){
#     print(paste("No zip file link for:", siteNum))
    return(0)
  }else if(length(index) == 1){
    print(paste("Zip file link found for site number:", siteNum))
    download.link = hyper.links[index]
    #remove amp; from search regexp, and paste bom.site address to create full link
    download.link = paste(bom.site, download.link, sep = "")
    download.link = paste(strsplit(download.link, "amp;")[[1]], collapse = "")
  }else{
    print(paste("Error: More than one download link found", siteNum))
    return(0)
  }
# desired hyperlink is of the form
#"http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=040911&p_c=-334751896&p_nccObsCode=136&p_startYear=2016"

  #download the data (need to add bom site directory, and get rid of amp; from search)
  try(download.file(url = download.link, destFile,  mode = "wb"))

  return(1)
}
