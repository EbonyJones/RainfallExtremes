#' Finds all .zip files in a directory. Iterates over these .zip files to get the station 
#' meta data. Then appends the site meta data to the output file if it exists, and creates the
#' output.file if it doesn't.
#' 
#' @param output.file file to write the meta data too
#' @return Returns nothing at the moment
#' 
#' 
writeMetaData <- function(output.file){
  #output = "metaData.txt"
  
  #get zip files
  zip.files = list.files()
  zip.i = grep("\\.zip$", zip.files)
  if(length(zip.i) == 0){
    #No zip files in directory
    return(NULL)
  }else{
    zip.files = zip.files[zip.i]
  }
  
  #get the meta data from the zip files  
  for(i in 1:length(zip.files)){
    zip.file = zip.files[i]
    print(zip.file)
    site.meta = getMetaDataFromZip(zip.file, output.file)
  }
  
  return(NULL)
}

