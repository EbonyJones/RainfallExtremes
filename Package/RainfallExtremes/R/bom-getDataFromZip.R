#' This function will extract csv data from Bureau of Meteorology zipped folder
#' (standard naming convention) containing rainfall data.
#'
#' @param site.num A string input for the station number. This is of width 6 padded with zeroes.
#' @return Returns a data frame containing rainfall observations. The form of this data frame
#' is the standard Bureau of Meteorology file type for rainfall with headers of:
#' 		Product code, Bureau of Meteorology station number, Year, Month, Day,
#' 		Daily rainfall (Millimetres), Period over which daily rainfall was measured (days),
#'		Quality of daily rainfall

getDataFromZip <- function(zip.file){

  #check zip.file exists
  if(!file.exists(zip.file)){
    print("Zip file was not found")
    return(NA)
  }

  #get site number
  site.num = strsplit(zip.file, "_")[[1]][2]

  # Create a name for the dir where we'll unzip
  zipdir <- tempdir()

  #files (standard bureau format)
  csv.file = paste("IDCJAC0009_", site.num, "_1800_Data.csv", sep = "")
  txt.file = paste("IDCJAC0009_", site.num, "_1800_Note.txt", sep = "")

  # Unzip the files into the zipdir
  unzip(zip.file, exdir=zipdir)
  unzip.files <- list.files(zipdir)

  # Find the csv file
  csv.file <- unzip.files[grep("\\.csv$", unzip.files)]
  # Read in csv file
  csv.data = read.csv(paste(zipdir, csv.file, sep = "/"))
  csv.data$Bureau.of.Meteorology.station.number =
      formatC(csv.data$Bureau.of.Meteorology.station.number,
              width = 6, flag = "0")
  csv.data = checkQualityFlags(csv.data)

  #remove the temporary directory
  unlink(zipdir, recursive = T)

  return(csv.data)

}
