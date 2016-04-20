#' This function will extract csv data and meta data from Bureau of Meteorology zipped folder 
#' (standard naming convention).
#' 
#' @param zip.file A string input of a .zip file
#' @param output.file A string input of .txt file where the meta data can be written too
#' @return Returns NULL if an output.file is provided. In this instance the meta data stripped 
#' for the rainfall station is written directly to file. If no output.file is provided,
#' then the site meta data is returned as vector. 
#' The meta data returned contains the information;
#' c("Long", "Lat", "SiteNum", "Name", "State", "First", "Last", "PercComp", "Elev", "Dist")
#' The additions of State, Elev and Dist are for my own applications

getMetaDataFromZip <- function(zip.file, output.file){
  
  #set up file for writing
  if(!missing(output.file) & !file.exists(output.file)){
    meta.data.names = c("Long", "Lat", "SiteNum", "Name", "State",
                      "First", "Last", "PercComp", "Elev", "Dist")
    df = data.frame(matrix(meta.data.names,1, length(meta.data.names)))
    write.table(df, file = output.file, row.names = F, col.names = F)
  }
  
  #check zip.file exists
  if(!file.exists(zip.file)){
    return(NA)
  }

  #get site number
  siteNum = strsplit(zip.file, "_")[[1]][2]
  
  #create temporary directory for unzipping
  zipdir <- tempdir()
  dir.create(zipdir)

  #unzip the files into the zipdir
  unzip(zip.file, exdir=zipdir)
  unzip.files <- list.files(zipdir)
  
  #get files
  csv.file = paste("IDCJAC0009_", siteNum, "_1800_Data.csv", sep = "")
  txt.file = paste("IDCJAC0009_", siteNum, "_1800_Note.txt", sep = "")
  #check the files in the folder
  #might not be if there was a drop of internet connection error during the scrape
  if(all(unzip.files != csv.file) | all(unzip.files != txt.file)){
    #retry data scrape for that site
    bool = scrapeBoMData(siteNum)
    unzip(zip.file, exdir=zipdir)
    unzip.files <- list.files(zipdir)
    if(bool == 0){
      print(paste("Error: Failure to scrape data for", siteNum))
      return(NA)
    }else if(all(unzip.files != csv.file) | all(unzip.files != txt.file)){
      print(paste("Error: .csv or .txt file missing for", siteNum))
      return(NA)
    }else{
      #unzipped files correctly
    }  
  }
      
  #get csv data
  csv.data = read.csv(paste(zipdir, csv.file, sep = "/"))
  
  #scan text file for meta data details
  stationName = scan(paste(zipdir, txt.file, sep = "/"), skip = 10, what = "character", nlines = 1, sep = ":", strip.white = T, quote = "")[2]
  first = as.numeric(scan(paste(zipdir,txt.file, sep = "/"), skip = 11, what = "numeric", n = 2, sep = ":")[2])
  last = scan(paste(zipdir,txt.file, sep = "/"), skip = 12, what = "character", nlines = 1, sep = ":")[2]
  if(typeof(last) == "character"){
    spl.last = unlist(strsplit(last, split = " "))
    last = spl.last[length(spl.last)]
  }
  if(is.na(as.numeric(last))){last = csv.data$Year[length(csv.data$Year)]}
  
  lat = as.numeric(scan(paste(zipdir,txt.file, sep = "/"), skip = 13, what = "numeric", n = 2, sep = ":")[2])
  long = as.numeric(scan(paste(zipdir,txt.file, sep = "/"), skip = 14, what = "numeric", n = 2, sep = ":")[2])
  elev = as.numeric(scan(paste(zipdir,txt.file, sep = "/"), skip = 15, what = "numeric", n = 2, sep = ":")[2])
    #Not for elevation it is possible that the elevation is unknown and an NA is returned
  state = scan(paste(zipdir,txt.file, sep = "/"), skip = 16, what = "character", n = 2, sep = ":", strip.white = T)[2]

  #get distance to coast
  #Only written for east coast at the moment
  if(any(state == c("QLD", "NSW", "VIC"))){
    coast = getCoast()
    dist = round(getDistToCoast(long, lat, coast)/1000,2)
  }else{
    dist = NA
  }
  #get percentage of record with observations
  percComplete = round(sum(!is.na(csv.data$Rain))/length(csv.data$Rain) ,2)
  
  #remove temporary directory
  unlink(zipdir, recursive = T)

  #output meta data
  output = c(long, lat, siteNum, stationName, state, first, last, percComplete, elev, dist)
  
  #write to file option
  if(!missing(output.file)){
    df = as.data.frame(matrix(output, 1, length(output)))
    write.table(df, file = output.file, row.names = F, append = T, col.names = F)
    return(NULL) 
  }else{
    return(output)
  }
}
