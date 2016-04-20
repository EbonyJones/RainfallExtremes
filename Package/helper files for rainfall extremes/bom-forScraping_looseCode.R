library(stringr)
#set working directory
setwd("Test")

#I don't actually know the station numbers for the bureau of meteorology site
#This makes it hard to query
#I do know these station number take the form of a 6 digit string starting with 0
#So I have just generated all such 6 digit numbers and attempted to see if there is a stataion
#If there isn't I am just redirected to an internal bureau website where there is an error

#generate all possible station numbers
siteNums = as.data.frame(matrix(NA, 10^5, 6))
names(siteNums) = c("Num1", "Num2", "Num3", "Num4", "Num5", "Num6")
siteNums$Num1 = 0
siteNums[,2:6] = expand.grid(V2 = seq(0,9), V3 = seq(0,9),
                V4 = seq(0,9), V5 = seq(0,9), V6 = seq(0,9))
getSiteNum = function(vec){str = paste(as.character(vec), collapse = "")}
siteNums = apply(siteNums, MARGIN = 1, FUN = getSiteNum)

#space check so I don't run out of room!
space.limit = 15*10^9
spaceCheck <- function(space.limit){
  folder.size = sum(file.info(list.files(".", all.files = TRUE, recursive = TRUE))$size)
  if(folder.size > space.limit){
    print(paste("Folder size limit reached at", siteNum))
    return(1)
  }
}

#scrape data and break if folder size exceeds the space limit
for(i in 1:siteNums){
  scrapeBomData(siteNum)
  if(spaceCheck(space.limit) == 1){
    break
  }
}

#From our scraped data now want to create a metadata file for reference
checkWrite = writeMetaData(output.file = "metaData.txt")

#Might want to read the meta data back in or search it
meta.data = readMetaDataFile(meta.data.file = "metaData.txt")
