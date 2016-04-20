print("dependendies of get.nbrs, check.nbrs, reconstruct")

#Function to write reconData
#implements reconstruction from Spain Paper
write.reconData <- function(siteInd, ASeries, ZSeries, BSeries, num.km){
  
  siteNum = formatC(ASeries$SiteNum[siteInd], width = 6, flag = 0)
  
  dailyData = read.data(siteNum)
  
  #Gap filling -----------------------------------------------------------
  
  #get nearby neigbours
#   site.nbrs = get.nbrs(ASeries$Long[siteInd], ASeries$Lat[siteInd], num.km)
  pnt = c(ASeries$Long[siteInd], ASeries$Lat[siteInd])
  z.nbrs = get.nbrs(pnt, ZSeries, num.km)
  b.nbrs = get.nbrs(pnt, BSeries, num.km)
#   valid.nbrs = check.nbrs(dailyData, ZSeries, BSeries, site.nbrs)
  if(length(z.nbrs) > 0){
    valid.z.nbrs = check.nbrs(dailyData, ZSeries, z.nbrs)
  }else{
    valid.z.nbrs = 0
  }
  if(length(b.nbrs) > 0){
    valid.b.nbrs = check.nbrs(dailyData, BSeries, b.nbrs)
  }else{
    valid.b.nrs = 0
  }
  
  #check there are valid neighbours - if not no reconstruction
  if(length(valid.z.nbrs)+length(valid.b.nbrs) == 0){
    print(paste("No appropriate neighbour sites for", siteNum))
    return(NULL)
  }
  
  #check for missing records
  na.len = length(which(is.na(dailyData$Rain)))
  if(na.len == 0){
    print(paste("No missing data for site", siteNum))
    return(NULL)
  }
  
  #get metaData of nearest neighbour and valid.nbr indices for filling
  if(valid.z.nbrs > 0 & valid.b.nbrs > 0){
    metaData = cbind(ZSeries[valid.z.nbrs,], BSeries[valid.b.nbrs,])
    valid.nbrs = 1:length(c(valid.z.nbrs, valid.b.nbrs))
  }else if(valid.z.nbrs == 0 & valid.b.nbrs > 0){
    metaData = BSeries[valid.b.nbrs,]
    valid.nbrs = 1:length(valid.b.nbrs)
  }else if(valid.z.nbrs > 0 & valid.b.nbrs == 0){
    metaData = ZSeries[valid.z.nbrs,]
    valid.nbrs = 1:length(valid.z.nbrs)
  }  

  #reconstruct the data using nearest neighbours
  reconData = reconstruct(dailyData, metaData, valid.nbrs)
  recon.len = sum(!is.na(reconData$Rain)) - sum(!is.na(dailyData$Rain))
  print(paste("Reconstructed:", recon.len, "of", na.len, "missing for site",
              siteNum))
  
  #write out reconstructed data to new file if any missing data was filled
  if(recon.len == 0){
    print(paste("No neighbour had suitable data for filling at site ", siteNum))
    return(NULL)
  }
  outputFile = paste("RQH/IDCJAC0009",siteNum, "1800", "Data", 
                     "R.csv", sep = "_")
  write.csv(reconData, outputFile, row.names = F)

#   OLD CODE
#   #return B Series used for filling
#   b.nbrs = valid.nbrs$b.nbrs
#   b.discard = NULL
#   if(length(b.nbrs) == 0){
#     return(NULL)
#   }else{
#     for(b.nbr in b.nbrs){
#       nbrSiteNum = formatC(BSeries$SiteNum[b.nbr], width = 6, flag = "0")
#       check = which(reconData$Quality == nbrSiteNum)
#       if(length(check) > 0){
#         b.discard = c(b.discard, b.nbr)
#       }
#     }
#     return(b.discard)
#   }
  
}
