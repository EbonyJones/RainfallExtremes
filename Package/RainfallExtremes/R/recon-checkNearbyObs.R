print("Dependecy - compare.sites, get.nbrs, check.nbrs")
library(geosphere)
#
# toowong.ind = 82 #toowong
# data = data.list[[toowong.ind]]
# excData = exc.list[[toowong.ind]]

check.nearby.obs <- function(excData, data, metaData, num.km,
                             min.nbrs, dataDir){

  site.num = formatC(data$Bureau.of.Meteorology.station.number[1],
                     width = 6, flag = "0")
  site.ind = which(metaData$SiteNum == site.num)
  site.locn = metaData[site.ind ,c(1,2)]
  nearest.nbrs = get.nbrs(site.locn, metaData, num.km)
  if(length(nearest.nbrs) == 0){return(NA)}

  valid.nbrs = check.nbrs(data, metaData, nearest.nbrs, dataDir)
  #need to change the read reference and pass the neighbour data directly
  #avoids the problem of data location and different file read types

  if(length(valid.nbrs) < min.nbrs){
    #can't do anything
    print("Insufficient neighbours for comparison")
    return(excData)
  }else{

    getObsAtNbrs <- function(exc.data, data, nbr.ind, dataDir){
      nbrData = matchObsFromNbr(data, metaData, nbr.ind, dataDir)
      exc.ind = as.numeric(row.names(exc.data))
      nbrObs = nbrData$Rainfall.amount..millimetres.[exc.ind]
      return(nbrObs)
    }

    nbrObs.list = lapply(valid.nbrs[1:min.nbrs], getObsAtNbrs, exc.data = excData,
                           data = data, dataDir = dataDir)
    nbrObs.mat = matrix(unlist(nbrObs.list), length(nbrObs.list[[1]]))

    checkNbrObs <- function(excData, nbrObs.mat){

        excData.check = excData
        #Option 1 (all other observations are non zero)
        row.sum = rowSums(nbrObs.mat, na.rm = T)
        excData.check$Rainfall.amount..millimetres.[excData.check$Rain > 0
                                                    & row.sum == 0] = NA
        #then the exceedance is questionable

        #Option2 (some other observations are non zero)
        perc.non.zero = rowSums(nbrObs.mat > 0, na.rm = T)/
          rowSums(!is.na(nbrObs.mat), na.rm = T)

        #Option 3 (all observations are non zero)
        excData.check$Rainfall.amount..millimetres.[excData.check$Rain == 0 &
                                                      perc.non.zero == 1] = NA
        #If all nonzero - then the observation should be non zero

        return(excData.check)

    }

    excData.check = checkNbrObs(excData, nbrObs.mat)
    incorrect.exc = sum(is.na(excData.check$Rainfall.amount..millimetres.))

    print(paste("Identified", incorrect.exc, "incorrect exceedances"))

    }

  #check for each observation the valid.nbrs
    #options to distance weight/congruence weight

  return(excData.check)

}

  #run separate function of obs check
  #where function has
  #quality control options (as in that paper ABZ method)
  #zero observations
  #medium observations
  #high observations
  #extreme observations
  #frequency

  #need to have cases, none, some and all near neighbours in agreement
