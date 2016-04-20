#Function to get maxima (deafult is annual maxima)
getMaxima <- function(data, startYear, endYear, 
                      startMonth, endMonth, 
                      percNAPerBlock, minYears, 
                      rmax, checkUntagged){
  
  #Inputs:
  #Data - has columns of date and rain
  #Date - Standard Date of rainfall observations
  #Rain - Rainfall observations corresponding to dates
  #startYear/endYear - restrict the year range over which to collect maxima
  #startDate/endDate/startMonth/endMonth - if we need to restrict the date range we can set these
  #percNAPerBlock - percentage of missing observations allowed per year (global)
  #minYears - minimum number of years of observations
  #rmax - number of maxima per block
  minBlockSize = 365*0.75 #For running viney bates, don't want to run the code if the obs set is too small
  king.rmax = 4 #For detecting untagged accums amongst the extremes (number from paper)
  #Output - data frame containing the N maximuma per block
  
  #Convert to input data to standard date form, eg. from 19000201 to "1900-02-01"
  #compatible with high quality sites and std bom rain gauges
  Date = data$Year*10000 + data$Month*100 + data$Day
  data = cbind(Date, data)
  
  #Assigns defaults for unprescribed variables -------------------------
  startDate = 1
  if(missing(startYear )){ startYear = data$Year[1]}
  if(missing(endYear   )){ endYear = data$Year[length(data$Year)]}
  if(missing(startMonth)){ startMonth = 1 }
  if(missing(endMonth  )){ endMonth = 12 }
  endDate = 31
  if(endMonth!=12){ 
    if(sum(endMonth == c(9,4,6,11))){ 
      endDate = 30
    }else if(endMonth == 2){
      endDate = 28
    }else{
     endDate = 31
    }
  }
  if(missing(minYears)){minYears = 0}
  if(missing(percNAPerBlock)){percNAPerBlock = 0.1}
  if(missing(rmax)){rmax = 1}
  if(missing(boolUntagged)){checkUntagged = F}
  
  #Error checking 
  if(!(dim(data)[2] > 0)){
    print("Error: Observation input is empty"); return(NULL)
  }

  #Code ----------------------------------------------------
  
  #Get number of observations per block for later checking of
      # sufficient observations per block
  if(startMonth <= endMonth){
    timeA = strptime(19000000 + startMonth*100 + startDate, "%Y%m%d")
    timeB = strptime(19000000 + endMonth*100   + endDate, "%Y%m%d")
    numObsPerBlock = as.numeric(abs(difftime(timeA, timeB))) + 1
  }else{
    timeA = strptime(19000000 + startMonth*100 + startDate, "%Y%m%d")
    timeB = strptime(19010000 + endMonth*100   + endDate, "%Y%m%d")
    numObsPerBlock = as.numeric(abs(difftime(timeA, timeB))) + 1    
  }
  
  #Initialises an empty data frame from for block maxima spanning desired years of observation
  nYears = (endYear - startYear + 1)
  maxData = as.data.frame(matrix(NA, nYears*rmax, 2), row.names = FALSE)
  names(maxData) <- c("Date", "Rain")
  maxData$Date = (startYear:endYear)*10000
  
  #Calculates the allowable number of NA per block using Haylock and Nicholls 2000
  allowable.numNA = - numObsPerBlock*( (-(percNAPerBlock - 1))^(1/rmax) -1) 
  
  #Check: Is there rainfall data for this record period
  if(sum(data$Rain >= 0, na.rm = T) == 0){
    return(maxData)
  }
  
  #If we want to check for untagged accumulations
  if(checkUntagged == T){
    #Run code to check for untagged accumulations amongst extremes King 2013
    max.data = getMaxima(data, startYear, endYear, 
                       startMonth, endMonth, 
                       percNAPerBlock, minYears, 
                       rmax = king.rmax)
    king.untagged = extremesUntaggedAccum(max.data$Date)
    #Do we detect untagged accumulations?
    if(king.untagged$bool == 1){
      untagged.days = king.untagged$days 
    }
  }
  
  #Iterate over years to extract maxima
  for(year in startYear:endYear){
    output.ind = 1:rmax + (year - startYear)*rmax
    
    #get the indices of the block within the data
    if(startMonth <= endMonth){
      block.ind = which(data$Year == year & data$Month >= startMonth & data$Month <=endMonth)
    }else{
      block.ind = which((data$Month >= startMonth & data$Year == year) |
                          (data$Month <= endMonth   & data$Year == year + 1))
    }
      
    #Check: Are there observations in the desired block?
    if(length(block.ind) == 0){
      next
    }else{
      block.obs = data[block.ind,]
    }
    
    #Check: Are there too many missing observations in the block?
    #We don't count tagged accumulations that are less than the maxima as NA
    numNA = numNAPerBlock(block.obs, rmax)
    if(numNA > allowable.numNA){
        next
    }else{
        #Can now get maxima, but we need to ensure the observation Period is 1 
        #to avoid untagged observations.
        possible.max.ind = which(block.obs$Period == 1)
        max.ind.block = possible.max.ind[order(block.obs$Rain[possible.max.ind], 
                                               decreasing = T, na.last = T)[1:rmax]]
          #we check there exist 1:rmax extremes in numNAPerBlock
        max.ind.data = block.ind[max.ind.block]
    }
    
    #Check: Do we raise any flags for possible untagged accumulations
    #Note: Only want to run the check if sufficiently large block size #HARDCODE
    if(checkUntagged == T & length(block.ind) > minBlockSize){
      vb.untagged = vineybates04(block.obs)
      #Can address other block sizes later
    }
    if((king.untagged$bool == 1 | vb.untagged$bool == 1) & all(max.ind.data > 1)){
#       WORKING HERE - FINISH CODING!!!!
#       maxData$Date[output.ind] = year*10000
      max.output =  check.obs.prior(data, block.ind, block.obs, rmax)
      maxData[output.ind,] = max.output
      if(max.output[,2] != block.obs$Rain[max.ind.block]){
        print(paste("Replaced possible untagged accumulations for year:" ,year))
      }
     }else{
       maxData$Date[output.ind] = block.obs$Date[max.ind.block]
       maxData$Rain[output.ind] = block.obs$Rain[max.ind.block]
     }

#           #will only run for annual maxima over calendar year, otherwise untagged.accum = -1
#           if(untagged.accum == 1 & check.obs.prior == 0){
#             #check site 041270 and year 1988
#             maxData$Date[output.ind] = rep(year*10000, numMaxPerBlock)
#             #should also check nearest neighbour obs here
#             next
#           }
    

  }
  
  #Check: Are there insufficient years of block maxima observations?
          #Yes - then set all maxData$Rain to NA
  if(sum(!is.na(maxData$Rain)) < minYears){
      maxData$Rain = NA
  }

  return(maxData)

}
#30/03/2015
#Identified bug with check.obs.prior and fixed it

#29/03/2015
#Trying to take into account tagged accumulations when considering how many missing oberservations their are per
#block so I've written a new function numNAPerBlock to subtract tagged accums that were not maxima
#Also added in R block maximum capabiliyt
#Made the code cleaner by simplifying an if statement and taken our std.date

#15/03/2016
#Changed the default way the inputs were set, 
#from eg. startYear = NULL, to instead not specifying 
#checking if the input was missing

#also wrote documentation for function

#05/02/2016
#Noticed what appeared to be a bug in AM for non calendar years, where for site "040171"
# the year July 1968- June 1969 was missing both maximum rainfall observations and data
# recorded instead as NA NA
# after cleaning the code up to change variable names to something more intuitive
# the issue resolved itself.

#14/12/2015
#Found bugs galore today - super depressing
#Bug fixes included - zero obs/small obs as maxima, which was due to an indexing issue
#                     related to removing maxima after NA obs
#                     this code is not relevant for the BoM raw data not HQ
#                     can deal with this later
# Also very difficult to debug when the data has beene altered and removed years already searched
# Have removed  this feature of the code, don't think it will make a giant speed difference

#27/10/2015
#Rewrote code that combines annual, seasonal and block maxima to combine together

#Tested AM and it appears to work (still need to write a test case)
#Tested seasonal blocks, and block that run over a two years, like Summer (still need to write a test case)
#Haven't tested other functionality - including multiple extremes per block

#Yet to add functionality :
  # - to do mutliday maxima ie Rx5d
  # - to start and end mid month
  # - identify dry sites
  # - update untagged accumulations code
  # - remove years of all zero observations


#24/10/2015
#Fixed a bug that didn't check the minimum number of observations per yer weren't NA

#Added in a code to check years where untagged accumulations were suspected, if the 
#observed maxima occurred after a 0 observation

