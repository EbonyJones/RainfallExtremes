extremesUntaggedAccum <- function(date){
#input form is vector of dates yyyymmdd

#converts the date to the standard form of "yyyy-mm-dd"
  #then uses POSIXlt to get the day of the week for the given date
  #where sun = 0, mon = 1 etc to sat = 6
days <- as.POSIXlt(strptime(date, "%Y%m%d"))$wday #can use weekdays() to get the char str.
rm = which(is.na(days))
if(length(rm) > 0){
  days = days[-rm]
}
#hist(days)

#bootstrapping - resamples from the origingal data with replacement R times
R = 1000
resamps = lapply(1:R, function(i) sample(days,replace=T))
freq <- seq(0,6)*0
len = length(days)
freq = table(unlist(resamps))/R

#Binomial Distribution
p = 1/7
E = len*p #expectation
ciLow = 0.05 
ciUpp = 0.95

#if one day is below 5% CI and the next is above 95% CI
lowBnd = as.numeric((freq < qbinom(ciLow, len, p)))
order <- c(2,3,4,5,6,7,1)
uppBnd = as.numeric((freq > qbinom(ciUpp, len, p)))[order] #gets days after
check = which(lowBnd == 1 & uppBnd == 1) # gets the day of accumulation 0 - 6

# #follows the method by King 2013 to accept or reject sites
# #if one day is below 2 stdDev and the next is above 2 stdDev
# stdDev = sqrt(len*p*(1-p))
# lowBnd = freq < (len/7 - 2*stdDev)
# order <- c(2,3,4,5,6,7,1)
# uppBnd = (freq > (len/7 + 2*stdDev))[order]
# check = which(lowBnd == 1 & uppBnd == 1) - 1

# #for plotting
# title = "Graph" #"Site Wallaroo 022020"
# barplot(freq, names.arg = c("Sun" ,"Mon", "Tues", "Wed","Thurs","Fri","Sat"), 
#         ylim = c(0, max(freq)+0.05), main = title)
# abline(len/7, 0, lty = 3)
# stdDev = sqrt(len*p*(1-p))
# abline(len/7+2*stdDev, 0, lty = 2)
# abline(len/7-2*stdDev, 0,  lty = 2)

#returns 0 if there are no untagged accumulations suspected amongst the extremes (H0)
#returns 1 if there are suspected untagged accumulations amongst the extremes (H1)
bool = 0
if(length(check) > 0){
  bool = 1 
}

output = list(bool, check)
names(output) = c("bool", "days")

return(output)

}

#12/4/2016
#Had an issue with the uppBnd ordering, instead of reording the vector I ended up
# with which extracting names, and check returning two values

#30/03/2016
#changed function to run the days we expect of untagged accumulations