#Viney and Bates 2004
#Objective test for the identification for untagged accumulations
#Currently using the fixed p.value suggested in the paper of 0.0008
#This is the level on average that we would reject a year incorrectly at a 5% level
    #During a 100 year period
#Alternately there is code to set the critical level according to different n and p1
   #values.

yearUntaggedAccum = function(block.obs, p.crit.vb){
  
  #### NEED TO EDIT NA SECTION current NA handling removes obs after NA values.
  #### However, these are observations that are tagged accumulations
  #### Code should treat these differently
  
  #check we have rainfall data
  if(length(block.obs$Rain) == 0 | sum(block.obs$Rain, na.rm = T) == 0){ 
    return(NA) 
  }  
  
  std.date = strptime(block.obs$Date, "%Y%m%d")
  weekdays = std.date$wday
  rm = which(is.na(weekdays))
  if(length(rm) > 0){
    table.obs = table(weekdays[-rm]) #0=Sun, 1=Mon etc.
  }else{
    table.obs = table(weekdays)
  }
  
  tot.wday.obs = sum(table.obs[2:5])
  if(is.na(tot.wday.obs) == 1){
    return(NA)
  }else if(tot.wday.obs == 0){ 
#     print(paste("Error: For year block beginning", years[1], "no Tues-Fri observations"))
    return(NA) 
  }else{}
  
  rainy.ind = which(block.obs$Rain > 0)
  table.rainy.obs = table(weekdays[rainy.ind])
  tot.rainy.wday.obs = sum(table.rainy.obs[2:5])
  if(is.na(tot.rainy.wday.obs) == 1){
    return(NA)
  }else if(tot.rainy.wday.obs == 0){ 
#     print(paste("Error: For year block beginning", years[1], "no Tues-Fri rainfall observations"))
    return(NA) 
  }else{}
  
  p1 = tot.rainy.wday.obs/tot.wday.obs
#   if(!(p1 > 0 & p1 < 1) | is.na(p1)){ return(NA) } #don't think I even need this 30/03/2016
#     #??? should this be & not or for p1 > 0 * p1 < 1 #changed it 29/02/2015
  block.len = length(block.obs$Rain)
  drywet.ind = which(  (block.obs$Rain[2:block.len] > 0 ) 
                     & (block.obs$Rain[1:(block.len-1)] ==0) )
  if(length(drywet.ind) ==0){ 
#     print(paste("Error: For year block beginning", years[1], "no dry then wet rainfall observations"))
    return(NA) 
  }
  table.drywet.obs = table(weekdays[drywet.ind])
  tot.drywet.obs = sum(table.drywet.obs[2:5])
  if(is.na(tot.drywet.obs) == 1){
    return(NA)
  }else if(tot.drywet.obs == 0){ 
    #     print(paste("Error: For year block beginning", years[1], "no Tues - Fri dry than wet rainfall observations"))
    return(NA) 
  }else{}  

  consec.ind = which(  (block.obs$Rain[2:block.len] > 0 ) 
                       & (block.obs$Rain[1:(block.len-1)]  > 0) )
  if(length(consec.ind) == 0){ 
#     print(paste("Error: For year block beginning", years[1], "no consecutive wet observations"))
    return(NA) 
  }
  table.consec.obs = table(weekdays[consec.ind])
  tot.consec.obs = sum(table.consec.obs[2:5])
  if(is.na(tot.consec.obs) == 1){
    return(NA)
  }else if(tot.consec.obs == 0){ 
#     print(paste("Error: For year block beginning", years[1], "no Tues-Fri consecutive wet rainfall observations"))
    return(NA) 
  }else{}
  
  p01 = tot.drywet.obs / tot.consec.obs
#   if(!(p01 > 0 & p01 < 1) | is.na(p01)){ return(NA) } #don't think I even need this 30/03/2016
    #again should this be an & # changed 29/02/2015

  ####GOT AN ISSUE HERE 
  ####(1) Possible that the tagged accumulation runs into previous year
  ####(2) If the period is incorrectly recorded! eg. the previous obs is not NA
  
  E.tagged.sundays = 0;
  tagged.ind = which(block.obs$Period > 1)
  if(length(tagged.ind) > 0){
    a = block.obs$Period[tagged.ind] #a for accumulation
    p.no.rain = 1 - (1 - p1)*(1-p01)^(a - 1)
    ps = p1/p.no.rain
    #Check which of these tagged accumulations include sundays.
    num.sundays = 0
    for(i in 1:length(tagged.ind)){
      check.ind = tagged.ind[i] + (-a[i] + 1):0
      #check we don't go into another block
      if(any(check.ind < 1)){
        check.ind = check.ind[check.ind >= 1]
        a[i] = length(check.ind)
      }
      #check that the period of accumulation is correct
      if(any(block.obs$Rainfall.amount..millimetres.[check.ind[-length(check.ind)]] 
             > 0, na.rm = T)){
        #Suspect error in record period!
        break
        print("Suspect error in tagged accumulations")
        print(block.obs[tagged.ind[i],])
        return(NA) 
      }
      num.sundays = num.sundays + sum(weekdays[check.ind] == 0)
    }  
    E.tagged.sundays = ceiling(sum(ps*num.sundays))
  }
  
  N_0 = sum(weekdays[rainy.ind] == 0) + E.tagged.sundays
  
  #Variable N ~ Bin(n, p1)
  n = sum(weekdays == 0) #n can be a maximum of 52
  if(n == 0){ 
#     print(paste("Error: For year block beginning", years[1], "no Sunday observations"))
    return(NA) 
  }
  
# #   #Should this level be fixed???? eg, crit.val = qbinom(0.0008, n, p)
#   n.sims.vb = 1000
#   n.samps.vb = n
#   alpha.vb = 0.05
#   if(missing(p.crit.vb)){
#     crit.levels = NULL
#     for(i in 1:n.sims.vb){
#       r.obs = rbinom(n.samps.vb, n, p1)
#       crit.levels = c(crit.levels, min(r.obs))
#     }
#     crit.val = sort(crit.levels)[alpha.vb*n.sims.vb]
#     p.value = pbinom(crit.val, n, p1)
#   }
#   
  p.value = pbinom(N_0, n, p1)

  # print(years)
#   print(paste("N_0 =", N_0, "p.value =", p.value))
  
  if(p.value <= p.crit.vb){
#     print("Refect H_0: Untagged accumulations present")
    return(1)
  }else{
#     print("Accept H_0: Untagged accumulations failed to be detected")
    return(0)
  } 

}#function end
