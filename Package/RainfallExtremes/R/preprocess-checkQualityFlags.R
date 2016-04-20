checkQualityFlags <- function(data){
  #Quality types Y
  #Quality types N
  #Quality types "" (NA observation)

  #Set any missing flags to NA
  setToNA.missing = which(data$Quality == "" & !is.na(data$Rain) == 1)
  if(length(setToNA.missing) > 0){
    data$Rainfall.amount..millimetres.[setToNA.missing] = NA
    print(paste("Set", length(setToNA.missing), "obs with missing qualtiy flags to NA"))
  }

  #Note: full name specification is Rainfall.amount..millimetres,
  #but R is intelligent enough we can just use $Rain instead

  #Set any N quality flags to NA
  setToNA.N = which(data$Quality == "N" & !is.na(data$Rain) == 1)
  if(length(setToNA.N) > 0){
    data$Rainfall.amount..millimetres.[setToNA.N] = NA
    print(paste("Set", length(setToNA.N), "obs with N qualtiy flags to NA"))
  }else{
    print("No change to raw data due to quality flags")
  }

  return(data)
}
