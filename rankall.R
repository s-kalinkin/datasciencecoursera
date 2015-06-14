rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  raw <- read.csv("hospitaldata//outcome-of-care-measures.csv", colClasses = "character")
  raw[, 11] <- as.numeric(raw[, 11]) # as numeric
  raw[, 17] <- as.numeric(raw[, 17]) # as numeric
  raw[, 23] <- as.numeric(raw[, 23]) # as numeric
  
  map <- data.frame(list(input = c("heart attack", "heart failure", "pneumonia"), colnum = c(11, 17, 23)))
  
  colnum <- map[map$input == outcome,]$colnum
  
  if(length(colnum) != 1)
  {
    stop("invalid outcome")
  }
  
  statesList <- split(raw, raw$State)
  
  
numHospitals <- lapply(statesList, findhospital, colnum, num)
  
 data.frame(hospital = as.character(numHospitals), state = names(numHospitals))
}

findhospital <- function(list, colnum, num)
{
  list <- as.data.frame(list)
  #list <- list[!is.na(list["Hospital.Name"]),]
  ordered <- list[order(list[colnum], list["Hospital.Name"]), ] # sort ascending

  
  if(num == "best")
  {
    
    ordered[1, "Hospital.Name"]
  }
  else if(num == "worst")
  {
    ordered[nrow(ordered), "Hospital.Name"]
  }
  else if(num <= nrow(ordered))
  {
    ordered[num, "Hospital.Name"]
  }
  else
  {
    NA
  }
}