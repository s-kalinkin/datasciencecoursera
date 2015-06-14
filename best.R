best <- function(state, outcome) {
  ## Read outcome data
  raw <- read.csv("hospitaldata//outcome-of-care-measures.csv", colClasses = "character")
  raw[, 11] <- as.numeric(raw[, 11]) # as numeric
  raw[, 17] <- as.numeric(raw[, 17]) # as numeric
  raw[, 23] <- as.numeric(raw[, 23]) # as numeric
  
  map <- data.frame(list(input = c("heart attack", "heart failure", "pneumonia"), colnum = c(11, 17, 23)))
  colnum <- map[map$input == outcome,]$colnum
  
  message(paste("using colnum ", colnum))
  
  bystate <- raw[raw$State == state, ]
  
  if(nrow(bystate) < 1)
  {    
    stop("invalid state")
  }
  
  minvalue <- min(bystate[, colnum], na.rm = T)
  
  hospitals <- bystate[bystate[, colnum] == minvalue, ]
  
  names <- unique(hospitals[, "Hospital.Name"])
  
  names[!is.na(names)]
  
  ## Check that state and outcome are valid
  
  
  ## Return hospital name in that state with lowest 30-day death
  
  
  
  ## rate
}