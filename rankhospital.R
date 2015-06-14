rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  
  message(paste("Using colnum: ", colnum))
  
  state <- raw[raw$State == state, ]
  
  if(nrow(state) < 1)
  {    
    stop("invalid state")
  }
  
  ordered <- state[order(state[colnum]), ]
  ordered <- na.omit(ordered)
  
  if(num == "best")
  {
    ordered[1, "Hospital.Name"]
    
    ordered[, c(2,colnum)]
  }
  else if(num == "worst")
  {
    ordered[nrow(ordered), "Hospital.Name"]
  }
  else if(num <= nrow(ordered))
  {
    ordered[num, c(2,colnum)]
  }
  else
  {
    NA
  }
}