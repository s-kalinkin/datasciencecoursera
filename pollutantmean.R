pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  file_names <- sprintf("%03d", id)
  files <- paste(directory, "/", file_names, ".csv", sep = "")
  p_sum <- 0
  p_count <- 0
  
  for(file in files)
  {
    file_values <- read.csv(file)[[pollutant]]
    p_sum <- p_sum + sum(file_values, na.rm = T)
    p_count <- p_count + length(file_values[!is.na(file_values)])
  }  
     
  p_sum / p_count
}