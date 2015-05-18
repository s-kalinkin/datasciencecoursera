corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  files = list.files(directory)
  count = 1

  for(file in files){
    values = read.csv(paste(directory,"/",file, sep = ""))
    v_clean = values[rowSums(is.na(values)) == 0, ]

    if(nrow(v_clean) > threshold){
      
      count = count + 1
    }
  }
  
  results = numeric(count - 1)
  count = 1

  for(file in files){
    values = read.csv(paste(directory,"/",file, sep = ""))
    v_clean = values[rowSums(is.na(values)) == 0, ]
    
    if(nrow(v_clean) > threshold){
      v_cor = cor(v_clean$sulfate, v_clean$nitrate)
      
      results[count] = v_cor
      
      count = count + 1
    }
  }

  results
}