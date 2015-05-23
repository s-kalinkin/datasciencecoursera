## Creates container for inverse matrix caching
## 'm' is object of class "matrix"
makeCacheMatrix <- function(m = matrix()){
  inverse <- NULL
  
  # init with new matrix
  set <- function(m_new){
    m <<- m_new
    inverse <<- NULL
  }
  
  # get original matrix
  get <- function() m
  
  # save inverse matrix
  setInverse <- function(inverse_new) inverse <<- inverse_new
  
  # get cached inverse matrix
  getInverse <- function() inverse
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## Gets inverse matrix by calculating or retrieving results from cache
## 'cacheMatrix' is a list object returned from 'makeCacheMatrix' function
cacheSolve <- function(cacheMatrix, ...){
  inverse <- cacheMatrix$getInverse()
  
  if(!is.null(inverse)){
    # returning inverse matrix from cache
    inverse
  }
  else
  {
    # calculating inverse matrix and storing it in cache
    m <- cacheMatrix$get()
    inverse <- solve(m, ...)
    
    cacheMatrix$setInverse(inverse)
    
    inverse
  }
}