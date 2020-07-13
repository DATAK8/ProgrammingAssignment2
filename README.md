# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {

  i <- NULL                 
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
   
  setInverse <- function(Inverse) {
    i <<- Inverse
  }
  
  #Gets the cached inverse
  getInverse <- function() i

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# CacheSolve computes the inverse of the special matrix before.
cacheSolve <- function(x, ...) {
  
   i <- x$getInverse()
   
  if(!is.null(i)) {
  
   message("getting cached data")
    
   return(i)
   
  }
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setInverse(i)
  
  i
}
