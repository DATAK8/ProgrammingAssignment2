
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  
  # Inizialise NULL variable
  i <- NULL                 
  
  #set the matrix and reinitialise the cached variable to Null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  
  #Sets the inverse and assigns the cache  
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
  
  # get the value from the inverse matrix
  
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

