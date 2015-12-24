## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    
    ## assigning new value to the matrix
    x <<- y
    
    ## resetting inv_x
    inv_x <<- NULL
  }
  
  ## Function that gets matrix
  get <- function() x
  
  ## Sets the value of inv_x
  
  setinv <- function(inverse) inv_x <<- inverse
  
  ## Returns the value of inversed matrix 
  
  getinv <- function() inv_x
  
  ## Public functions and variables
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

## cacheSolve takes a matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv_x <- x$getinv()
  
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then the cachesolve should retrieve the inverse from the cache.
  
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  ## If it was not computed calculate the inverse 
  else {
    data <- x$get()
    inv_x <- solve(data, ...)
    
    ## set the inverse so that it does not need to be recalculated 
    
    x$setinv(inv_x)
    inv_x
  }
}







