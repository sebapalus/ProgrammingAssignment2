## Functions use lexical scoping to set internal values within special "cached" matrix object

## This function creates a special "matrix" object that can cache its inverse.
## Contains list o 4 functions, explained inside
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## set source matrix (clean inverse)
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get source matrix that is assigned to this special object
  get <- function() x
  
  ## set inversed matrix
  setInverse <- function(i) {
    inverse <<- i
  }
  
  ## get inversed matrix (or NULL if not exists yet) 
  getInverse <- function() inverse
  
  ## return list of available functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## If the inverse is already computed, then returns the value from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
    
  if (is.null(inverse)) {
    ## If cached value is NULL, then we need to compute and cache the result
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
  } else {
    ## If cached value is not NULL, then nothing needs to be computed
    ## Just show message that cached matrix is being used
    message('getting cached matrix')
  }
  
  ## Return the value (either from cache or computed)
  inverse
}
