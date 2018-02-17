## Computing the inverse of a square matrix can be done with the solve function in R
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## set the value of the matrix
  
  set <- function(w){
    x <<- w
    inverse <<- NULL
  }
  
  ## get the value of the matrix
  
  get <- function() x
  
  ## set the value of the inverse
  
  setInverse <- function(resultMatrix) inverse <<- resultMatrix
  
  ## get the value of the inverse
  
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  ##The following function calculates the inverse of the special "matrix" created with the above function. 
  
  ##However, it first checks to see if the inverse has already been calculated. 
  ##If so, it gets the inverse from the cache and skips the computation. 
  ##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
  
  if(!is.null(inverse)){
    message("...getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}

