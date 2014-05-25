## Implementation of a cache to allow for the caching of
## time-consuming computations.
##

## This function returns a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m <<- y  
    inverse <<- NULL
  }
  get <- function() m
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the special matrix
## created by the function above ('makeCacheMatrix')
## It first checks if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse and sets the values of
## the inverse in the cache via 'setInverse'

cacheSolve <- function(m, ...) {

## returns a matrix that is the inverse of m
  inverse <- m$getInverse()
  
  ## if the inverse exists, then return it immediately
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## otherwise calculate the inverse and set it in the cache.
  data <- m$get()
  inverse <- solve(data, ...)
  m$setInverse(inverse)
  inverse
}
