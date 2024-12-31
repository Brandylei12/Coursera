## Put comments here that give an overall description of what your
## functions do

## This function is a constructor function that creates a special matrix object that can cache its inverse. It creates a list containing functions to set and get the value of the matrix and its inverse. The list acts as an environment and the functions within it have access to this environment, allowing them to share and preserve state.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache. This can save time when dealing with large matrices where the computation of the inverse can be time-consuming.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

