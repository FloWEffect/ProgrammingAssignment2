## Hi fellows,

## Similar to the supplied functions in the assignment, these functions cache output (in this case an inverse matrix) 
## with the primary goal to avoid recomputation every time you call for the same output. It's actually a good exercise 
## to figure out how R stores variables in the different environments and how they interact with each other.

## The first function 'makeCacheMatrix' is actually a list of functions that are later called from the 2nd function 
## environment 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function does the actual computation but only if the inverse, for the same matrix, hasn't been created before, 
## In which case it just retrieves the previously computeted inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mx <- x$get()
  inv <- solve(mx, ...)
  x$setInverse(inv)
  inv
}
