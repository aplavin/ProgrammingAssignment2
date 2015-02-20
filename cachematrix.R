# This module contains functions which allow caching the matrix inverse value.
# makeCacheMatrix creates the matrix which supports caching, and
# cacheSolve computes the inverse value of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Create a special "matrix", which is a list containing functions to
  # - set and get the value of the matrix
  # - set and get the value of its inverse
  inverse.cache <- NULL  # the cache variable - unaccessible from outside
  set <- function(y) {
    x <<- y
    inverse.cache <<- NULL  # invalidate the cache when updating the value
  }
  get <- function() x
  setInverse <- function(inverse) inverse.cache <<- inverse
  getInverse <- function() inverse.cache
  # return all the getters and setters, but not the private cache variable
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # Calculate the inverse of the "matrix" created with the above function.
  # If it has already been calculated, the value from the cache is returned.
  inverse <- x$getInverse() # try to get the cached value
  if(!is.null(inverse)) {
    # already computed, so emit a debug message and return the value
    message("getting cached data")
    return(inverse)
  }
  # no value in cache, so compute it and then return
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
