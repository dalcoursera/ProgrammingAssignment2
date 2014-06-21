## This set of functions makes a special matrix that can cache its inverse 
##  so the cached result can be returned if not already calculated.

## makeCacheMatrix creates a special matrix, which is really a list that 
##  contains functions to set or get the value of the matrix and set or
##  get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the matrix inverse, but only if the inverse
##  has not already been calculated. If it already has, it skips
##  calculating it and returns the cached value.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
