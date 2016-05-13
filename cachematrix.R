## Two functions that speed up the process of using the inverse of 
## a matrix by creating and using a cached copy.

## makeCacheMatrix takes a matrix as an input and outputs a list of
## functions that will set and retrieve variables (the input matrix and its
## inverse) when called (from cacheSolve).
makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function (y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mx <<- inv
  getinv <- function() mx
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve takes the list output from makeCacheMatrix and
## uses it to get the cached inverse of the matrix contained within, or
## to calculate it if the it has not already been set.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mx <- x$getinv()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setinv(mx)
  mx
}