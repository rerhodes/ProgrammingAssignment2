## This pair of functions can be used to cache the inverse of a 
## matrix

## This function creates a special matrix object that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmat <- function(mat) inv <<- mat
  getmat <- function() inv
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## This function computes the inverse of the special matrix 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated, then the cacheSolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getmat()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmat(inv)
  return(inv)
}
