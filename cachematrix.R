## This pair of functions caches the inverse of of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL$
  }
  get <- function()x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will either return the cached inverse of the matrix or calculate the inverse if it is not cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- getinverse(x)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
