## Put comments here that give an overall description of what your
## functions do

## Create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( y ) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inv) {
    i <<- inv
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix above. 
## If the inverse has already been calculated, 
## then the it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
