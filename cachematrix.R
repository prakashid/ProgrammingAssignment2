## Caching the Inverse of a Matrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  pinv <- NULL
  set <- function(y) {
    x <<- y
   pinv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) pinv <<- inverse
  getInverse <- function() pinv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  pinv <- x$getInverse()
  if (!is.null(pinv)) {
    message("getting cached data")
    return(pinv)
  }
  mat <- x$get()
  pinv <- solve(mat, ...)
  x$setInverse(pinv)
  pinv
}