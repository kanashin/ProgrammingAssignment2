## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matrix$get()
  inverse <- solve(data, ...)
  matrix$setInverse(inverse)
  inverse
}
