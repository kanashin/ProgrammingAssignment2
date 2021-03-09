## a wrapper for matrix which allows to set and get the inverse of it
## and a getter/setter of the cached inverse

## make a matrix wrapper with inverse setter and getter

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


## Gets the cached inverse of matrix or generate it if not exists

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
