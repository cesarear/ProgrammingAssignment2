## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    mInverse <<- NULL
  }
  get <- function() x
  setInverse<- function(inverse) mInverse <<- inverse
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInverse <- mean(data, ...)
  x$setInverse(mInverse)
  mInverse
  
}
