## Call makeCacheMatrix with a square matrix first
## then call cacheSolve and pass the makeCacheMatrix 
## to get the inverse


## Make a cacheable Matrix
## call this function First!
## Parameters: x: invertible or nonsingular square matrix
## Returns cachaable Matrix Object
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


## Finds the Inverse of a cacheable Matrix object (makeCacheMatrix)
## Parameters: x: makeCacheMatrix
## Return: inverse of matrix from  makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInverse <- solve(data, ...)
  x$setInverse(mInverse)
  mInverse
  
}
