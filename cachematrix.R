## Call makeCacheMatrix with a square matrix first
## then call cacheSolve and pass the makeCacheMatrix 
## to get the inverse


## Make a cacheable Matrix
## call this function First!
## Parameters: x: invertible or nonsingular square matrix
## Returns cachaable Matrix Object
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL ##Variable to store the inverse
  
  ##Constructor. Initialize CacheMatrix
  set <- function(newMatrix) {
    x <<- newMatrix
    mInverse <<- NULL
  }
  get <- function() x
  
  ##Set the inverse, to be called by cacheSolve
  setInverse<- function(inverse) mInverse <<- inverse
  
  ##Get the stored inverse, to be called by cacheSolve, may be null (first time)
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the Inverse of a cacheable Matrix object (makeCacheMatrix)
## If necessary calculate the inverse and set the inverse in makeCacheMatrix
## Parameters: x: makeCacheMatrix
## Return: inverse of matrix from  makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mInv <- x$getInverse()
  ##Is there a inverse already calculated?
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  ##No inverse available. Calculation required
  ##Get Matrix
  data <- x$get()
  ##Calculate Inverse
  mInverse <- solve(data, ...)
  ##Set inverse in makeCacheMatrix
  x$setInverse(mInverse)
  ##Return Inverse
  mInverse
  
}
