## Functions that cache the inverse of a matrix
## > m <- makeCacheMatrix(matrix(c(2, 1, 4, 6), 2, 2))
## > cacheSolve(m)

## Create a special matrix, which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {

    # initially nothing is cached
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since matrix is new, cache is NULL
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## Calculate the inverse of the special matrix created with the above
## function, reusing cached matrix if available

cacheSolve <- function(y, ...) {

  # get the cached value
  inverse <- y$getInverse()
  
  # if a cache exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise get the matrix, and store the inverse
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}