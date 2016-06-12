## Caching Inverse of Matrix - Coursera Programmig Assignment 2 R Programming
## These functions create a special matrix and compute its inverse and stores the same in cache.
## Since inverse of matrix calculation is a computationally heavy process, its benefitical to store the inverse,
## specially if the value of matrix is not going to change in the program run.

## the makeCacheMatrix function creates the special matrix whose inverse is to be calculated and stored in cache
## it returns a list of functions used to obtain the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(cacheinverse) inverse <<- cacheinverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the cacheSolve function returns the inverse of the special matrix.
## It first checks if the inverse is already caluclated, if it is then it returns the same else it computes 
## the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached inverse of x")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}