## Put comments here that give an overall description of what your
## functions do

## This function creates the matrix object for this assignment
## Usage is B = matrix(rnorm(9),nrow=3,ncol=3)
## qq <- makeCacheMatrix()
## qq$set(matrix(rnorm(9),nrow=3,ncol=3))
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the Inverse of the matrix on the first run and stores it in a cache
## On subsequent runs it will return a cache of the inverse of the matrix
## Usage is cacheSolve(B)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
