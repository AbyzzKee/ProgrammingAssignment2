## Assignment: Caching the Inverse of a Matrix

## EXAMPLE
## m=makeCacheMatrix(matrix(1:4, 2,2))
## cacheSolve(m)

## makeCacheMatrix
## The following function accepts a input matrix x and 
## creates a list that containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the input matrix
## get the value of the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve
## The following function accepts a input list x and 
## calculates the inverse of the matrix in the list created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
