## Put comments here that give an overall description of what your
## functions do
## This script contains two functions, makeCacheMatrix and cacheSolve for caching 
## the inverse of a square matrix. It helps to minimize the computational calculation. 

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix, represented as a list containing 
##four sub-functions:
##1. set(y), set the value of the matrix, input y is a square matrix
##2. get(), get the value of the matrix
##3. setinverse(inverse), set the value of the inverse
##4. getinverse(), get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve caches the inverse of the special matrix generated from makeCacheMatrix
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
