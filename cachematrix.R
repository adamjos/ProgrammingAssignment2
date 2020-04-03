
## The follwing functions are used to create and store a matrix, and compute and store its inverse. 
## The function "makeCacheMatrix" creates and stores a matrix and its inverse in an object-oriented fashion,
## by outputting a list with get and set functions for both the matrix and the inverse. The second function called 
## "cacheSolve" uses the makeCacheMatrix object returned from the first function and returns if available, a cached
## matrix inverse. If no matrix inverse is cached, cacheSolve computes the inverse by getting the original matrix,
## computing the inverse and calls the set function for the matrix inverse in the makeCacheMatrix object to set it.


## This function creates and stores a matrix and its inverse in an object-oriented fashion,
## by outputting a list with get and set functions for both the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved) inv <<- solved
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function uses the makeCacheMatrix object returned from the first function and returns if available, a cached
## matrix inverse. If no matrix inverse is cached, cacheSolve computes the inverse by getting the original matrix,
## computing the inverse and calls the set function for the matrix inverse in the makeCacheMatrix object to set it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
