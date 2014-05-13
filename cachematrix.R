# The fucntions makeCacheMatrix and cacheSolve 
# cache the inverse of a matrix rather than compute it repeatedly.

# makeCacheMatrix creates a cache matrix that contains x(the matrix)
# and inv(the catched inverse). These function returns a list of
# four functions set,get,setinverse and gerinverse; with x and inv in
# the enviroment associated to these four functions.
# If the makeCacheMatrix is called with no arguments, makeCacheMatrix(), 
# x and inv are set both to NULL. Tf it is  called with an argumet y, 
# makeCacheMatrix(y), then y is assigned to x (x <<- y), y should be a 
# square matrix or an error will be produced later when cacheSolve 
# calls solve.  
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve checks if the inverse has not been previosly calculated
# if that is case calculates the inverse, catches this in the enviroment
# with x$setinverse(inv) and returns the calculated inverse. If the inverse 
# has been previously calculated sends the message "getting cached data"
# and returns the inverse
cacheSolve<- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}