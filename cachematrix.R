## These functions will be used to calculate the inverse of a square matrix and cache the results. 

## The makeCacheMatrix function takes a matrix as an argument and returns 4 different functions
## for setting or retriveing the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set x to the matrix passed as an argument and set the inv variable to NULL
  set.matrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Retrieve the matrix passed as an argument
  get.matrix <- function() x
  
  ## Set the inv variable to the inverse of the matrix 
  set.inverse <- function(solve) inv <<- solve
  
  ## Retrieve the inverse of the matrix
  get.inverse <- function() inv
  
  # Return a list of functions
  list(set.matrix = set.matrix,
       get.matrix = get.matrix,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## The cacheSolve function calculates the inverse of the matrix created in the makeCacheMatrix functions.
## It checks to see if the inverse have previously been calculated and cached, and if so returns the 
## cached value instead of using the resources to calculate it again.
##
## It is assumed that the matrix is square. If not, it will result in an error when calling the solve() function.

cacheSolve <- function(x, ...) {
  
  ## Set the variable inverse to the value in makeCacheMatrix$get.inverse()
  inv <- x$get.inverse()
  
  ## If the value is not NULL, return the cached value
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  ## Else, calculate the inverse and cache it with makeCacheMatrix$set.inverse()
  data <- x$get.matrix()
  inv <- solve(data,)
  x$set.inverse(inv)
  inv
}
