
## This code reads in a matrix and returns its inverse.  To be more efficient, this code saves results from previous matrix input
## so if it has seen the matrix before, it will automatically retrieve the matrix's inverse.  Otherwise, the code will compute
## the new matrix's inverse and save it in the cache

## This function creates a list of four funcitons that 1) set the value of the matrix; 2) get the value of the matrix; 3) set the value 
## of the inverse; and 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
  
  # Get the inverse of the matrix 
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function reads in a special matrix list from the makeCacheMatrix function.  It first looks at
## the cache and sees whether the matrix has been saved.  If yes, then it will retrieve the inverse of 
## the matrix.  If no, then it will compute the inverse of the new matrix and put it in the cache for later use

cacheSolve <- function(x, ...) {
  
  # Get the value of the matrix inverse
  m <- x$getinverse()
  
  # If found, then return the inverse
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If not found, then compute the inverse, return the inverse, and save it to the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
