## This functions make use of the <<- assigment operator to calculate and store a matrix and its inverse 
## in the enviroment to allow the access to them by other functions.

## Defines an enviroment and returns a list of functions that are in that enviroment.
## It also defines objects 'x' and 'inverse' to store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL  # Sets the inverse to NULL as the first time it hasn´t been calculated
  
  # Sets the matrix to be cached and the defalt value NULL to the inverse
  setMatrix <- function(m) 
  {
    x <<- m
    inverse <<- NULL
  }
  
  # Returns the matrix
  getMatrix <- function() x
  
  # Sets the inverse to be cached 
  setInverse <- function(i) inverse <<- i
  
  # Returns the inverse of the matrix
  getInverse <- function() inverse
  
  # Returns the set of functions to use 
  list(setMatrix = setMatrix
       , getMatrix = getMatrix
       , setInverse = setInverse
       , getInverse = getInverse)
}


## Stores and returns the inverse matrix of 'x'.

## IF the inverse hasn't been calculated yet t calculates the inverse
## and stores it in the objects defined by makeCacheMatrix().

## Parameter 'f' contains the list of functions defined by makeCacheMatrix().
cacheSolve <- function(f, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # If the inverse has already being calculated and returns it
  inverse <- f$getInverse()
  if(!is.null(inverse))
  {
    return(inverse)
  }
  # If not it calculates the inverse and set it to the appropiate variable
  m <- f$getMatrix()
  # Calculates the inverse
  inverse <- solve(m) 
  f$setInverse(inverse) 
  # Returns the inverse
  inverse
}
