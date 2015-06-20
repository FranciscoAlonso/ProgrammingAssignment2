## Put comments here that give an overall description of what your
## functions do

## This functions make use of the <<- assigment operator to store a matrix a its inverse 
## in the 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL  #sets the inverse to NULL as the frst time t hasn´t been calculated
  
  #Sets the matrix to be cached and the defalt value NULL to the inverse
  setMatrix <- function(m) 
  {
    x <<- m
    inverse <<- NULL
  }
  
  #Returns the matrix
  getMatrix <- function() x
  
  #Sets the inverse to be cached 
  setInverse <- function(i) inverse <<- i
  
  #Returns the inverse of the matrx
  getInverse <- function() inverse
  
  
  list(setMatrix = setMatrix
       , getMatrix = getMatrix
       , setInverse = setInverse
       , getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(f=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- f$getInverse()
  if(!is.null(inverse))
  {
    return(inverse)
  }
  m <- f$getMatrix()
  inverse <- solve(m)
  f$setInverse(inverse)
  inverse
}
