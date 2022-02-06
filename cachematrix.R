## Programming Assignment 2 from Coursera's R Programming course
## Submitted by: Christine Groesbeck
## Date: 2/5/22
## 
## These functions allow you to cache the inverse of a matrix to save
## time and computing power instead of needing to repeat the calculation
## to invert the matrix.
## This assignment assumes that any matrix supplied is always invertible.

## the makeCacheMatrix function creates a matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inv
  inv <- NULL
  ## function to set new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## function to retrieve matrix
  get <- function() x
  ## function to set the inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  ## function to get the inverse of the matrix
  getinv <- function() inv
  
  # returns a named list
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## the cacheSolve function computes the inverse of the matrix object created by
## the makeCacheMatrix function. If the matrix hasn't changed and the inverse
## has already been computed, it will retrieve its inverse from the cache.

cacheSolve <- function(x, ...) {
  # check for existing inverse value
  inv <- x$getinv()
  if(!is.null(inv)) {
    # if inverse value already exists, return existing value
    message("getting cached data")
    return(inv)
    }

  ## else, calculate the inverse of the matrix
  ## get the matrix and assign to matrix1
  matrix1 <- x$get()
  
  ## calc the inverse of the matrix
  inv <- solve(matrix1, ...)
  
  # set the calculated inverse
  x$setinv(inv)
  # return the inverse
  inv
    
}
