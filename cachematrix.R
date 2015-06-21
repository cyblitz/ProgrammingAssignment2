## Put comments here that give an overall description of what your
## functions do

## Created for Coursera R Programming Assignment 2
## This script contains two functions:
##    1. makeCacheMatrix: Creates a list of functions
##    2. cacheSolve: Computes an inverse of a matrix if it is not already cached and returns the inverse if the cache already has it

## makeCacheMatrix: Returns a list of functions that will be used to calculate the inverse of a matrix if that is already not cached
## This function defines other functions get, set, getinv and setinv that can be used elsewhere to 
## advantageously to minimize the need to  by storing results and returning them later without
## the need to repetitively running long running operations/functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: Return a matrix that is the inverse of the matrix passed to it. 
## The inverse is calculated using the solve function, only if it was not calculated and cached previously using the makeCacheMatrix's setinv function
## If the inverese was cached previously, it uses the makeCacheMatrix's getinv function to pull the inverse from the cache instead
## of executing a potentially costly (from a time standpoint) solve function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}