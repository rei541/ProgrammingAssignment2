## Cache Inverse Matrix
## Coursera Programming Assignment 2
## Purpose: Obtains the inverse of a matrix.

## Notes: Because matrix inversion can be costly, these two functions
## are used to first check if there's an existing inverse matrix
## already stored in the cache for the inputted matrix. If there is,
## the cached inversed matrix is retrieved. If not, a new inverse
## matrix is calculated, returned, and cached.

## Includes 2 functions:
## 1) makeCacheMatrix
## 2) cacheSolve

## Assumptions: The matrix supplied is invertible.




## makeCacheMatrix: Creates a special "matrix" object that can
## cache the matrix inverse.
## The object returned has a set of functions (set, get, setim, getim) 
## that are returned in a list to the parent environment 
## and two data objects x and im
makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set <- function(y){
    x <<-y
    im <<-NULL
  }
  get <- function() x
  setim <- function(invmat) im <<-invmat
  getim <-function() im
  list(set=set, get=get,
       setim = setim,
       getim = getim)
}


## cacheSolve: Checks if the inverse of the special "matrix" object
## (created from the makeCacheMatrix function above) has already
## been calculated. If so, this inverse is retrieved from the cache.
## If not, a new inverse is calculated, stored in the cache, and 
## returned.
## Note: MUST use the special "matrix" object created from the 
## makeCacheMatrix function. Do not use any other object in this funcion.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <-x$getim()
  if(!is.null(im)) {
    message("Getting cached inverse matrix")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setim(im)
  im
}


