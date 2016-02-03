##--------------------------------------------------------------------------##
## The following functions are used to cache the inverse of a square matrix ##
##--------------------------------------------------------------------------##




## makeCacheMatrix function creates a special "matrix" than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  S <- NULL
  
  ##Function initializing the matrix
  set <-function(y)
  {
    x <<- y
    S <<- NULL
  }
  
  ##Function returning the matrix
  get <-function() {x}
  
  ##Function inversing of the matrix
  setSolve <- function(M) {S <<- M}
  
  ##Getting/Displaying the inverse of the matrix
  getSolve <- function() {S}
  
  list(get = get, set = set, setSolve = setSolve, getSolve = getSolve)
  
}





## cacheSolve function computes the inverse of the special matrix created with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  ## Get the inverse of the matrix
  S <- x$getSolve()
  
  ## if the inverse matrix exists (ie. was ccomputed), then return the existing value
  if(!is.null(S)){
    message("Getting cached data")
    return(S)
  }
  ## if the inverse matrix does not exist, then compute the inverse of the matrix
  else{
    S<-solve(x$get(),...)
    x$setSolve(S) 
    return(S)
  }

  
}
