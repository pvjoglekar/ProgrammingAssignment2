## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y){    ## initialize p as NULL; will hold value of matrix inverse, 
    #define the set function to assign new 
    x <<- y   # matrix value in parent environment
    p <<- NULL # reset p to null if there is new matrix
  }
  get <- function()x  # defining get function to return the value of matrix argument 
  setInverse <- function(inverse) p <<- inverse # ## assigns value of p in parent environment
  getInverse <- function() p ## gets the value of p where called
  list(set = set, get = get, 
       setInverse = setInverse,     
       getInverse = getInverse)  ## we need this in order to refer 
  ## to the functions with the $ operator
}


## Write a short comment describing this function
## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  p <- x$getInverse() 
  if(!is.null(p)){
    message("getting cached data")
    return(p)
  }
  mat <- x$get()
  p <- solve(mat,...)
  x$setInverse(p)
  p
}
