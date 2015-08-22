## These functions make the inverse of a matrix, caching 1 result for saving on time-consuming computations
## IMPORTANT: it assumes the original matrix is ALWAYS invertible
## we have 2 functions: 
##    makeCacheMatrix that caches the values. SET is mandatory before using cacheSolve
##    cacheSolve witch uses the cache and calculates the inverse only if necessary
##

## this function caches matrix and it's inverse for saving time-consuming computations
## USAGE: access via 4 functions returned in a list:  
##    get/set base matrix. 
##    getinv/setinv  for getting and setting inverse matrix, according to base matrix
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

#   internal functions. SET and GET base matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x

#   internal functions. SET and GET inverse matrix
    setinv <- function(inversa) inv <<- inversa
    getinv <- function() inv 

#   function return: list of all 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    }



## This function calculates inverse of a matrix
## caches LAST matrix and its inverse, for optimization
## PARAMETERS:
##    IN: a list returned by makeCacheMatrix function witch contains an invertible matrix
##    OUT: the inverse matrix  
cacheSolve <- function(x, ...) {

    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
    }
    
    data <- x$get()
    inversa <- solve(data, ...)
    x$setinv(inversa)
    inversa
}
