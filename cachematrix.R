## These functions computes the inverse of a matrix, but
## caches the calculated inverse instead of computing it 
## repeatedly. This saves computation time since the inverse
## is only computed once. If the matrix is reset, the
## inverse of the new matrix is computed and cached.

## The makeCacheMatrix function creates a special "matrix" 
## object than can cahce its inverse. The matrix can be reset 
## and then the inverse of the new matrix will be calculated 
## once and cahced by the second function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      invM <- NULL
      set <- function(y) {
        x <<- y
        invMatrix <<- NULL
      }
      get <- function() x
      setInvM <- function(invMatrix) invM <<- invMatrix
      getInvM <- function() invM
      list(set = set, get = get,
           setInvM = setInvM,
           getInvM = getInvM)
}


## The cacheSolve function takes a special "matrix" object as
## an argument and checks if that matrix allready has its
## inverse calculated. If the inverse is allready calculated
## the cached inverse is returned. If the inverse is not
## calculated this function calculates the inverse and
## cahce the result.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invM <- x$getInvM()
      if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
      }
      data <- x$get()
      invM <- solve(data, ...)
      x$setInvM(invM)
      invM
}
