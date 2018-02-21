## The function MakeCacheMatrix is a tool that creates a list object that holds
## cached values for a matrix A and its inverse inv along with four functions   
## to set and get those values.

## inv and A are assigned into the parent environment so that they are cached
## into and available in the parent rather than deleted at the end of this 
## function's execution.
## inv is set to null each time A is set, so inv must be reset by cacheSolve().
## whenever A is set.

makeCacheMatrix <- function(A = matrix()) {
      inv <- NULL
      set <- function(y) {
            A <<- y
            inv <<- NULL
      }
      get <- function() A
      setinv <- function(mat) inv <<- mat
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The following function returns the inverse of the cached matrix A. 
## If the inverse already exists, this function returns the cached 
## inverse and skips the computation. Otherwise, it Solves 
## for the inverse of the global matrix, saves result into cache using setinv 
## then returns the inverse.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
      }

