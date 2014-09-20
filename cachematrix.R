# 'makeCacheMatrix' takes a matrix 'x' as an input and returns a list 'mat' of functions 'set', 'get', 'setinv', 'getinv'.
# 'set' assigns a matrix 'y' to 'x'.
# 'get' gets 'x'.
# 'setinv' sets 'inverse' as the inverse of 'x'.
# 'getinv' gets the inverse of 'x'.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   mat <- list(set = set, get = get, setinv = setinv, getinv = getinv)
   mat
}

# cacheSolve takes 'mat' as an input and returns the inverse of 'x' either by computing it for the first time or getting its cached value.

cacheSolve <- function(mat, ...) {
   inv <- mat$getinv()
   if(!is.null(inv)) {
      message("Cached data:")
      return(inv)
   }
   data <- mat$get()
   inv <- solve(data, ...)
   mat$setinv(inv)
   inv
}