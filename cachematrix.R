## The functions makeCacheMatrix and cacheSolve are used to calculate the inverse of 
## a square matrix using solve() with the use of caching to improve calculation efficiency

## makeCacheMatrix creates a CacheMatrix object that stores and retrieves the inverse of a 
##square matrix by calling cacheSolve, and caches the result

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}


##cacheSolve calculates the inverse of a square matrix.  If the result has
##previously been cached, the cached result is returned to improve processing efficiency.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m 
  
}
