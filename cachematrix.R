
## The functions defined in this source code module provide for the caching 
## of a matrix and its inverse in a manner transparent to the calling program.
##       The interface protocol for the calling program is as follows:
##          (1) Call the makeCacheMatrix function which returns  a "handle" to
##              to the matrix and its interse.
##          (2) Call the function: handle$set(x) where x is the matrix.
##          (3) Call the function: cacheSolve(handle) which returns the inverse.
##       Note: Step (2) can be repeated at any time to cache a different matrix.

## The function makeCacheMatrix returns a list of functions that are used by the 
## calling program to interface with the cache.

makeCacheMatrix <- function(MatrixData = matrix()) {
      CacheMatrixInverse <<- NULL
      set <- function(MatrixData) {
            CacheMatrix <<- MatrixData
            CasheMatrixInverse <<- NULL
      }
      get <- function() CacheMatrix 
      setinverse <- function(inverse) CacheMatrixInverse <<- inverse
      getinverse <- function() CacheMatrixInverse
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}	

## The cacheSolve function returns the inverse of the cached matrix. 
## Its input is the function list returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv=x$getinverse()
      if(!is.null(inv)){
##            message(" returning Cached Inverse")
            return(inv)
      }
      matrix <- x$get()
      inv = solve(matrix)
      x$setinverse(inv)
##      message(" returning Calculated Inverse")
      inv
}
