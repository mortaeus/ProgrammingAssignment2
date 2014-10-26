## Functions that allow the creation of wrapper around a matrix object that 
## allows the caching of the wrapped matrix's inverse.

## Create the matrix wrapper which includes the cahced inverse.
## Returns a list of functions:
##  set - set the underlying matrix (invalidates cached inverse)
##  get - get the underlying matrix
##  setinverse - set the cahced inverse
##  getinverse - get the cached inverse (may be NULL)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  #invalidate cache
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Returns the inverse of the matrix represented by x which is assumed to
## have been created with makeCacheMatrix. Will used cached value if present,
## and will compute otherwise.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()

    if(!is.null(i)) {
        message("using cached inverse")
        return(i)
    }

    M <- x$get()
    inv <- solve(M, ...)
    x$setinverse(inv)
    inv
}
