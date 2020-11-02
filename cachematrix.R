## Below are two functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above

cacheSolve <- function(matrix, ...) {
        inverse <- matrix$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- matrix$get()
    inverse <- solve(data, ...)
    matrix$setinverse(inverse)
    inverse
}
