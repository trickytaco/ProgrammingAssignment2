## The makeCacheMatrix function creates a special "matrix" that
## can store the matrix's inverse in a cache and retrieve the
## matrix's inverse upon request if it is in the cache.  Otherwise,
## the makeCacheMatrix function calculates the matrix's inverse.

## The cacheSolve function returns the matrix inverse of the
## input matrix, either by returning the previously cached matrix
## inverse or calculating the matrix inverse if no cache exists.



## The makeCacheMatrix function creates a special "matrix" that
## can store the matrix's inverse in a cache and retrieve the
## matrix's inverse upon request if it is in the cache.  Otherwise,
## the makeCacheMatrix function calculates the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function () x
    setInverse <- function(solve) matrixInverse <<- solve
    getInverse <- function() matrixInverse
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function returns the matrix inverse of the
## input matrix, either by returning the previously cached matrix
## inverse or calculating the matrix inverse if no cache exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(data)
    m
}