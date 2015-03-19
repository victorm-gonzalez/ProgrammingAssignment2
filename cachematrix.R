## A caching system for computing the inverse of matrices


## Creates a special vector which contains a Matrix and a set of functions 
## to get/set its value and inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        ## Reset the cached inverse if the matrix changes
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Computes the inverse of a Matrix using the vector created with 'makeCacheMatrix'
## First check if the result is already cached
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    ## Caches the result for subsequent calls
    x$setInverse(inv)
    inv
}

