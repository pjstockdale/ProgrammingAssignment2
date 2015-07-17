##############################################################################
## R Programming
## Programming Assignment 02
##
## PJ Stockdale
##
## This package demonstrates closure semantics to enable the results of 
## expensive intermediate operations to be cached ahead of their computed need
##

## This function establishes the environment that will contain the cached
## object along with the functions and objects necessary to manage that cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invx) inv <<- invx
    getinv <- function() inv
    list(set    = set, 
         get    = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function returns the inverse of the matrix x from the cache if the 
## object has previously been computed or by computing the object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
