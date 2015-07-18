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
    
    # initialize the cache (uses one variable for storage)
    x_inv <- NULL
    
    # define function to set the matrix x. each time the matrix 
    # is set, the cache is cleared
    set <- function(y) {
        x <<- y
        x_inv <- NULL
        #x_inv <<- matrix()
    }
    
    # define a function to return the matrix x
    get <- function() x

    # a function to programatically set the inverse of matrix x    
    setinv <- function(inv_of_x_in) x_inv <<- inv_of_x_in

    # a function to return the inverse of x from the cache
    getinv <- function() x_inv
    
    # return a list of references to these functions
    list(set    = set, 
         get    = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function returns the inverse of the matrix x from the cache if the 
## object has previously been computed or by computing the object

cacheSolve <- function(x, ...) {

    # check whether inverse is already in cache and return the cached
    # value if it is
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    
    # if not in the cache, compute it
    data <- x$get()
    x_inv <- solve(data, ...)
    
    # store the inverse in the cache
    x$setinv(x_inv)
    
    # and return the computed value
    x_inv
    
}
