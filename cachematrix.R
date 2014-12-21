## These two functions aim to offer a time-efficient way of returning the inverse 
## of a matrix by using a cache. 


# loading MASS package required to use the ginv function
library(MASS)

## This function performs the computation required and caches the results.
makeCacheMatrix <- function(x = matrix()) {
    # initialize inv to NULL
    inv <- NULL

    # access original matrix
    get <- function() { x }

    # compute and store inverse
    setinv <- function(ginv)  { inv <<- ginv }
                                       
    # return cached inverse
    getinv <- function() { inv } 

    # available methods
    list(get = get,          
         setinv = setinv,
         getinv = getinv)

}


## This function returns the results of the inverse of matrix x. It calls 
## makeCacheMatrix to checks if a cached result exists. Compute it if not and
## cache it.
cacheSolve <- function(x, ...) {
    
    # access the inverse of x
    inv <- x$getinv()
    # if inv is not NULL (i.e. the inverse is cached), return it      
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if not cached, compute the inverse, cache it and return it
    data <- x$get()
    inv <- ginv(data, ...)
    x$setinv(inv)
    inv

}
