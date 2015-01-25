## These functions deals with the operation of invert a matrix which can
## be very time-consuming. The purpose is have a cache of the solution, so
## the computations must be done only once, and the remaining times when
## inverse matrix is needed, only have to access the cache.


## This function creates a object which represents a special matrix 
## object## that contains the matrix itself and a cache of its inverse
## matrix. This object is a list of functions to get/set the matrix 
## itself or its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## Sets the matrix and clears the cache
        set <- function(m) {
                x <<- m 
                inverse <<- NULL
        }
        
        ## Gets the matrix
        get <- function() x
        
        ## Sets the cache 
        setinverse <- function(inv) inverse <<- inv
        
        ## Gets the cache
        getinverse <- function() inverse
        
        ## Returns the special matrix object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gets the inverse matrix of a special matrix object, using 
## its cache or processing the answer if it is necessary. NOTE: The message
## is there only for debugging purposes.

cacheSolve <- function(x, ...) {
        
        ## Get the inverse object of x
        inverse <- x$getinverse()
        
        ## if inverse is not null, means that
        ## is already cached, so just return it
        if (!is.null(inverse)) {
                message("getting cache data")
                return(inverse)   
        }
        
        ## if inverse is not cached, it must be
        ## calculated, stored in x and returned
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse

}
