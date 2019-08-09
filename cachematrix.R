## Put comments here that give an overall description of what your
## functions do

## This function provides a list of four functions for accessing the matrix of interest:
        # set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL            # Sets inv to NULL by default when constructed
        # Sets the new matrix to the provided one. Erases any stored inverse
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x  # Just returns x. No input variable needed
        setinverse <- function(inverse_m) inv <<- inverse_m
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## # This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
