## These functions (makeCashMatrix and cacheSolve) have been written for the 
## purposes of R Programming Assignment 2.

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.
        inverse <- x$getinverse()
                if(!is.null(inverse)) {
                        message("getting cached data")
                        return(inverse)
                }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        }
