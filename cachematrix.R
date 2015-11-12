## The purpose of these functions are to create a matrix object that can cache
## the value of its inverse in order to avoid unnecessary recalculation.

## This makeCacheMatrix function creates a special matrix which is really a list 
## that allows the user to set the matrix, get the matrix, set the inverse of 
## the matrix, or get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function is used to calculate the matrix inversion and store it
## in the getsolve list item.  If the getsolve item is already valued, 
## the function will return the matrix inversion already stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m        
}
