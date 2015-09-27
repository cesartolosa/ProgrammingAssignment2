## These functions can be used to create a special object that stores
## a numeric square matrix and cache its inverse.

## makeCacheMatrix creates a special "matrix"
## which is really a list containinga function to
## set the value of the matrix
## get the value of the matrix
## set the value of the solve
## get the value of the solve  

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() {
                x
        }
        setsolve <- function(solve) {
                s <<- solve
        }
        getsolve <- function() {
                s
        }
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve calculates the inverse of a special "matix" 
## created with makeCacheMatrix. First checks to see if the inverse has already
## been solved. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it solves the matrix and sets the
## value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()
        
        if(!is.null(s)) {
                message("getting cached data")
                
                return(s)
        }
        
        s <- solve(x$get(), ...)
        
        x$setsolve(s)
        
        s
}
