## Two functions for caching the inverse of a matrix


## Initiates a matrix object that caches its inverse
makeCacheMatrix <- function( m = matrix() ) {
        
        ## Initialize inverse property
        i <- NULL
        
        ## Method for setting the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Method for getting matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Method for setting the inverse of matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Method for getting inverse of matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        
        ## Return method list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculate the inverse of thes matrix returned by makeCacheMatrix. If the inverse has already been calculated / matrix hasn't changed, then cachesolve retrieves inverse from cache.
cacheSolve <- function(x, ...) {
        
        ## Return  matrix that is the inverse of x
        m <- x$getInverse()
        
        ## return the inverse if its already been set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get matrix from object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set inverse to object
        x$setInverse(m)
        
        ## Return matrix
        m
}