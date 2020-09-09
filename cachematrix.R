makeCacheMatrix <- function(x = matrix()) {
        p <- NULL
        set <- function(y) {
                x <<- y
                p <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) p <<- inverse
        getInverse <- function() p
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(p)) {
                message("Getting cached data - Matrix")
                return(p)
        }
        data <- x$get()
        p <- solve(data, ...)
        x$setInverse(p)
        p
}
